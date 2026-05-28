################################################################################
# SCRIPT 03A: EXTRACT PATIENT DEMOGRAPHICS AND ENROLLMENT
# Stroke Rehabilitation Research - CDA Study
# ORD Waddell 202408036D
################################################################################
#
# Purpose:
#   Extract patient demographics, enrollment status, and socioeconomic variables
#   including age, sex, race, ethnicity, marital status, rurality, Medicare/
#   Medicaid enrollment, VA priority group, and area deprivation index.
#
# Author: Nick Cardamone
# Created: 2025-06-10
# Last Modified: 2025-12-11
# Version: 4.0
#
# Inputs:
#   - rhf_post_acute_final.parquet (from Script 2a)
#   - pdx_stroke_visit_summary.parquet (from Script 1)
#   - CDW OMOP: OMOPV5_PERSON, OMOP_CMS_PERSON_FF291
#   - CDW: SPatient, Outpat_Visit, ADR tables, PSSG data
#   - CMS: MBSF_1822 (Master Beneficiary Summary File)
#   - RUCA codes (external file)
#
# Outputs:
#   - demo_cohort.parquet: Comprehensive demographics dataset
#
# Variables Created:
#   Demographics: dob, dod, Age_ADMSNDT, Gender, Race, Ethnicity, MaritalStatus
#   Enrollment: dual, FFS_only, MA, PriorityGroupName
#   Geography: GISURH, loc_source, ADI_NATRANK
#
################################################################################

# ==============================================================================
# SETUP
# ==============================================================================

# Source configuration
source("00_config.R")

# Load required packages
load_packages(c("dplyr", "dbplyr", "DBI", "arrow", "data.table",
                "table1", "tidyr", "lubridate", "stringr", "readxl"))

cat("\n=============================================================================\n")
cat("SCRIPT 03A: EXTRACT PATIENT DEMOGRAPHICS AND ENROLLMENT\n")
cat("=============================================================================\n\n")

# Set working directory
setwd(project_base)


# ==============================================================================
# LOAD INPUT DATASETS
# ==============================================================================

cat("Loading input datasets...\n")

# Load full cohort
cohort_full <- load_parquet_safe("rhf_post_acute_final.parquet")
pdx_stroke_visit_summary <- load_parquet_safe("pdx_stroke_visit_summary.parquet")

# Extract key stroke admission information
pdx_stroke_visit_summary <- pdx_stroke_visit_summary %>% 
  transmute(
    scrssn = SCRSSN, 
    ADMSNDT = ACUTE_INPATIENT_VISIT_START, 
    discharge_SOURCE
  ) %>% 
  distinct()

# Create cohort with one row per hospitalization
cat("Creating cohort with lookback periods...\n")
cohort <- cohort_full %>%
  select(PatientICN, scrssn, PERSON_ID, ADMSNDT, DSCHRGDT, hee_from, hee_thru) %>%
  group_by(PatientICN, scrssn, PERSON_ID, ADMSNDT) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    hosp_start = if_else(hee_from < ADMSNDT, hee_from, ADMSNDT),
    year_prior_to_admission = hosp_start %m-% years(1),
    twoyears_prior_to_admission = hosp_start %m-% years(2)
  ) %>%
  left_join(pdx_stroke_visit_summary, by = c("scrssn", "ADMSNDT"))

log_count(cohort, "Cohort with lookback periods")

# ==============================================================================
# DATABASE CONNECTIONS
# ==============================================================================

cat("\nEstablishing database connections...\n")
con <- connect_db(database = db_project, server = db_server)
cdwwork <- connect_db(database = db_cdwwork, server = db_server)

# Load OMOP concept reference table
omop_concept <- tbl(con, in_schema(schema_src, 'OMOPV5_CONCEPT'))

# ==============================================================================
# CREATE CROSSWALK TABLES
# ==============================================================================

cat("Creating patient crosswalk tables...\n")

# PatientICN to PatientSID crosswalk
xw_icn_sid <- tbl(con, in_schema(schema_src, 'SPatient_Spatient')) %>%
  inner_join(cohort, by = "PatientICN", copy = TRUE) %>%
  select(PatientICN, ADMSNDT, DSCHRGDT, hosp_start, 
         year_prior_to_admission, PatientSID) %>%
  distinct()

# PatientICN to ADRPersonSID crosswalk (for enrollment data)
xw_icn_adr <- tbl(con, in_schema(schema_src, 'Veteran_ADRPerson')) %>%
  inner_join(
    tbl(con, in_schema(schema_src, 'Veteran_MVIPerson')), 
    by = "MVIPersonSID"
  ) %>%
  filter(ICNStatusCode %in% c('P', 'T')) %>%
  select(PatientICN = ADRPersonICN, ADRPersonSID) %>%
  inner_join(cohort, by = "PatientICN", copy = TRUE) %>%
  select(PatientICN, ADMSNDT, DSCHRGDT, hosp_start, 
         year_prior_to_admission, ADRPersonSID) %>%
  distinct()

# ==============================================================================
# MODULE 1: BASIC DEMOGRAPHICS (Age, Sex, Race, Ethnicity)
# ==============================================================================

cat("\n--- MODULE 1: Basic Demographics ---\n")

# Helper function to clean OMOP concept values
clean_concept <- function(value) {
  if_else(value == "No matching concept", NA_character_, value)
}

# Extract demographics from CDW OMOP
cat("Extracting CDW demographics...\n")
omop_person_cdw <- tbl(con, in_schema(schema_src, 'OMOPV5_PERSON')) %>% 
  inner_join(
    cohort %>% select(PERSON_ID) %>% distinct(), 
    by = "PERSON_ID", 
    copy = TRUE
  ) %>% 
  left_join(
    omop_concept %>% transmute(CONCEPT_ID, Race = CONCEPT_NAME), 
    by = c("RACE_CONCEPT_ID" = "CONCEPT_ID")
  ) %>% 
  left_join(
    omop_concept %>% transmute(CONCEPT_ID, Gender = CONCEPT_NAME), 
    by = c("GENDER_CONCEPT_ID" = "CONCEPT_ID")
  ) %>% 
  left_join(
    omop_concept %>% transmute(CONCEPT_ID, Ethnicity = CONCEPT_NAME), 
    by = c("ETHNICITY_CONCEPT_ID" = "CONCEPT_ID")
  ) %>% 
  transmute(
    PERSON_ID, 
    cdw_dob = if_else(
      as.Date(BIRTH_DATETIME) < "1910-01-01", 
      NA, 
      as.Date(BIRTH_DATETIME)
    ),
    cdw_Race = if_else(Race == "No matching concept", NA, Race), 
    cdw_Gender = if_else(Gender == "No matching concept", NA, Gender), 
    cdw_Ethnicity = if_else(Gender == "No matching concept", NA, Ethnicity), 
  ) %>% 
  distinct() %>% 
  collect()

log_count(omop_person_cdw, "CDW demographics")

# Extract demographics from CMS OMOP
cat("Extracting CMS demographics...\n")
omop_person_cms <- tbl(con, in_schema(schema_src, 'OMOP_CMS_PERSON_FF291')) %>% 
  inner_join(
    cohort %>% select(PERSON_ID) %>% distinct(), 
    by = "PERSON_ID", 
    copy = TRUE
  ) %>% 
  left_join(
    omop_concept %>% transmute(CONCEPT_ID, Race = CONCEPT_NAME), 
    by = c("RACE_CONCEPT_ID" = "CONCEPT_ID")
  ) %>% 
  left_join(
    omop_concept %>% transmute(CONCEPT_ID, Gender = CONCEPT_NAME), 
    by = c("GENDER_CONCEPT_ID" = "CONCEPT_ID")
  ) %>% 
  left_join(
    omop_concept %>% transmute(CONCEPT_ID, Ethnicity = CONCEPT_NAME), 
    by = c("ETHNICITY_CONCEPT_ID" = "CONCEPT_ID")
  ) %>% 
  transmute(
    PERSON_ID, 
    cms_dob = if_else(
      as.Date(BIRTH_DATETIME) < "1910-01-01", 
      NA, 
      as.Date(BIRTH_DATETIME)
    ),
    cms_Race = if_else(Race == "No matching concept", NA, Race), 
    cms_Gender = if_else(Gender == "No matching concept", NA, Gender), 
    cms_Ethnicity = if_else(Gender == "No matching concept", NA, Ethnicity), 
  ) %>% 
  distinct() %>% 
  collect()

log_count(omop_person_cms, "CMS demographics")

# Extract date of death
cat("Extracting date of death...\n")
omop_death_cdw <- tbl(con, in_schema(schema_src, 'OMOPV5_DEATH')) %>% 
  transmute(
    PERSON_ID, 
    cdw_dod = if_else(
      as.Date(DEATH_DATETIME) < "2019-01-01", 
      NA, 
      as.Date(DEATH_DATETIME)
    )
  )

omop_death_cms <- tbl(con, in_schema(schema_src, 'OMOP_CMS_DEATH_FF291')) %>% 
  transmute(
    PERSON_ID,             
    cms_dod = if_else(
      as.Date(DEATH_DATETIME) < "2019-01-01", 
      NA, 
      as.Date(DEATH_DATETIME)
    )
  )

# Combine death dates from both sources
omop_xw_death <- tbl(con, in_schema(schema_src, 'OMOPV5Map_SPatient_PERSON')) %>% 
  left_join(omop_death_cdw, by = "PERSON_ID") %>% 
  left_join(omop_death_cms, by = "PERSON_ID") %>% 
  transmute(PERSON_ID, dod = coalesce(cms_dod, cdw_dod)) %>% 
  distinct() %>% 
  collect()


# ==============================================================================
# MODULE 3: MARITAL STATUS
# ==============================================================================

cat("\n--- MODULE 3: Marital Status ---\n")

# Extract marital status from SPatient table
cat("Extracting marital status from SPatient...\n")
marital_spatient <- tbl(con, in_schema(schema_src, "SPatient_SPatient")) %>% 
  transmute(PatientSID, date = as.Date(PatientEnteredDateTime), MaritalStatus) %>% 
  inner_join(xw_icn_sid, by = "PatientSID", copy = TRUE) %>% 
  collect() %>%
  filter(MaritalStatus %!in% c("*Missing*", "UNKNOWN", "*Unknown at this time*", "SINGLE *DO NOT USE*")) %>%
  transmute(
    PatientICN,
    ADMSNDT,
    hosp_start,
    date,
    Married = if_else(MaritalStatus == "MARRIED", 1, 0),
    MaritalStatus = case_when(
      MaritalStatus == "WIDOW/WIDOWER" ~ "WIDOWED",
      MaritalStatus == "SINGLE" ~ "NEVER MARRIED",
      TRUE ~ MaritalStatus
    )
  ) %>%
  mutate(days_from = as.numeric(hosp_start - date))

log_count(marital_spatient, "Marital status from SPatient")

# Extract marital status from outpatient visits
cat("Extracting marital status from outpatient visits...\n")
marital_outpat_visit <- tbl(con, in_schema(schema_src, 'Outpat_Visit')) %>% 
  transmute(
    PatientSID, 
    date = as.Date(VisitDateTime),
    PatientMaritalStatus
  ) %>% 
  filter(PatientMaritalStatus %in% c('W', 'M', 'D', 'S', 'N')) %>%
  distinct() %>%
  inner_join(xw_icn_sid, by = "PatientSID", copy = TRUE) %>% 
  transmute(
    PatientICN,
    ADMSNDT,
    date,
    hosp_start,
    Married = if_else(PatientMaritalStatus == 'M', 1, 0),
    MaritalStatus = case_when(
      PatientMaritalStatus == "W" ~ "WIDOWED",
      PatientMaritalStatus == "M" ~ "MARRIED",
      PatientMaritalStatus == "N" ~ "NEVER MARRIED",
      PatientMaritalStatus == "S" ~ "SEPARATED",
      PatientMaritalStatus == "D" ~ "DIVORCED",
      TRUE ~ NA_character_
    )
  ) %>% 
  collect() %>%
  mutate(days_from = as.numeric(hosp_start - date))

log_count(marital_outpat_visit, "Marital status from outpatient visits")

# Combine marital status sources
marital_status <- rbind(marital_spatient, marital_outpat_visit) %>% 
  filter(MaritalStatus %in% c("MARRIED", "DIVORCED", "SEPARATED", 
                              "NEVER MARRIED", "WIDOWED")) %>%
  distinct()

# Get most recent marital status before hospitalization
cat("Selecting best marital status...\n")
marital_prev <- marital_status %>% 
  filter(days_from >= 0) %>%
  arrange(PatientICN, hosp_start, desc(date)) %>% 
  group_by(PatientICN, hosp_start) %>% 
  slice(1) %>% 
  ungroup() %>% 
  transmute(
    PatientICN, 
    ADMSNDT, 
    MaritalStatus_any_before = MaritalStatus, 
    # Only use if within 2 years
    Marital_Status_before = if_else(days_from <= 730, MaritalStatus, NA_character_)
  ) %>% 
  distinct()

# Get earliest marital status after hospitalization (backup)
marital_after <- marital_status %>% 
  filter(days_from < 0) %>%
  arrange(PatientICN, hosp_start, date) %>% 
  group_by(PatientICN, hosp_start) %>% 
  slice(1) %>% 
  ungroup() %>% 
  transmute(
    PatientICN, 
    ADMSNDT, 
    MaritalStatus_any_after = MaritalStatus, 
    # Only use if within 30 days
    Marital_Status_after = if_else(abs(days_from) <= 30, MaritalStatus, NA_character_)
  ) %>% 
  distinct()

# Create final marital status dataset
marital_status_final <- cohort_full %>% 
  select(PatientICN, ADMSNDT) %>%
  left_join(marital_prev, by = c("PatientICN", "ADMSNDT")) %>%
  left_join(marital_after, by = c("PatientICN", "ADMSNDT")) %>% 
  transmute(
    PatientICN,
    ADMSNDT,
    MaritalStatus = coalesce(Marital_Status_before, Marital_Status_after, 
                             MaritalStatus_any_before, MaritalStatus_any_after, 
                             "UNKNOWN"),
    Married = if_else(MaritalStatus == "MARRIED", 1, 0)
  ) %>%
  distinct()

log_count(marital_status_final, "Final marital status")

# ==============================================================================
# MODULE 4: VA ENROLLMENT PRIORITY GROUP
# ==============================================================================

cat("\n--- MODULE 4: VA Enrollment Priority Group ---\n")

# Load dimension tables
ADREnrollmentStatus <- tbl(cdwwork, in_schema(schema_ndim, "ADREnrollStatus"))
ADRPriorityGroup <- tbl(cdwwork, in_schema(schema_ndim, "ADRPriorityGroup"))

# Extract enrollment history
cat("Extracting VA enrollment history...\n")
pat_enrollment <- tbl(con, in_schema(schema_src, "ADR_ADREnrollHistory")) %>% 
  select(ADRPersonSID, ADREnrollStatusSID, ADRPrioritySubGroupSID, 
         ADRPriorityGroupSID, EnrollStartDate, EnrollEndDate, 
         RecordModifiedDate, NextRecordModifiedDate) %>%
  left_join(
    ADREnrollmentStatus %>% 
      select(ADREnrollStatusSID, EnrollStatusName, EnrollCategoryName), 
    by = "ADREnrollStatusSID", 
    copy = TRUE
  ) %>% 
  left_join(
    ADRPriorityGroup %>% 
      select(ADRPriorityGroupSID, PriorityGroupCode, PriorityGroupName), 
    by = "ADRPriorityGroupSID", 
    copy = TRUE
  ) %>% 
  filter(EnrollStatusName == "Verified" & EnrollCategoryName == "Enrolled") %>%
  inner_join(xw_icn_adr, by = "ADRPersonSID") %>% 
  select(PatientICN, ADRPersonSID, ADMSNDT, hosp_start, 
         EnrollStatusName, EnrollCategoryName, PriorityGroupCode, 
         PriorityGroupName, EnrollStartDate, EnrollEndDate, 
         RecordModifiedDate, NextRecordModifiedDate) %>%
  distinct() %>% 
  collect()

log_count(pat_enrollment, "Enrollment history records")

# Find enrollment status at time of hospitalization
cat("Matching enrollment to hospitalization dates...\n")
pat_enrollment_fin <- pat_enrollment %>%
  filter(
    # Enrollment started before hospitalization
    case_when(
      !is.na(EnrollStartDate) ~ as.Date(EnrollStartDate),
      !is.na(EnrollEndDate) ~ as.Date(EnrollEndDate),
      TRUE ~ as.Date(RecordModifiedDate)
    ) < hosp_start,
    # Enrollment active during hospitalization
    coalesce(as.Date(NextRecordModifiedDate), as.Date("2100-12-31")) >= hosp_start
  ) %>%
  group_by(PatientICN, ADMSNDT) %>%
  arrange(desc(RecordModifiedDate), 
          desc(coalesce(NextRecordModifiedDate, as.Date("2100-12-31")))) %>%
  mutate(MostRecentStatusChangeRecord = row_number()) %>%
  ungroup() %>%
  filter(MostRecentStatusChangeRecord == 1)

log_count(pat_enrollment_fin, "Enrollment at hospitalization")

# ==============================================================================
# MODULE 5: MEDICARE/MEDICAID ENROLLMENT
# ==============================================================================

cat("\n--- MODULE 5: Medicare/Medicaid Enrollment ---\n")

# Load monthly MBSF enrollment data
cat("Loading MBSF monthly enrollment...\n")
MBSF1822_monthly <- tbl(con, in_schema(schema_dflt, "MBSF_1822")) %>% 
  select(PatientICN, SCRSSN, RFRNC_YR, matches("\\d{2}$")) %>%
  collect() %>%
  pivot_longer(
    cols = matches("\\d{2}$"),
    names_to = c("variable", "month"),
    names_pattern = "^(.*?)(\\d{2})$"
  ) %>%
  drop_na(month) %>%
  distinct() %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  mutate(date = ymd(paste(RFRNC_YR, month, "01", sep = "-")))

# Save intermediate file (large dataset)
save_parquet_safe(MBSF1822_monthly, "MBSF1822_monthly.parquet")

# Process enrollment indicators
cat("Processing enrollment indicators...\n")
MBSF1822_monthly <- load_parquet_safe("MBSF1822_monthly.parquet") %>%
  inner_join(
    cohort_full %>% select(PatientICN) %>% distinct(), 
    by = "PatientICN"
  )

MBSF1822_monthly_trunc <- MBSF1822_monthly %>% 
  transmute(
    PatientICN, 
    date,
    BUYIN,
    HMOIND,
    DUAL_,
    # Create enrollment indicators
    dual = if_else(
      DUAL_ %in% c("01", "02", "03", "04", "06", "08", "09"), 
      1, 0
    ),
    MA = if_else(HMOIND == "C", 1, 0),
    FFS_only = if_else(BUYIN != 0 & MA == 0, 1, 0)
  )

# Match to hospitalization dates
MBSF1822_monthly_trunc <- MBSF1822_monthly_trunc %>%
  inner_join(
    cohort %>% select(PatientICN, ADMSNDT, hosp_start) %>% distinct(), 
    by = "PatientICN"
  ) %>%
  filter(date <= hosp_start) %>% 
  arrange(PatientICN, ADMSNDT, hosp_start, desc(date)) %>%
  group_by(PatientICN, ADMSNDT, hosp_start) %>%
  slice(1) %>%
  ungroup()

log_count(MBSF1822_monthly_trunc, "Medicare enrollment at hospitalization")

# ==============================================================================
# MODULE 6: RURALITY (GISURH)
# ==============================================================================

cat("\n--- MODULE 6: Rurality (GISURH) ---\n")

# Extract CDW patient addresses
cat("Loading patient addresses from CDW...\n")
pat_add <- tbl(con, in_schema(schema_src, "SPatient_SPatientAddress")) %>% 
  filter(
    OrdinalNumber == 13,  # Self-reported primary residence
    !is.na(StreetAddress1), 
    is.na(BadAddressIndicator)
  ) %>% 
  select(PatientSID, StreetAddress1, City, StateSID, 
         AddressChangeDateTime, GISAddressUpdatedDate, GISURH)

pa <- pat_add %>% 
  inner_join(xw_icn_sid, by = "PatientSID") %>% 
  mutate(
    LastKnownAddressDate = coalesce(AddressChangeDateTime, GISAddressUpdatedDate)
  ) %>% 
  collect() %>%
  transmute(
    PatientICN,
    date = ymd(as.Date(LastKnownAddressDate)), 
    GISURH,
    loc_source = "SPatient"
  ) %>% 
  drop_na(GISURH) %>% 
  distinct()

log_count(pa, "Patient addresses from CDW")

# Extract PSSG data (Planning Systems Support Group)
cat("Loading PSSG rurality data...\n")

get_pssg_data <- function(con, fiscal_year) {
  table_name <- paste0("PSSG_FY", fiscal_year)
  
  tryCatch({
    tbl(con, in_schema(schema_src, table_name)) %>% 
      transmute(
        PatientICN = ICN, 
        URH,
        Quarter, 
        Year
      ) %>% 
      collect() %>% 
      mutate(
        PSSGDate = case_when(
          Quarter == "q1" ~ paste0("12-31-", as.numeric(Year) - 1),
          Quarter == "q2" ~ paste0("03-31-", Year),
          Quarter == "q3" ~ paste0("06-30-", Year),
          TRUE ~ paste0("09-30-", Year)
        ),
        PatientICN = str_sub(PatientICN, start = 1L, end = 10L)
      ) %>% 
      inner_join(
        cohort %>% select(PatientICN) %>% distinct(), 
        by = "PatientICN"
      )
  }, error = function(e) {
    cat("Warning: PSSG table for FY", fiscal_year, "not found\n")
    return(NULL)
  })
}

# Load PSSG data for relevant years
pssg_years <- 2017:2022
pssg_all <- purrr::map_dfr(pssg_years, ~get_pssg_data(con, .x)) %>%
  filter(!is.null(.))

# Process PSSG rurality data
pssg_coh <- pssg_all %>% 
  transmute(
    PatientICN, 
    date = ymd(as.Date(PSSGDate, "%m-%d-%Y")), 
    loc_source = "PSSG", 
    GISURH = URH
  ) %>% 
  filter(GISURH != " " & GISURH != "") %>% 
  drop_na(GISURH) %>% 
  distinct()

log_count(pssg_coh, "PSSG rurality records")

# Combine all rurality sources
cat("Combining rurality sources...\n")
all_gisurh <- rbind(pssg_coh, pa) %>% distinct()

# Match to hospitalization (most recent before discharge)
pat_info <- all_gisurh %>% 
  inner_join(cohort, by = "PatientICN") %>%
  filter(date <= DSCHRGDT) %>% 
  arrange(PatientICN, ADMSNDT, desc(date)) %>% 
  group_by(PatientICN, ADMSNDT) %>% 
  slice(1) %>% 
  ungroup()

log_count(pat_info, "Rurality matched to hospitalizations")

# ==============================================================================
# MODULE 7: MEDICARE ZIP CODE RURALITY (MBSF + RUCA)
# ==============================================================================

cat("\n--- MODULE 7: Medicare ZIP Code Rurality (RUCA) ---\n")

# Load MBSF zip codes
MBSF1822_rurality <- tbl(con, in_schema(schema_dflt, "MBSF_1822")) %>% 
  select(PatientICN, year = RFRNC_YR, STATE_CD, CNTY_CD, ZIP_CD) %>% 
  distinct() %>%
  inner_join(
    cohort %>% select(PatientICN, ADMSNDT, hosp_start) %>% distinct(), 
    by = "PatientICN", copy = T
  ) %>% collect()

# Get most recent zip code before hospitalization
MBSF1822_rurality_update <- MBSF1822_rurality %>% 
  filter(hosp_start >= as.Date(paste(year, "01", "01", sep = "-"))) %>% 
  arrange(PatientICN, ADMSNDT, hosp_start, desc(year)) %>%
  group_by(PatientICN, ADMSNDT, hosp_start) %>%
  slice(1) %>%
  ungroup()

# Load RUCA (Rural-Urban Commuting Area) codes
cat("Loading RUCA codes...\n")
ruca_file <- file.path(project_base, "RUCA/RUCA-codes-2020-zipcode.xlsx")

if (file.exists(ruca_file)) {
  ruca_s2 <- readxl::read_xlsx(ruca_file, sheet = 2, skip = 1)
  
  # Map RUCA codes to GISURH categories
  # Reference: https://www.ruralhealth.va.gov/docs/ORH_rualityFactSheet_508.pdf
  MBSF1822_rurality_update_ruca <- MBSF1822_rurality_update %>% 
    ungroup() %>%
    left_join(ruca_s2, by = c("ZIP_CD" = "ZIPCode")) %>%
    transmute(
      PatientICN,
      ADMSNDT,
      ZIP_CD,
      mbsf_GISURH = case_when(
        SecondaryRUCA <= 1.1 ~ "U",  # Urban
        SecondaryRUCA >= 2 & SecondaryRUCA <= 9 ~ "R",  # Rural
        SecondaryRUCA %in% c(10.1, 10.2, 10.3) ~ "R",  # Rural
        SecondaryRUCA == 10 ~ "H",  # Highly rural
        TRUE ~ NA_character_
      ),
      mbsf_loc_source = "MBSF"
    )
  
  log_count(MBSF1822_rurality_update_ruca, "MBSF rurality with RUCA codes")
} else {
  warning("RUCA file not found. Skipping RUCA-based rurality.")
  MBSF1822_rurality_update_ruca <- NULL
}

# ==============================================================================
# MODULE 8: AREA DEPRIVATION INDEX (ADI)
# ==============================================================================

cat("\n--- MODULE 8: Area Deprivation Index ---\n")

# Load ADI data
adi_pssg <- tbl(con, in_schema(schema_src, "ADI_Pat_PSSG")) %>% 
  transmute(
    PatientICN = PATIENTICN,
    Quarter = quarter,
    Year = year,
    ADI_NATRANK
  ) %>%
  inner_join(cohort, by = "PatientICN", copy = TRUE) %>% 
  collect() %>% 
  mutate(
    PSSGDate = case_when(
      Quarter == "q1" ~ paste0("12-31-", as.numeric(Year) - 1),
      Quarter == "q2" ~ paste0("03-31-", Year),
      Quarter == "q3" ~ paste0("06-30-", Year),
      TRUE ~ paste0("09-30-", Year)
    ),
    PSSGDate = ymd(as.Date(PSSGDate, "%m-%d-%Y"))
  ) %>% 
  filter(PSSGDate <= hosp_start)

# Get most recent ADI before hospitalization
adi_pssg_recent <- adi_pssg %>% 
  group_by(PatientICN, ADMSNDT) %>%
  arrange(desc(PSSGDate)) %>%
  slice(1) %>%
  ungroup()

log_count(adi_pssg_recent, "ADI at hospitalization")

# ==============================================================================
# COMBINE ALL DEMOGRAPHICS
# ==============================================================================

cat("\n--- Combining All Demographics ---\n")

demo_cohort <- cohort %>% 
  # Medicare/Medicaid enrollment (time-specific)
  left_join(
    MBSF1822_monthly_trunc %>% 
      select(PatientICN, ADMSNDT, dual, FFS_only, MA), 
    by = c("PatientICN", "ADMSNDT")
  ) %>%
  # Basic demographics (person-level)
  left_join(omop_person_cms, by = "PERSON_ID") %>% 
  left_join(omop_person_cdw, by = "PERSON_ID") %>% 
  left_join(MBSF1822_demo_update, by = "PatientICN") %>%
  left_join(omop_xw_death, by = "PERSON_ID") %>% 
  # Marital status
  left_join(marital_status_final, by = c("PatientICN", "ADMSNDT")) %>%
  # VA enrollment
  left_join(
    pat_enrollment_fin %>% select(PatientICN, ADMSNDT, PriorityGroupName), 
    by = c("PatientICN", "ADMSNDT")
  ) %>%
  # Rurality (time-specific)
  left_join(
    pat_info %>% select(PatientICN, ADMSNDT, GISURH, loc_source), 
    by = c("PatientICN", "ADMSNDT")
  ) %>%
  # ADI
  left_join(
    adi_pssg_recent %>% select(PatientICN, ADMSNDT, ADI_NATRANK), 
    by = c("PatientICN", "ADMSNDT")
  )

# Add MBSF rurality if available
if (!is.null(MBSF1822_rurality_update_ruca)) {
  demo_cohort <- demo_cohort %>%
    left_join(MBSF1822_rurality_update_ruca, by = c("PatientICN", "ADMSNDT"))
} else {
  demo_cohort <- demo_cohort %>%
    mutate(mbsf_GISURH = NA_character_, mbsf_loc_source = NA_character_)
}

# Create final cleaned variables
demo_cohort <- demo_cohort %>%
  transmute(
    PatientICN,
    PERSON_ID,
    ADMSNDT,
    # Dates
    dob = coalesce(mbsf_dob, cms_dob, cdw_dob),
    dod = coalesce(mbsf_dod, dod),
    # Age
    Age_ADMSNDT = floor(as.numeric(hosp_start - dob) / 365.25),
    # Enrollment
    dual, 
    FFS_only,
    MA,
    # Demographics
    Race = coalesce(mbsf_race, cms_Race, cdw_Race),
    Race = case_when(
      Race %in% c("Black", "Black or African American") ~ "Black",
      Race == "White" ~ "White",
      Race %in% c("Asian", "Asian or Pacific Islander", 
                  "Native Hawaiian or Other Pacific Islander") ~ "Asian",
      Race == "American Indian or Alaska Native" ~ "American Indian or Alaska Native",
      Race == "Other" ~ "Other",
      TRUE ~ "Unknown"
    ),
    Non_White = if_else(Race %!in% c("White", "Unknown"), 1, 0),
    Ethnicity = coalesce(cms_Ethnicity, cdw_Ethnicity, mbsf_Ethnicity),
    Gender = coalesce(mbsf_sex, cms_Gender, cdw_Gender),
    # Marital status
    MaritalStatus,
    Married,
    # Geography
    GISURH = coalesce(GISURH, mbsf_GISURH),
    loc_source = coalesce(loc_source, mbsf_loc_source),
    Rural = if_else(GISURH %in% c("R", "H"), 1, 0),
    # Deprivation
    ADI_NATRANK,
    # VA enrollment
    PriorityGroupName
  )

log_count(demo_cohort, "Final demographics dataset")

# ==============================================================================
# DATA VALIDATION
# ==============================================================================

cat("\n--- Data Validation ---\n")

# Summary statistics
cat("\nDemographics summary:\n")
print(table1(~ Race + Ethnicity + Gender + Age_ADMSNDT + MaritalStatus + Married, 
             data = demo_cohort))

cat("\nEnrollment summary:\n")
print(table1(~ as.factor(dual) + as.factor(FFS_only) + as.factor(MA) + 
               PriorityGroupName, 
             data = demo_cohort))

cat("\nRurality summary:\n")
print(table1(~ GISURH + Rural + loc_source + ADI_NATRANK, 
             data = demo_cohort))

# Check missing values
cat("\nMissing value counts:\n")
cat("Age:", sum(is.na(demo_cohort$Age_ADMSNDT)), "\n")
cat("Gender:", sum(is.na(demo_cohort$Gender)), "\n")
cat("Race:", sum(demo_cohort$Race == "Unknown"), "\n")
cat("Marital Status:", sum(demo_cohort$MaritalStatus == "UNKNOWN"), "\n")
cat("Rurality:", sum(is.na(demo_cohort$GISURH)), "\n")

# Check age distribution
cat("\nAge distribution:\n")
print(summary(demo_cohort$Age_ADMSNDT))

# Flag unusual values
if (any(demo_cohort$Age_ADMSNDT < 18, na.rm = TRUE)) {
  warning("Some patients are under 18 years old")
}
if (any(demo_cohort$Age_ADMSNDT > 110, na.rm = TRUE)) {
  warning("Some patients are over 110 years old")
}

# ==============================================================================
# SAVE OUTPUT
# ==============================================================================

cat("\n--- Saving Output ---\n")
save_parquet_safe(demo_cohort, "demo_cohort.parquet")

# ==============================================================================
# SESSION INFO
# ==============================================================================

cat("\n=============================================================================\n")
cat("SCRIPT 03A COMPLETED SUCCESSFULLY\n")
cat("=============================================================================\n")
cat("Output: demo_cohort.parquet\n")
cat("Rows:", format(nrow(demo_cohort), big.mark = ","), "\n")
cat("Unique patients:", 
    format(length(unique(demo_cohort$PatientICN)), big.mark = ","), "\n")
cat("\nKey variables:\n")
cat("  - Demographics: Age, Gender, Race, Ethnicity, Marital Status\n")
cat("  - Enrollment: Medicare dual, FFS, MA; VA Priority Group\n")
cat("  - Geography: Rurality (GISURH), Area Deprivation Index\n")
cat("=============================================================================\n\n")

# Clean up
dbDisconnect(con)
dbDisconnect(cdwwork)

################################################################################
# END OF SCRIPT
################################################################################
