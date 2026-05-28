################################################################################
# SCRIPT 06a2: FIND PRIOR STROKES FOR FINAL COHORT
# Stroke Rehabilitation Research - CDA Study
# ORD Waddell 202408036D
################################################################################
#
# Purpose:
#   Identify all acute inpatient hospitalizations with PRIMARY diagnosis of 
#   stroke from both VA (CDW) and Medicare (CMS) data sources. Roll up 
#   transfers and consecutive stays into single hospitalization episodes.
#
# Author: Nick Cardamone
# Created: 2026-01-16
# Last Modified: 2026-01-16
# Version: 1.0
#
#
################################################################################

# ==============================================================================
# SETUP
# ==============================================================================

# Source configuration
source("00_config.R")

# Check and load required packages
check_packages(required_packages)
load_packages(c("DBI", "dbplyr", "dplyr", "arrow", "stringr", 
                "lubridate", "data.table", "VINCI"))

cat("\n=============================================================================\n")
cat("SCRIPT 01: CREATE STROKE HOSPITALIZATION COHORT\n")
cat("=============================================================================\n\n")

# ==============================================================================
# DATABASE CONNECTIONS
# ==============================================================================

cat("Establishing database connections...\n")
con <- connect_db(database = db_project, server = db_server)
cdwwork <- connect_db(database = db_cdwwork, server = db_server)

# Load crosswalk tables
cat("Loading patient crosswalk tables...\n")
cw_sid <- tbl(con, in_schema(schema_dflt, 'cw_sid')) 
cw_icn <- tbl(con, in_schema(schema_dflt, 'cw_icn'))
cw_scrssn <- tbl(con, in_schema(schema_dflt, 'cw_scrssn'))

# ==============================================================================
# EXTRACT PRIMARY DIAGNOSIS OF STROKE - CDW
# ==============================================================================

analysis_icns <- load_parquet_safe("analysis_data.parquet") %>% 
  select(PatientICN) %>%
  distinct() %>% as.data.frame() %>% head(10)

cat("\n--- Processing CDW (VA) Data ---\n")

# Load OMOP crosswalk: Patient ICN to PERSON_ID
omop_xw <- tbl(con, in_schema(schema_src, 'OMOPV5Map_SPatient_PERSON')) %>% 
  dplyr::inner_join(analysis_icns, by = "PatientICN", copy = T)

# Define primary diagnosis concept IDs
# These OMOP concept IDs represent primary/first position diagnoses
cat("Defining primary diagnosis concepts...\n")
omop_concept_pdx <- tbl(con, in_schema(schema_src, 'OMOPV5_CONCEPT')) %>% 
  filter(CONCEPT_NAME %in% c("Inpatient header - primary position", 
                             "Inpatient header - primary", 
                             "Inpatient header - 1st position")) %>% 
  transmute(CONCEPT_ID, CONCEPT_NAME, pdx = 1)

# Load inpatient visits from CDW
# Visit concept IDs (defined in config):
#   9201 = Inpatient Visit
#   800000001 = Inpatient Observation Visit
#   581385 = Emergency Room and Inpatient Visit
#   80000002 = Outpatient Visit Within Inpatient Visit
#   262 = Emergency Room and Inpatient Visit
#   9203 = Emergency Room Visit
cat("Loading CDW inpatient visits...\n")
omop_inpatient_cdw <- tbl(con, in_schema(schema_src, 'OMOPV5_VISIT_OCCURRENCE')) %>% 
  filter(VISIT_CONCEPT_ID %in% visit_concept_ids) %>% 
  filter(VISIT_START_DATE >= "01/01/2017" & 
           VISIT_START_DATE < "01/01/2023" & 
           VISIT_END_DATE < "01/01/2023") %>% 
  select(VISIT_OCCURRENCE_ID, PERSON_ID, VISIT_START_DATETIME, VISIT_END_DATETIME, 
         VISIT_CONCEPT_ID, VISIT_TYPE_CONCEPT_ID, VISIT_SOURCE_VALUE) %>% 
  inner_join(omop_xw, by = "PERSON_ID") %>% 
  distinct()

# Load condition codes for CDW
cat("Loading CDW condition codes...\n")
omop_cond_cdw <- tbl(con, in_schema(schema_src, 'OMOPV5_CONDITION_OCCURRENCE')) %>% 
  select(VISIT_OCCURRENCE_ID, CONDITION_SOURCE_VALUE, CONDITION_TYPE_CONCEPT_ID)

# Filter for primary diagnoses only
# Extract stroke diagnosis codes
# ICD-10 stroke codes: G46, I60, I61, I62, I63, I66, I67.89, I97.81, I97.82
cat("Filtering for primary stroke diagnoses (CDW)...\n")
omop_pdx_cdw <- omop_cond_cdw %>% 
  inner_join(omop_concept_pdx, by = c("CONDITION_TYPE_CONCEPT_ID" = "CONCEPT_ID")) %>%
  inner_join(omop_inpatient_cdw %>% select(PERSON_ID, VISIT_OCCURRENCE_ID) %>% distinct(), 
             by = "VISIT_OCCURRENCE_ID") %>%
  distinct() %>% 
  collect() %>% 
  # Extract ICD-10 code from formatted string (format: "ICD10|X##.##")
  mutate(CONDITION_SOURCE_VALUE = substr(CONDITION_SOURCE_VALUE, 7, nchar(CONDITION_SOURCE_VALUE))) %>% 
  mutate(ICD10 = substr(CONDITION_SOURCE_VALUE, 1, 3)) %>% 
  arrange(VISIT_OCCURRENCE_ID, CONCEPT_NAME) %>% 
  # Take first diagnosis per visit (highest priority)
  group_by(VISIT_OCCURRENCE_ID) %>% 
  slice_head(n = 1) %>% 
  ungroup()

# Flag stroke diagnoses using pattern matching
omop_pdx_cdw <- omop_pdx_cdw %>%  
  mutate(pdx_stroke = as.numeric(str_detect(CONDITION_SOURCE_VALUE, pattern_stroke_cdw)))

log_count(omop_pdx_cdw %>% filter(pdx_stroke == 1), 
          "CDW visits with primary stroke diagnosis")

# Create dataset of persons with stroke
omop_pdx_stroke_cdw <- omop_pdx_cdw %>% filter(pdx_stroke == 1)

# Join all inpatient visits for patients with stroke back to diagnosis data
omop_pdx_inpatient_cdw <- omop_inpatient_cdw %>% 
  inner_join(omop_pdx_stroke_cdw %>% select(PERSON_ID) %>% distinct(), 
             by = "PERSON_ID", copy = TRUE) %>% 
  left_join(omop_pdx_cdw %>% select(-PERSON_ID), 
            by = "VISIT_OCCURRENCE_ID", copy = TRUE) %>% 
  transmute(PERSON_ID, PatientICN, SOURCE = "CDW", VISIT_OCCURRENCE_ID, 
            VISIT_START_DATETIME, VISIT_END_DATETIME, VISIT_CONCEPT_ID, 
            CONDITION_SOURCE_VALUE, ICD10, pdx_stroke, 
            VISIT_TYPE_CONCEPT_ID, VISIT_SOURCE_VALUE) %>% 
  distinct() %>% 
  collect() %>% 
  arrange(PERSON_ID, PatientICN, VISIT_START_DATETIME)

log_count(omop_pdx_inpatient_cdw, "CDW inpatient visits for stroke patients")

# ==============================================================================
# EXTRACT PRIMARY DIAGNOSIS OF STROKE - CMS
# ==============================================================================

cat("\n--- Processing CMS (Medicare) Data ---\n")

# Load CMS condition codes
cat("Loading CMS condition codes...\n")
omop_cond_cms <- tbl(con, in_schema(schema_src, 'OMOP_CMS_CONDITION_OCCURRENCE_FF291')) %>% 
  select(VISIT_OCCURRENCE_ID, CONDITION_SOURCE_VALUE, CONDITION_TYPE_CONCEPT_ID)

# Load CMS inpatient visits
cat("Loading CMS inpatient visits...\n")
omop_inpatient_cms <- tbl(con, in_schema(schema_src, 'OMOP_CMS_VISIT_OCCURRENCE_FF291')) %>% 
  filter(VISIT_CONCEPT_ID %in% visit_concept_ids) %>% 
  filter(VISIT_START_DATE >= "01/01/2017" & 
           VISIT_START_DATE < "01/01/2023" & 
           VISIT_END_DATE < "01/01/2023") %>% 
  select(VISIT_OCCURRENCE_ID, PERSON_ID, VISIT_START_DATETIME, VISIT_END_DATETIME, 
         VISIT_CONCEPT_ID, VISIT_TYPE_CONCEPT_ID, VISIT_SOURCE_VALUE) %>% 
  inner_join(omop_xw, by = "PERSON_ID") %>% 
  distinct()

# Filter for primary stroke diagnoses in CMS
# CMS format: ICD10-XXXXX (no decimal point)
cat("Filtering for primary stroke diagnoses (CMS)...\n")
omop_pdx_cms <- omop_cond_cms %>% 
  inner_join(omop_concept_pdx, by = c("CONDITION_TYPE_CONCEPT_ID" = "CONCEPT_ID")) %>%
  inner_join(omop_inpatient_cms %>% select(PERSON_ID, VISIT_OCCURRENCE_ID) %>% distinct(), 
             by = "VISIT_OCCURRENCE_ID") %>%
  distinct() %>% 
  collect() %>% 
  # Extract ICD-10 code from formatted string (format: "ICD10-XXXXX")
  mutate(CONDITION_SOURCE_VALUE = substr(CONDITION_SOURCE_VALUE, 7, nchar(CONDITION_SOURCE_VALUE))) %>% 
  mutate(ICD10 = substr(CONDITION_SOURCE_VALUE, 1, 3)) %>% 
  arrange(VISIT_OCCURRENCE_ID, CONCEPT_NAME) %>% 
  # Take first diagnosis per visit
  group_by(VISIT_OCCURRENCE_ID) %>% 
  slice_head(n = 1) %>% 
  ungroup()

# Flag stroke diagnoses
omop_pdx_cms <- omop_pdx_cms %>% 
  mutate(pdx_stroke = as.numeric(str_detect(CONDITION_SOURCE_VALUE, pattern_stroke_cms)))

log_count(omop_pdx_cms %>% filter(pdx_stroke == 1), 
          "CMS visits with primary stroke diagnosis")

# Create dataset of persons with stroke
omop_pdx_stroke_cms <- omop_pdx_cms %>% filter(pdx_stroke == 1)

# Join all inpatient visits back to diagnosis data
omop_pdx_inpatient_cms <- omop_inpatient_cms %>% 
  inner_join(omop_pdx_stroke_cms %>% select(PERSON_ID) %>% distinct(), 
             by = "PERSON_ID", copy = TRUE) %>% 
  left_join(omop_pdx_cms %>% select(-PERSON_ID), 
            by = "VISIT_OCCURRENCE_ID", copy = TRUE) %>% 
  transmute(PERSON_ID, PatientICN, SOURCE = "CMS", VISIT_OCCURRENCE_ID, 
            VISIT_START_DATETIME, VISIT_END_DATETIME, VISIT_CONCEPT_ID, 
            CONDITION_SOURCE_VALUE, ICD10, pdx_stroke, 
            VISIT_TYPE_CONCEPT_ID, VISIT_SOURCE_VALUE) %>% 
  distinct() %>% 
  collect() %>% 
  arrange(PERSON_ID, PatientICN, VISIT_START_DATETIME)

log_count(omop_pdx_inpatient_cms, "CMS inpatient visits for stroke patients")

# ==============================================================================
# COMBINE CDW AND CMS DATA
# ==============================================================================

cat("\n--- Combining CDW and CMS Data ---\n")
prior_stroke_complete <- rbind(omop_pdx_inpatient_cdw, omop_pdx_inpatient_cms)
log_count(prior_stroke_complete, "Combined inpatient visits (CDW + CMS)")

# Save raw combined data
save_parquet_safe(prior_stroke_complete, "prior_stroke_complete.parquet")

# Open parquet:
prior_stroke_complete <- open_dataset("parquet/prior_stroke_complete.parquet") %>% collect()

# ==============================================================================
# ROLL UP TRANSFERS AND CONSECUTIVE ADMISSIONS
# ==============================================================================

cat("\n--- Rolling Up Transfers and Consecutive Stays ---\n")

# Identify transfers: admission date = prior discharge date
# Identify consecutive stays: admission date within same day as prior discharge
prior_stroke_complete_rollup <- prior_stroke_complete %>% 
  group_by(PatientICN, PERSON_ID) %>% 
  arrange(PatientICN, PERSON_ID, VISIT_START_DATETIME) %>% 
  mutate(
    VISIT_START_DATE = as.Date(VISIT_START_DATETIME),
    VISIT_END_DATE = as.Date(VISIT_END_DATETIME),
    # Transfer indicator: same-day transfer from one facility to another
    transfer = if_else(VISIT_START_DATE == lag(VISIT_END_DATE), 1, 0),
    transfer = replace_na(transfer, 0),
    # Track which source has primary stroke diagnosis
    CDW_pdx = if_else(SOURCE == "CDW" & pdx_stroke == 1, 1, 0),
    CMS_pdx = if_else(SOURCE == "CMS" & pdx_stroke == 1, 1, 0),
    # New admission indicator: current admission is after prior discharge
    new_admit = as.integer(VISIT_START_DATE > lag(VISIT_END_DATE, default = first(VISIT_START_DATE))),
    # Create run ID for consecutive stays
    admit_run = data.table::rleid(replace_na(cumsum(new_admit), 0))
  ) %>% 
  ungroup() %>% 
  mutate(
    pdx_stroke = replace_na(pdx_stroke, 0),
    CDW_pdx = replace_na(CDW_pdx, 0),
    CMS_pdx = replace_na(CMS_pdx, 0)
  )

log_count(prior_stroke_complete_rollup, "Visits with rollup indicators")

# For each admission run, get the MOST RECENT stroke ICD-10 code
# If multiple primary stroke diagnoses in a run, use the last one chronologically
cat("Extracting most recent stroke ICD-10 code per admission run...\n")
last_stroke_icd <- prior_stroke_complete_rollup %>% 
  filter(pdx_stroke == 1) %>% 
  group_by(PatientICN, admit_run) %>% 
  arrange(desc(VISIT_START_DATETIME)) %>%  
  slice_head(n = 1) %>% 
  select(PatientICN, admit_run, ICD10) %>% 
  ungroup()

# Summarize each admission run into single hospitalization
cat("Summarizing admission runs into hospitalizations...\n")
prior_stroke_visit_summary <- prior_stroke_complete_rollup %>% 
  group_by(PatientICN, admit_run) %>% 
  summarize(
    PERSON_ID = first(PERSON_ID),
    # Track data source at admission and discharge
    admit_SOURCE = first(SOURCE),
    discharge_SOURCE = last(SOURCE),
    # Track if stroke diagnosis appeared in CDW and/or CMS
    CDW_pdx = max(CDW_pdx),
    CMS_pdx = max(CMS_pdx),
    # Hospitalization dates span from first admission to last discharge
    ACUTE_INPATIENT_VISIT_START = first(VISIT_START_DATE),
    ACUTE_INPATIENT_VISIT_END = last(VISIT_END_DATE),
    .groups = 'drop'
  )

# Add stroke ICD-10 code
prior_stroke_visit_summary <- pdx_stroke_visit_summary %>% 
  left_join(last_stroke_icd, by = c("PatientICN", "admit_run"))

# Add scrambled SSN for linking to other datasets
prior_stroke_visit_summary <- prior_stroke_visit_summary %>% 
  left_join(cw_scrssn %>% select(SCRSSN, PatientICN) %>% distinct() %>% collect(), 
            by = "PatientICN")

# Remove duplicates (keep one row per patient-admission)
prior_stroke_visit_summary <- prior_stroke_visit_summary %>% 
  group_by(PatientICN, admit_run) %>% 
  filter(n() == 1) %>% 
  ungroup()

# Remove records without valid ICD-10 code and add year variable
prior_stroke_visit_summary <- prior_stroke_visit_summary %>% 
  filter(!is.na(ICD10)) %>% 
  mutate(year = year(ACUTE_INPATIENT_VISIT_START))

log_count(prior_stroke_visit_summary, "Final prior stroke hospitalizations")

# ==============================================================================
# DATA VALIDATION AND QUALITY CHECKS
# ==============================================================================

cat("\n--- Data Validation ---\n")

# Check year distribution
cat("\nHospitalizations by year and data source:\n")
print(table(pdx_stroke_visit_summary$year, pdx_stroke_visit_summary$discharge_SOURCE))

# Check stroke diagnosis source
cat("\nStroke diagnosis source:\n")
cat("CDW only:", sum(pdx_stroke_visit_summary$CDW_pdx == 1 & pdx_stroke_visit_summary$CMS_pdx == 0), "\n")
cat("CMS only:", sum(pdx_stroke_visit_summary$CDW_pdx == 0 & pdx_stroke_visit_summary$CMS_pdx == 1), "\n")
cat("Both:", sum(pdx_stroke_visit_summary$CDW_pdx == 1 & pdx_stroke_visit_summary$CMS_pdx == 1), "\n")

# Cross-tabulation
cat("\nCross-tabulation of stroke diagnosis by source:\n")
print(table(CDW = pdx_stroke_visit_summary$CDW_pdx, 
            CMS = pdx_stroke_visit_summary$CMS_pdx, 
            dnn = c("Stroke DX in CDW", "Stroke DX in CMS")))

# Check for missing SCRSSN
n_missing_scrssn <- sum(is.na(pdx_stroke_visit_summary$SCRSSN))
if (n_missing_scrssn > 0) {
  warning(sprintf("%d hospitalizations missing SCRSSN", n_missing_scrssn))
}

# Validate date ranges
validate_dates(study_start_date, study_end_date, 
               pdx_stroke_visit_summary$ACUTE_INPATIENT_VISIT_START)

# ==============================================================================
# SAVE FINAL DATASET
# ==============================================================================

cat("\n--- Saving Final Dataset ---\n")
save_parquet_safe(prior_stroke_visit_summary, "prior_stroke_visit_summary.parquet")

# ==============================================================================
# SESSION INFO
# ==============================================================================

cat("\n=============================================================================\n")
cat("SCRIPT 01 COMPLETED SUCCESSFULLY\n")
cat("=============================================================================\n")
cat("Output: pdx_stroke_visit_summary.parquet\n")
cat("Rows:", format(nrow(pdx_stroke_visit_summary), big.mark = ","), "\n")
cat("Unique patients:", format(length(unique(pdx_stroke_visit_summary$PatientICN)), big.mark = ","), "\n")
cat("Date range:", min(pdx_stroke_visit_summary$ACUTE_INPATIENT_VISIT_START), "to", 
    max(pdx_stroke_visit_summary$ACUTE_INPATIENT_VISIT_START), "\n")
cat("=============================================================================\n\n")

# Clean up
dbDisconnect(con)
dbDisconnect(cdwwork)

################################################################################
# END OF SCRIPT
################################################################################
