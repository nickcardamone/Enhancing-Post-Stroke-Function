################################################################################
# SCRIPT 02B: EXTRACT PRIOR HEALTHCARE SERVICE UTILIZATION
# Stroke Rehabilitation Research - CDA Study
# ORD Waddell 202408036D
################################################################################
#
# Purpose:
#   Calculate prior healthcare utilization (1 year before stroke hospitalization)
#   including primary care visits and inpatient admissions. These variables
#   serve as covariates in regression models.
#
# Author: Nick Cardamone
# Created: 2025-07-30
# Last Modified: 2025-12-11
# Version: 2.0
#
# Inputs:
#   - rhf_post_acute_final.parquet (from Script 2a)
#   - pdx_stroke_visit_summary.parquet (from Script 1)
#   - rhf_no_gap.parquet (from Script 2a)
#   - CDWWork.Dim.StopCode
#   - Src.Outpat_Workload
#
# Outputs:
#   - cohort_prevservice_use.parquet: Prior service use indicators
#
# Variables Created:
#   - p1_count_pc: Number of primary care visits in prior year
#   - p1_any_pc: Any primary care visit in prior year (0/1)
#   - p1_count_inpat: Number of inpatient admissions in prior year
#   - p1_los_inpat: Total inpatient days in prior year
#   - p1_any_inpat: Any inpatient admission in prior year (0/1)
#
################################################################################

# ==============================================================================
# SETUP
# ==============================================================================

# Source configuration
source("00_config.R")

# Load required packages
load_packages(c("dplyr", "dbplyr", "DBI", "arrow", "data.table", 
                "table1", "lubridate", "stringr"))

# SCRIPT 02B: EXTRACT PRIOR HEALTHCARE SERVICE UTILIZATION

# Set working directory
setwd(project_base)

# ==============================================================================
# LOAD INPUT DATASETS
# ==============================================================================

cat("Loading input datasets...\n")

# Load cohort data
cohort_full <- load_parquet_safe("rhf_post_acute_final.parquet")
pdx_stroke_visit_summary <- load_parquet_safe("pdx_stroke_visit_summary.parquet")

# Extract stroke admission dates and discharge source
pdx_stroke_visit_summary <- pdx_stroke_visit_summary %>% 
  transmute(
    scrssn = SCRSSN, 
    ADMSNDT = ACUTE_INPATIENT_VISIT_START, 
    discharge_SOURCE
  ) %>% 
  distinct()

# Create cohort with lookback periods
cat("Creating lookback date windows...\n")
cohort <- cohort_full %>%
  select(PatientICN, scrssn, PERSON_ID, ADMSNDT, DSCHRGDT, hee_from, hee_thru) %>%
  group_by(PatientICN, scrssn, PERSON_ID, ADMSNDT) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    # Use earliest date (RHF or admission) as hospitalization start
    hosp_start = if_else(hee_from < ADMSNDT, hee_from, ADMSNDT),
    # Calculate lookback periods
    year_prior_to_admission = hosp_start %m-% years(1),
    twoyears_prior_to_admission = hosp_start %m-% years(2)
  ) %>%
  left_join(pdx_stroke_visit_summary, by = c("scrssn", "ADMSNDT"))

log_count(cohort, "Cohort with lookback periods")

# ==============================================================================
# DATABASE CONNECTION
# ==============================================================================

cat("\nEstablishing database connections...\n")
con <- connect_db(database = db_project, server = db_server)
cdwwork <- connect_db(database = db_cdwwork, server = db_server)

# ==============================================================================
# CREATE PATIENT CROSSWALK (ICN TO SID)
# ==============================================================================

cat("Creating PatientICN to PatientSID crosswalk...\n")
xw_icn_sid <- tbl(con, in_schema(schema_src, 'SPatient_Spatient')) %>%
  inner_join(cohort, by = "PatientICN", copy = TRUE) %>%
  select(PatientICN, ADMSNDT, DSCHRGDT, hosp_start, year_prior_to_admission, PatientSID) %>%
  distinct()

# ==============================================================================
# EXTRACT PRIMARY CARE VISITS
# ==============================================================================

cat("\n--- Extracting Primary Care Visits ---\n")

# Load stop code dimension tables
cat("Loading stop code reference tables...\n")
prim_stopcode <- tbl(cdwwork, in_schema(schema_dim, 'StopCode')) %>% 
  transmute(StopCodeSID, Primary_SC = StopCode)

sec_stopcode <- tbl(cdwwork, in_schema(schema_dim, 'StopCode')) %>% 
  transmute(StopCodeSID, Secondary_SC = StopCode)

# Load outpatient workload data
cat("Loading outpatient visit data...\n")
outpat <- tbl(con, in_schema(schema_src, 'Outpat_Workload')) %>%
  select(PatientSID, VisitDateTime, Sta3n, PrimaryStopCodeSID, 
         SecondaryStopCodeSID, ServiceCategory, PatientStatusInOut)

# Join stop codes and filter to study cohort
cat("Identifying primary care visits in prior year...\n")
pc_outpat <- outpat %>%
  inner_join(prim_stopcode, by = c("PrimaryStopCodeSID" = "StopCodeSID"), copy = TRUE) %>%
  left_join(sec_stopcode, by = c("SecondaryStopCodeSID" = "StopCodeSID"), copy = TRUE) %>%
  inner_join(xw_icn_sid, by = "PatientSID") %>%
  # Filter to visits within lookback period
  filter(as.Date(VisitDateTime) >= year_prior_to_admission & 
         as.Date(VisitDateTime) < ADMSNDT) %>%
  collect()

log_count(pc_outpat, "Outpatient visits in lookback period (raw)")

# Apply primary care visit criteria
# Primary care is defined as:
#   - Primary stop codes: 322, 323, 348, 350
#   - Allowed secondary stop codes (see config)
#   - Special rule: Primary 350 + Secondary 117 is valid
cat("Applying primary care visit criteria...\n")
pc_outpat <- pc_outpat %>% 
  mutate(
    # Set missing secondary codes to 999 for easier filtering
    Secondary_SC = if_else(is.na(Secondary_SC), 999, Secondary_SC),
    # Define primary care visit
    primary_care = if_else(
      Primary_SC %in% pc_primary_stopcodes & Secondary_SC %in% pc_secondary_stopcodes, 
      1, 0
    ),
    # Apply special rule for stop code 350 + 117 combination
    primary_care = if_else(Primary_SC == 350 & Secondary_SC == 117, 1, primary_care)
  )

# Apply additional filters:
#   - Exclude hospitalized patients (PatientStatusInOut = 1)
#   - Include only ambulatory encounters (ServiceCategory = "A")
#   - Include only identified primary care visits
pc_outpat_fin <- pc_outpat %>%
  filter(
    PatientStatusInOut != 1 & 
    ServiceCategory == "A" & 
    primary_care == 1
  )

log_count(pc_outpat_fin, "Valid primary care visits")

# Summarize primary care visits by patient and hospitalization
cat("Summarizing primary care utilization...\n")
prior_pc <- pc_outpat_fin %>%
  mutate(date = as.Date(VisitDateTime)) %>%
  select(PatientICN, ADMSNDT, date) %>%
  distinct() %>%
  group_by(PatientICN, ADMSNDT) %>% 
  summarize(
    p1_count_pc = n(),
    p1_any_pc = 1,
    .groups = 'drop'
  )

log_count(prior_pc, "Patients with primary care visits")

# ==============================================================================
# EXTRACT PRIOR INPATIENT ADMISSIONS
# ==============================================================================

cat("\n--- Extracting Prior Inpatient Admissions ---\n")

# Load RHF data (already processed/cleaned)
rhf_no_gap <- load_parquet_safe("rhf_no_gap.parquet")

# Match RHF records in lookback period
cat("Identifying inpatient admissions in prior year...\n")
prior_rhf_gap <- cohort %>% 
  select(-hee_from, -hee_thru) %>%
  inner_join(rhf_no_gap, by = "scrssn") %>%
  # Filter to episodes starting in prior year but before current hospitalization
  filter(hee_from >= year_prior_to_admission & hee_from < hosp_start) %>%
  # Keep only inpatient episodes
  filter(Grouping == "INP") %>%
  mutate(los = as.numeric(hee_thru - hee_from)) %>%
  arrange(scrssn, ADMSNDT) %>% 
  group_by(scrssn, ADMSNDT) %>% 
  summarize(
    p1_count_inpat = n(),
    p1_los_inpat = sum(los),
    .groups = 'drop'
  )

log_count(prior_rhf_gap, "Patients with prior inpatient admissions")

# ==============================================================================
# COMBINE AND FINALIZE PRIOR SERVICE USE DATASET
# ==============================================================================

cat("\n--- Finalizing Prior Service Use Dataset ---\n")

cohort_prevservice_use <- cohort %>% 
  left_join(prior_pc, by = c("PatientICN", "ADMSNDT")) %>%
  left_join(prior_rhf_gap, by = c("scrssn", "ADMSNDT")) %>%
  transmute(
    PatientICN,
    ADMSNDT,
    # Inpatient utilization
    p1_count_inpat = replace_na(p1_count_inpat, 0), 
    p1_los_inpat = replace_na(p1_los_inpat, 0),
    p1_any_inpat = if_else(p1_count_inpat > 0, 1, 0),
    # Primary care utilization
    p1_any_pc = replace_na(p1_any_pc, 0),
    p1_count_pc = replace_na(p1_count_pc, 0)
  )

log_count(cohort_prevservice_use, "Final prior service use dataset")

# ==============================================================================
# DATA VALIDATION
# ==============================================================================

cat("\n--- Data Validation ---\n")

# Summary statistics
cat("\nPrior service use summary (all patients):\n")
print(table1(~ p1_count_inpat + p1_los_inpat + p1_any_inpat + 
               p1_any_pc + p1_count_pc, 
             data = cohort_prevservice_use))

# Summary for patients with any inpatient stay
cat("\nPrior service use summary (patients with any inpatient stay):\n")
print(table1(~ p1_count_inpat + p1_los_inpat + p1_any_inpat + 
               p1_any_pc + p1_count_pc, 
             data = cohort_prevservice_use %>% filter(p1_any_inpat > 0)))

# Check for missing values
cat("\nMissing value check:\n")
cat("Missing PatientICN:", sum(is.na(cohort_prevservice_use$PatientICN)), "\n")
cat("Missing ADMSNDT:", sum(is.na(cohort_prevservice_use$ADMSNDT)), "\n")

# Distribution checks
cat("\nDistribution of inpatient admissions:\n")
print(table(cohort_prevservice_use$p1_count_inpat))

cat("\nDistribution of primary care visits:\n")
print(table(cohort_prevservice_use$p1_count_pc))

# ==============================================================================
# SAVE OUTPUT
# ==============================================================================

cat("\n--- Saving Output ---\n")
save_parquet_safe(cohort_prevservice_use, "cohort_prevservice_use.parquet")

# ==============================================================================
# SESSION INFO
# ==============================================================================

cat("\n=============================================================================\n")
cat("SCRIPT 02B COMPLETED SUCCESSFULLY\n")
cat("=============================================================================\n")
cat("Output: cohort_prevservice_use.parquet\n")
cat("Rows:", format(nrow(cohort_prevservice_use), big.mark = ","), "\n")
cat("Patients with any PC visit:", 
    sum(cohort_prevservice_use$p1_any_pc), "\n")
cat("Patients with any inpatient admission:", 
    sum(cohort_prevservice_use$p1_any_inpat), "\n")
cat("=============================================================================\n\n")

# Clean up
dbDisconnect(con)
dbDisconnect(cdwwork)

################################################################################
# END OF SCRIPT
################################################################################
