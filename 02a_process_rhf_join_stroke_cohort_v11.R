################################################################################
# SCRIPT 02A: PROCESS RHF AND LINK TO STROKE COHORT
# Stroke Rehabilitation Research - CDA Study
# ORD Waddell 202408036D
################################################################################
#
# Purpose:
#   Process Residential History File (RHF) data to identify post-acute care
#   episodes (IRF, SNF, HH) following stroke hospitalizations. Clean episodes
#   by removing small gaps, rolling up consecutive stays, and applying
#   discharge timing rules to link post-acute care to stroke admissions.
#
# Author: Nick Cardamone
# Created: 2025-05-12
# Last Modified: 2025-12-11
# Version: 11.0
#
# Processing Steps:
#   1. Load and categorize RHF episode types (groupings)
#   2. Link RHF to stroke cohort by scrambled SSN
#   3. Remove gaps in care ≤3 days and "OTHER" stays ≤3 days
#   4. Roll up consecutive episodes of same care setting
#   5. Apply post-acute care termination rules
#   6. Match RHF inpatient episodes to CDW/CMS stroke hospitalizations
#   7. Extract eligible post-acute care sequences
#
# Inputs:
#   - pdx_stroke_visit_summary.parquet (from Script 1)
#   - RHFB_SHIP2393 (RHF database table)
#   - Groupings.xlsx (manual classification of RHF episode types)
#
# Outputs:
#   - rhf_no_small_gap.parquet: RHF with small gaps removed
#   - rhf_no_gap.parquet: RHF fully cleaned
#   - rhf_cohort_included.parquet: Stroke admissions with post-acute care
#   - rhf_post_acute_final.parquet: Final post-acute care episodes
#   - dt_first_post_acute_elig.parquet: First post-acute episode per stroke
#
# Expected Results:
#   - ~3.5M RHF records for stroke patients (raw)
#   - ~430K inpatient episodes after cleaning
#   - ~74K stroke hospitalizations with post-acute care
#   - ~98K post-acute care episodes
#
################################################################################

# ==============================================================================
# SETUP
# ==============================================================================

# Source configuration
source("00_config.R")

# Load required packages
load_packages(c("dplyr", "data.table", "dbplyr", "DBI", 
                "arrow", "zoo", "stringr", "readxl", "gt"))

cat("\n=============================================================================\n")
cat("SCRIPT 02A: PROCESS RHF AND LINK TO STROKE COHORT\n")
cat("=============================================================================\n\n")

# Set working directory
setwd(project_base)

# ==============================================================================
# LOAD RHF GROUPING CLASSIFICATION
# ==============================================================================

cat("Loading RHF episode type groupings...\n")

# Load manual classification of RHF type1 codes into care settings
# Groupings: IRF, SNF, HH, INP, GAP, HOSPICE, LTCH, OTHER
groupings_file <- file.path(project_base, "Groupings.xlsx")

if (!file.exists(groupings_file)) {
  stop("Groupings.xlsx file not found. This file is required for RHF classification.")
}

Groupings <- readxl::read_xlsx(groupings_file) %>% 
  mutate(Grouping = if_else(Grouping == "EXCL", "OTHER", Grouping))

log_count(Groupings, "RHF type1 code groupings")

# ==============================================================================
# DATABASE CONNECTION AND DATA LOADING
# ==============================================================================

cat("\nEstablishing database connection...\n")
con <- connect_db(database = db_project, server = db_server)

# Load stroke cohort
cat("Loading stroke cohort...\n")
cohort <- load_parquet_safe("pdx_stroke_visit_summary.parquet") %>%
  mutate(
    scrssn = SCRSSN, 
    ADMSNDT = ACUTE_INPATIENT_VISIT_START, 
    DSCHRGDT = ACUTE_INPATIENT_VISIT_END,
    # Track whether stroke was in VA, non-VA, or both systems
    setting = case_when(
      CDW_pdx == 1 & CMS_pdx == 0 ~ "VA",
      CMS_pdx == 1 & CDW_pdx == 0 ~ "Non-VA",
      TRUE ~ "Both"
    )
  )

log_count(cohort, "Stroke hospitalizations")
cat("Unique patients:", format(length(unique(cohort$scrssn)), big.mark = ","), "\n")

# Load RHF data
cat("\nLoading Residential History File (RHF) data...\n")
cat("Note: This is a large dataset and may take several minutes...\n")

RHFB <- tbl(con, in_schema(schema_dflt, 'RHFB_SHIP2393')) %>% 
  collect() %>%
  left_join(Groupings, by = c("hee_type1" = "type1"))

log_count(RHFB, "RHF records (all patients)")

# ==============================================================================
# MODULE 1: LINK RHF TO STROKE COHORT
# ==============================================================================

# Limit RHF to patients with stroke hospitalizations
cat("Filtering RHF to stroke cohort patients...\n")
RHFB_cohort <- RHFB %>%
  inner_join(
    cohort %>% transmute(scrssn) %>% distinct(), 
    by = "scrssn"
  )

log_count(RHFB_cohort, "RHF records for stroke patients")
cat("Unique patients:", format(length(unique(RHFB_cohort$scrssn)), big.mark = ","), "\n")

# Check grouping distribution
cat("\nRHF episode type distribution:\n")
print(table(RHFB_cohort$Grouping))

# Create ICU indicator from detailed type codes
RHFB_cohort <- RHFB_cohort %>%
  mutate(
    Grouping2 = if_else(
      str_detect(hee_type1, "ICU"), 
      "ICU", 
      "OTHER"
    )
  )

# Prepare RHF dataset with key variables
cat("Preparing RHF dataset for processing...\n")
RHFB_cohort <- RHFB_cohort %>% 
  transmute(
    scrssn, 
    hee_from, 
    hee_thru, 
    type1 = hee_type1, 
    Grouping, 
    Grouping2, 
    hes_dod,  # Date of death
    provnum = hee_provn1  # Provider number
  )

log_count(RHFB_cohort, "RHF records prepared for processing")

# ==============================================================================
# MODULE 2: REMOVE SMALL GAPS AND CLEAN EPISODE DATES
# ==============================================================================

# Handle date inconsistencies and death records
cat("Cleaning episode dates and handling death records...\n")
rhf_no_small_gap <- RHFB_cohort %>% 
  mutate(
    # Fix end dates that are before start dates
    hee_thru = if_else(hee_from > hee_thru, hee_from, hee_thru),
    # Handle death records (end date = start date)
    hee_thru = if_else(
      type1 %in% c("98. POST DEATH EXACT", "99. CLDT > DOD", 
                   "99. MULTIPLE DOD CSTAT ASSERTION"), 
      hee_from, 
      hee_thru
    ),
    Grouping = if_else(
      type1 %in% c("98. POST DEATH EXACT", "99. CLDT > DOD", 
                   "99. MULTIPLE DOD CSTAT ASSERTION"), 
      "DEATH", 
      Grouping
    ),
    los = as.numeric(hee_thru - hee_from, units = "days")
  )

# Convert to data.table for efficient processing
setDT(rhf_no_small_gap)

# Create lag/lead variables for episode sequencing
cat("Creating episode sequence variables...\n")
rhf_no_small_gap[, `:=` (
  # Previous episode
  bf_setting = shift(Grouping, type = "lag"),
  bf_hee_from = shift(hee_from, type = "lag"),
  bf_hee_thru = shift(hee_thru, type = "lag"),
  bf_provnum = shift(provnum, type = "lag"),
  # Next episode
  nx_setting = shift(Grouping, type = "lead"),
  nx_hee_from = shift(hee_from, type = "lead"),
  nx_hee_thru = shift(hee_thru, type = "lead"),
  nx_provnum = shift(provnum, type = "lead")
), by = .(scrssn)]

# Group consecutive episodes of same type
cat("Grouping consecutive episodes of same type...\n")
rhf_no_small_gap <- rhf_no_small_gap %>% 
  group_by(scrssn) %>% 
  arrange(scrssn, hee_from) %>%
  mutate(
    # Create run ID: consecutive rows with same Grouping and type1
    setting_run = data.table::rleid(Grouping, type1)
  ) %>% 
  ungroup()

# Create care location indicators
setDT(rhf_no_small_gap)
rhf_no_small_gap[, `:=` (
  icu = if_else(grepl("ICU", type1), 1, 0),
  VA = if_else(grepl("VA", type1), 1, 0),
  nonVA = if_else(grepl("VA", type1), 0, 1)
)]

# Roll up consecutive episodes of exact same type
cat("Rolling up consecutive same-type episodes...\n")
rhf_no_small_gap <- rhf_no_small_gap[, .(
  type = first(type1),
  Grouping = first(Grouping),
  icu = max(icu, na.rm = TRUE),
  VA = max(VA, na.rm = TRUE),
  nonVA = max(nonVA, na.rm = TRUE),
  records = .N,
  hee_from = min(hee_from, na.rm = TRUE),
  hee_thru = max(hee_thru, na.rm = TRUE)
), by = .(scrssn, setting_run)]

log_count(rhf_no_small_gap, "Episodes after initial rollup")

# Reclassify INP episodes
rhf_no_small_gap <- rhf_no_small_gap %>% 
  mutate(type = if_else(Grouping == "INP", "INP", type))

# Identify and remove small gaps
cat("Identifying episodes to remove (GAP ≤3 days, OTHER ≤3 days)...\n")
rhf_no_small_gap <- rhf_no_small_gap %>% 
  mutate(
    los = as.numeric(hee_thru - hee_from),
    rem = if_else(
      (Grouping == "GAP" & los <= max_gap_days) | 
      (Grouping == "OTHER" & los <= max_gap_days), 
      1L, 0L
    )
  )

cat("Episodes to remove:", sum(rhf_no_small_gap$rem), "\n")
rhf_no_small_gap <- rhf_no_small_gap %>% filter(rem == 0)

log_count(rhf_no_small_gap, "Episodes after removing small gaps")

# Save intermediate file
save_parquet_safe(rhf_no_small_gap, "rhf_no_small_gap.parquet")

# ==============================================================================
# MODULE 3: FINAL EPISODE ROLLUP AND GAP REMOVAL
# ==============================================================================

rhf_no_small_gap <- load_parquet_safe("rhf_no_small_gap.parquet")

# Roll up consecutive episodes of same Grouping (now that small gaps removed)
cat("Grouping consecutive episodes after gap removal...\n")
rhf_no_gap <- rhf_no_small_gap %>% 
  group_by(scrssn) %>% 
  arrange(scrssn, hee_from) %>% 
  mutate(setting_run = data.table::rleid(Grouping, type)) %>% 
  ungroup()

# Final rollup by setting_run
setDT(rhf_no_gap)
rhf_no_gap <- rhf_no_gap[, .(
  Grouping = first(Grouping),
  type = first(type),
  icu = max(icu, na.rm = TRUE),
  only_NonVA = min(nonVA, na.rm = TRUE),  # All episodes in setting were non-VA
  any_NonVA = max(nonVA, na.rm = TRUE),   # At least one non-VA episode
  only_VA = min(VA, na.rm = TRUE),        # All episodes in setting were VA
  any_VA = max(VA, na.rm = TRUE),         # At least one VA episode
  hee_from = min(hee_from, na.rm = TRUE),
  hee_thru = max(hee_thru, na.rm = TRUE)
), by = .(scrssn, setting_run)]

log_count(rhf_no_gap, "Episodes after final rollup")

# Remove all GAP and OTHER episodes
cat("Removing GAP and OTHER episodes...\n")
rhf_no_gap <- rhf_no_gap %>% 
  filter(Grouping != "GAP" & Grouping != "OTHER")

log_count(rhf_no_gap, "Final cleaned RHF episodes")

# Create next episode variables
setDT(rhf_no_gap)
rhf_no_gap <- rhf_no_gap[order(scrssn, hee_from)]
rhf_no_gap[, `:=` (
  nx_setting = shift(Grouping, type = "lead"),
  nx_hee_from = shift(hee_from, type = "lead"),
  nx_hee_thru = shift(hee_thru, type = "lead")
), by = scrssn]

# Calculate days to next setting
rhf_no_gap[, `:=` (
  days_to_nx_setting = as.numeric(nx_hee_from - hee_thru)
), by = scrssn]

# Save cleaned RHF
save_parquet_safe(rhf_no_gap, "rhf_no_gap.parquet")

# ==============================================================================
# MODULE 4: IDENTIFY ELIGIBLE POST-ACUTE CARE SEQUENCES
# ==============================================================================

rhf_no_gap <- load_parquet_safe("rhf_no_gap.parquet")

cat("Unique patients in cleaned RHF:", 
    format(length(unique(rhf_no_gap$scrssn)), big.mark = ","), "\n")

cat("Inpatient episodes:", 
    format(nrow(rhf_no_gap %>% filter(Grouping == "INP") %>% distinct(scrssn, hee_from)), 
           big.mark = ","), "\n")

# Apply discharge timing rules
# IRF/SNF: must start within 3 days of acute discharge
# HH: must start within 14 days of acute discharge
cat("\nApplying post-acute care discharge timing rules...\n")
cat(sprintf("  IRF/SNF: ≤%d days after acute discharge\n", max_days_to_snf))
cat(sprintf("  Home Health: ≤%d days after acute discharge\n", max_days_to_hh))

rhf_inp_discharge_to_snf <- rhf_no_gap %>% 
  filter(Grouping == "INP" & 
         days_to_nx_setting <= max_days_to_snf & 
         nx_setting == "SNF")

rhf_inp_discharge_to_irf <- rhf_no_gap %>% 
  filter(Grouping == "INP" & 
         days_to_nx_setting <= max_days_to_irf & 
         nx_setting == "IRF")

rhf_inp_discharge_to_hh <- rhf_no_gap %>% 
  filter(Grouping == "INP" & 
         days_to_nx_setting <= max_days_to_hh & 
         nx_setting == "HH")

cat("INP → SNF:", format(nrow(rhf_inp_discharge_to_snf), big.mark = ","), "\n")
cat("INP → IRF:", format(nrow(rhf_inp_discharge_to_irf), big.mark = ","), "\n")
cat("INP → HH:", format(nrow(rhf_inp_discharge_to_hh), big.mark = ","), "\n")

# Create list of eligible discharge dates (inpatient end dates)
included_list <- rbind(
  rhf_inp_discharge_to_snf, 
  rhf_inp_discharge_to_irf, 
  rhf_inp_discharge_to_hh
) %>% 
  select(scrssn, hee_thru) %>% 
  distinct()

log_count(included_list, "Eligible inpatient discharge dates")

# ==============================================================================
# MODULE 5: CREATE EPISODE SEQUENCES AND TERMINATION RULES
# ==============================================================================

# Create inpatient discharge date tracker
cat("Creating episode sequences indexed by inpatient discharge...\n")
rhf_index <- rhf_no_gap %>% 
  arrange(row_number()) %>% 
  mutate(
    # Create helper variable for last inpatient discharge
    INP_hee_helper = if_else(Grouping == "INP", as.Date(hee_thru), NA),
    # Use na.locf to carry forward last inpatient discharge date
    INP_hee_thru = lag(zoo::na.locf(INP_hee_helper, na.rm = FALSE)),
    INP_hee_thru = if_else(!is.na(INP_hee_helper), INP_hee_helper, INP_hee_thru)
  )

log_count(rhf_index, "Episodes indexed by inpatient discharge")

# Apply post-acute care termination rules
cat("Applying post-acute care termination rules...\n")
cat("Terminal episode if ANY of the following:\n")
cat("  1. Next setting is not SNF, IRF, or HH\n")
cat("  2. Gap to next episode >14 days\n")
cat("  3. Next SNF/IRF admission >3 days after discharge\n")
cat("  4. Readmitted to inpatient within 1 day\n\n")

rhf_index <- rhf_index %>% 
  select(scrssn, INP_hee_thru, Grouping, type, icu, any_NonVA, only_NonVA, 
         only_VA, any_VA, hee_from, hee_thru, nx_setting, days_to_nx_setting) %>% 
  mutate(
    nx_setting = replace_na(nx_setting, "END"),
    days_to_nx_setting = replace_na(days_to_nx_setting, 9999),
    # Termination criteria
    readmitted = if_else(nx_setting == "INP" & days_to_nx_setting <= 1, 1, 0),
    nx_not_SNF_IRF_HH = if_else(nx_setting %!in% c("SNF", "IRF", "HH"), 1, 0),
    nx_over_14 = if_else(days_to_nx_setting > 14, 1, 0),
    nx_SNF_IRF_1 = if_else(
      Grouping %in% c("HH", "SNF", "IRF") & 
      nx_setting %in% c("SNF", "IRF") & 
      days_to_nx_setting > 1, 
      1, 0
    ),
    nx_SNF_IRF_3 = if_else(
      Grouping %in% c("HH", "SNF", "IRF") & 
      nx_setting %in% c("SNF", "IRF") & 
      days_to_nx_setting > 3, 
      1, 0
    ),
    # Terminal indicators (3-day and 1-day thresholds)
    terminal3 = if_else(
      nx_not_SNF_IRF_HH == 1 | nx_over_14 == 1 | nx_SNF_IRF_3 == 1, 
      1, 0
    ),
    terminal1 = if_else(
      nx_not_SNF_IRF_HH == 1 | nx_over_14 == 1 | nx_SNF_IRF_1 == 1, 
      1, 0
    )
  )

log_count(rhf_index, "Episodes with termination indicators")

# Find first terminal episode for each post-acute care sequence
cat("Identifying first terminal episode per sequence...\n")
rhf_index_terminal_date <- rhf_index %>% 
  filter(terminal3 == 1) %>% 
  group_by(scrssn, INP_hee_thru) %>% 
  arrange(hee_from) %>% 
  slice_head(n = 1) %>% 
  transmute(scrssn, INP_hee_thru, care_thru = hee_thru) %>% 
  ungroup()

log_count(rhf_index_terminal_date, "Terminal episode dates")

# Limit episodes to those up to and including terminal episode
cat("Limiting to episodes through terminal point...\n")
rhf_index_terminal <- rhf_index %>% 
  left_join(rhf_index_terminal_date, by = c("scrssn", "INP_hee_thru")) %>% 
  filter(care_thru >= hee_thru) %>% 
  group_by(scrssn, INP_hee_thru) %>% 
  mutate(readmitted = max(readmitted, na.rm = TRUE)) %>% 
  ungroup()

log_count(rhf_index_terminal, "Episodes through terminal point")

# Extract inpatient index episodes
rhf_INP_index_terminal <- rhf_index_terminal %>% 
  filter(Grouping == "INP") %>% 
  select(scrssn, hee_from, INP_hee_thru) %>% 
  distinct()

log_count(rhf_INP_index_terminal, "Inpatient index episodes")
cat("Unique patients:", 
    format(length(unique(rhf_INP_index_terminal$scrssn)), big.mark = ","), "\n")

# ==============================================================================
# MODULE 6: MATCH RHF TO CDW/CMS STROKE HOSPITALIZATIONS
# ==============================================================================

# Filter cohort to patients in RHF
rhf_cohort <- cohort %>% 
  filter(scrssn %in% RHFB_cohort$scrssn)

cat("Stroke hospitalizations for patients in RHF:\n")
cat("  Unique hospitalizations:", 
    format(nrow(rhf_cohort %>% distinct(scrssn, ADMSNDT)), big.mark = ","), "\n")
cat("  Unique patients:", 
    format(nrow(rhf_cohort %>% distinct(scrssn)), big.mark = ","), "\n\n")

# Match RHF inpatient episodes to stroke hospitalizations
# Matching rule: RHF episode overlaps with or is within 1 day of stroke admission
cat("Matching RHF inpatient episodes to stroke hospitalizations...\n")
cat("Matching rule: RHF episode overlaps with stroke admission ±1 day\n\n")

rhf_cohort <- rhf_cohort %>%
  inner_join(rhf_INP_index_terminal, by = "scrssn") %>% 
  mutate(
    # Calculate overlap between RHF and CDW/CMS inpatient episodes
    overlap_bf = pmax(
      0, 
      as.numeric(pmin(DSCHRGDT, INP_hee_thru, na.rm = TRUE) - 
                 pmax(ADMSNDT, hee_from, na.rm = TRUE) + 1)
    ),
    # Calculate days from stroke hospitalization
    days_from_stroke_stay = case_when(
      INP_hee_thru < ADMSNDT ~ as.numeric(INP_hee_thru - ADMSNDT),
      hee_from > DSCHRGDT ~ as.numeric(hee_from - DSCHRGDT),
      TRUE ~ 0
    )
  ) %>% 
  # Keep only matches within ±1 day
  filter(days_from_stroke_stay <= 1 & days_from_stroke_stay > -1) %>% 
  select(PatientICN, PERSON_ID, scrssn, ADMSNDT, DSCHRGDT, 
         setting, ICD10, INP_hee_thru) %>% 
  distinct()

log_count(rhf_cohort, "Matched stroke hospitalizations")
cat("Unique hospitalizations:", 
    format(nrow(rhf_cohort %>% distinct(scrssn, ADMSNDT)), big.mark = ","), "\n")
cat("Unique patients:", 
    format(nrow(rhf_cohort %>% distinct(scrssn)), big.mark = ","), "\n\n")

# Limit to eligible post-acute care sequences
cat("Limiting to eligible post-acute care sequences...\n")
rhf_cohort_included <- rhf_cohort %>%
  inner_join(included_list, by = c("scrssn", "INP_hee_thru" = "hee_thru"))

log_count(rhf_cohort_included, "Stroke hospitalizations with post-acute care")
cat("Unique hospitalizations:", 
    format(nrow(rhf_cohort_included %>% distinct(scrssn, ADMSNDT)), big.mark = ","), "\n")
cat("Unique patients:", 
    format(nrow(rhf_cohort_included %>% distinct(scrssn)), big.mark = ","), "\n")

# Save eligible cohort
save_parquet_safe(rhf_cohort_included, "rhf_cohort_included.parquet")

# ==============================================================================
# MODULE 7: DEDUPLICATE AND CREATE FINAL DATASET
# ==============================================================================

# Join full episode sequences to eligible hospitalizations
cat("Creating episode-level dataset...\n")
rhf_index_terminal_de_dup <- rhf_index_terminal %>% 
  inner_join(rhf_cohort_included, by = c("scrssn", "INP_hee_thru")) %>%
  select(PatientICN, scrssn, PERSON_ID, ADMSNDT, DSCHRGDT, ICD10, 
         INP_hee_thru, icu, any_NonVA, only_NonVA, only_VA, any_VA, 
         Grouping, type, hee_from, hee_thru, setting, nx_setting, 
         days_to_nx_setting, care_thru, terminal1, terminal3, readmitted) %>% 
  mutate(los = as.numeric(hee_thru - hee_from))

log_count(rhf_index_terminal_de_dup, "Episode-level records")

# Deduplicate: if RHF episode matches multiple stroke hospitalizations, 
# keep the one with ADMSNDT closest to post-acute care start
cat("Deduplicating multiple matches (keeping closest admission date)...\n")
rhf_index_included_terminal_elig_rollup <- rhf_index_terminal_de_dup %>%
  arrange(PatientICN, INP_hee_thru, hee_thru, desc(ADMSNDT)) %>% 
  group_by(PatientICN, INP_hee_thru, setting, Grouping, type, hee_from, hee_thru) %>% 
  slice_head(n = 1) %>% 
  ungroup()

log_count(rhf_index_included_terminal_elig_rollup, "Deduplicated episodes")

# Roll up episodes by care setting
cat("Rolling up consecutive episodes by care setting...\n")
rhf_post_acute_final <- rhf_index_included_terminal_elig_rollup %>% 
  group_by(scrssn, ADMSNDT, INP_hee_thru, setting) %>% 
  arrange(scrssn, ADMSNDT, INP_hee_thru, hee_from) %>% 
  mutate(
    episode_run = data.table::rleid(Grouping),
    setting_run = data.table::rleid(Grouping, type)
  ) %>% 
  ungroup()

# Final rollup by episode_run
setDT(rhf_post_acute_final)
rhf_post_acute_final <- rhf_post_acute_final[, .(
  Grouping = first(Grouping),
  transfers = max(setting_run) - 1,  # Number of facility transfers
  settings = list(unique(type)),     # List of specific setting types
  icu = max(icu, na.rm = TRUE),
  readmitted = max(readmitted),
  inpatient_setting = first(setting),
  only_NonVA = min(only_NonVA, na.rm = TRUE), 
  any_NonVA = max(any_NonVA, na.rm = TRUE), 
  only_VA = min(only_VA, na.rm = TRUE),
  any_VA = max(any_VA, na.rm = TRUE),
  hee_from = min(hee_from, na.rm = TRUE),
  hee_thru = max(hee_thru, na.rm = TRUE),
  nx_Grouping = last(nx_setting),
  days_to_nx_Grouping = last(days_to_nx_setting),
  los = sum(los)
), by = .(PatientICN, PERSON_ID, scrssn, ADMSNDT, DSCHRGDT, ICD10, 
          INP_hee_thru, episode_run)]

rhf_post_acute_final <- rhf_post_acute_final %>% distinct()

log_count(rhf_post_acute_final, "Final post-acute care episodes")

# Save final dataset
save_parquet_safe(rhf_post_acute_final, "rhf_post_acute_final.parquet")

# ==============================================================================
# MODULE 8: EXTRACT FIRST POST-ACUTE CARE EPISODE
# ==============================================================================

rhf_post_acute_final <- load_parquet_safe("rhf_post_acute_final.parquet")

cat("Final episode counts:\n")
cat("  Unique stroke hospitalizations:", 
    format(nrow(rhf_post_acute_final %>% 
           select(scrssn, ADMSNDT, INP_hee_thru) %>% distinct()), 
           big.mark = ","), "\n")
cat("  Unique patients:", 
    format(length(unique(rhf_post_acute_final$scrssn)), big.mark = ","), "\n\n")

# Extract first post-acute care episode per stroke hospitalization
cat("Extracting first post-acute care episode per hospitalization...\n")
dt_first_post_acute <- rhf_post_acute_final %>%  
  group_by(scrssn, ADMSNDT, INP_hee_thru) %>%
  arrange(scrssn, ADMSNDT, INP_hee_thru, episode_run) %>% 
  # Episode 1 is INP, episode 2 is first post-acute
  slice(2) %>% 
  ungroup()

# Distribution of first discharge location
cat("\nFirst post-acute care setting distribution:\n")
first_discharge_table <- dt_first_post_acute %>% 
  transmute(scrssn, ADMSNDT, INP_hee_thru, Grouping) %>% 
  distinct() %>% 
  select(Grouping) %>% 
  table() %>% 
  as.data.frame()

print(first_discharge_table)

# Add initial discharge variable to full dataset
rhf_post_acute_final <- rhf_post_acute_final %>%
  group_by(scrssn, ADMSNDT, INP_hee_thru) %>%
  mutate(
    initial_discharge = nth(Grouping, 2),
    # Create sequence of all settings
    setting_sequence = paste(Grouping, collapse = ",")
  ) %>%
  ungroup()

# Save first episode dataset
dt_first_post_acute_elig <- dt_first_post_acute %>%
  mutate(
    initial_discharge = Grouping,
    total_pac_episodes = 1
  )

save_parquet_safe(dt_first_post_acute_elig, "dt_first_post_acute_elig.parquet")
save_parquet_safe(rhf_post_acute_final, "rhf_post_acute_final.parquet")

# ==============================================================================
# DATA VALIDATION AND SUMMARY STATISTICS
# ==============================================================================

# Check for duplicates
cat("\nChecking for duplicate hospitalizations...\n")
dt_first_post_acute_dup <- rhf_post_acute_final %>%  
  select(scrssn, ADMSNDT, INP_hee_thru) %>%
  distinct() %>%
  group_by(scrssn, ADMSNDT) %>%
  summarize(n_chk = n(), .groups = 'drop') %>%
  filter(n_chk > 1)

if (nrow(dt_first_post_acute_dup) > 0) {
  warning(sprintf("%d stroke hospitalizations matched to multiple RHF sequences", 
                  nrow(dt_first_post_acute_dup)))
}

# Multiple care settings analysis
cat("\nAnalyzing use of multiple care settings...\n")
rhf_setting_grid <- rhf_post_acute_final %>% 
  transmute(
    scrssn, 
    ADMSNDT, 
    IRF = if_else(Grouping == "IRF", 1, 0),
    SNF = if_else(Grouping == "SNF", 1, 0),
    HH = if_else(Grouping == "HH", 1, 0)
  ) %>% 
  group_by(scrssn, ADMSNDT) %>% 
  summarize(
    IRF = max(IRF), 
    SNF = max(SNF), 
    HH = max(HH),
    .groups = 'drop'
  ) %>% 
  mutate(
    year = year(ADMSNDT),
    multi = if_else(IRF + SNF + HH > 1, 1, 0),
    IRF_SNF = if_else(IRF + SNF > 1, 1, 0),
    IRF_HH = if_else(IRF + HH > 1, 1, 0),
    SNF_HH = if_else(SNF + HH > 1, 1, 0)
  )

cat("\nMultiple care setting use by year:\n")
multi_setting_summary <- rhf_setting_grid %>% 
  group_by(year) %>% 
  summarize(
    multi = mean(multi),
    IRF_SNF = mean(IRF_SNF),
    IRF_HH = mean(IRF_HH),
    SNF_HH = mean(SNF_HH)
  )

print(multi_setting_summary)

# Episode length of stay summary
cat("\nLength of stay summary by setting:\n")
los_summary <- rhf_post_acute_final %>%
  group_by(Grouping) %>%
  summarize(
    n = n(),
    mean_los = mean(los, na.rm = TRUE),
    median_los = median(los, na.rm = TRUE),
    min_los = min(los, na.rm = TRUE),
    max_los = max(los, na.rm = TRUE)
  )

print(los_summary)

# VA vs non-VA care
cat("\nVA vs non-VA care distribution:\n")
va_summary <- rhf_post_acute_final %>%
  group_by(Grouping) %>%
  summarize(
    only_VA = sum(only_VA == 1),
    any_VA = sum(any_VA == 1),
    only_NonVA = sum(only_NonVA == 1),
    any_NonVA = sum(any_NonVA == 1),
    .groups = 'drop'
  )

print(va_summary)

# ==============================================================================
# SESSION INFO
# ==============================================================================

# SCRIPT 02A COMPLETED SUCCESSFULLY

cat("\nKey Output Files:\n")
cat("  - rhf_no_gap.parquet: Cleaned RHF episodes\n")
cat("  - rhf_cohort_included.parquet: Stroke admissions with post-acute care\n")
cat("  - rhf_post_acute_final.parquet: Final post-acute care episodes\n")
cat("  - dt_first_post_acute_elig.parquet: First post-acute episode per stroke\n\n")

cat("Summary Statistics:\n")
cat("  Stroke hospitalizations with post-acute care:", 
    format(nrow(rhf_cohort_included %>% distinct(scrssn, ADMSNDT)), big.mark = ","), "\n")
cat("  Unique patients:", 
    format(nrow(rhf_cohort_included %>% distinct(scrssn)), big.mark = ","), "\n")
cat("  Total post-acute care episodes:", 
    format(nrow(rhf_post_acute_final), big.mark = ","), "\n")
cat("  First discharge to IRF:", 
    sum(dt_first_post_acute$Grouping == "IRF"), "\n")
cat("  First discharge to SNF:", 
    sum(dt_first_post_acute$Grouping == "SNF"), "\n")
cat("  First discharge to HH:", 
    sum(dt_first_post_acute$Grouping == "HH"), "\n")

# Clean up
dbDisconnect(con)

################################################################################
# END OF SCRIPT
################################################################################
