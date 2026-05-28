################################################################################
# SCRIPT 05B: MATCH GG FUNCTIONAL ASSESSMENTS TO POST-ACUTE CARE EPISODES
# Stroke Rehabilitation Research - CDA Study
# ORD Waddell 202408036D
################################################################################
#
# Purpose:
#   Match GG functional assessment records from MDS, OASIS, and IRF-PAI to
#   post-acute care episodes from RHF. Apply sophisticated matching algorithms
#   to identify best admission and discharge assessments for each episode.
#   Create final analytical dataset with functional outcomes linked to episodes.
#
# Author: Nick Cardamone
# Created: 2025-06-10
# Last Modified: 2025-12-11
# Version: 10.0
#
# Processing Steps:
#   1. Load and prepare GG assessment data (scale-level)
#   2. Handle multiple assessments per date (select best)
#   3. Convert to wide format (admission, discharge, follow-up)
#   4. Load post-acute care episodes from RHF
#   5. Match episodes to assessments by setting and timing
#   6. Apply episode-level exclusion criteria
#   7. Select best admission assessment (within 2 weeks)
#   8. Select best discharge assessment (within 1 week before/2 weeks after)
#   9. Apply final exclusions (VA facilities, missing data)
#   10. Create analytical dataset with matched assessments
#
# Matching Rules:
#   - SNF episodes → MDS assessments
#   - HH episodes → OASIS assessments
#   - IRF episodes → IRF-PAI assessments
#   - Assessment admit date must be ≤1 day before or any day after episode start
#   - Assessment admit date must be before end of post-acute care sequence
#
# Admission Assessment Selection (if multiple candidates):
#   1. Within 2 weeks of episode start
#   2. Has non-missing GG scores
#   3. Most non-missing items
#   4. Most valid scores (01-06)
#   5. Closest to admission date
#
# Discharge Assessment Selection (if multiple candidates):
#   1. Within 7 days before discharge
#   2. If none, within 2 weeks after discharge
#   3. Has non-missing GG scores
#   4. Most non-missing items
#   5. Most valid scores (01-06)
#   6. Closest to discharge date
#
# Exclusions (Hospitalization Level):
#   - Readmitted to acute care within 1 day of post-acute discharge
#   - Died within 1 day of post-acute discharge
#   - Marked as died on any assessment
#   - Marked as unplanned discharge on any assessment
#   - Episode ≤3 days (too short for rehabilitation)
#   - Age >99 or <65
#   - Acute hospitalization >365 days
#
# Exclusions (Episode Level 1 ):
#   - No matched assessment record (no GG data available)
#   - VA SNF with no external assessment data
#   - VA HH (purchased home health only)
#   - VA IRF with no assessment data
#
# Exclusions (Episode Level 2):
#   - No GG items with a valid score 01, 02, 03, etc.
#
# Inputs:
#   - gg_complete_scale.parquet (from Script 05a)
#   - gg_meta.parquet (from Script 05a)
#   - dt_post_acute_elig.parquet (from Script 04a)
#
# Outputs:
#   - elig_rhf_data_assessment_2.parquet: All episode-assessment matches
#   - elig_rhf_data_assessment_final.parquet: Best admission/discharge per episode
#   - elig_rhf_data_assessment_final_epis_excl.parquet: Final analytical dataset
#   - gg_complete_item.parquet: Item-level data for visualization
#
# Expected Results:
#   - ~98K episodes matched to assessments
#   - ~64K episodes after exclusions (final analytical sample)
#
################################################################################

# ==============================================================================
# SETUP
# ==============================================================================

# Source configuration
source("00_config.R")

# Load required packages
load_packages(c("dplyr", "dbplyr", "DBI", "VINCI", "arrow", "data.table",
                "table1", "stringr", "tidyr", "ggplot2", "scales", "tidytable"))

cat("\n=============================================================================\n")
cat("SCRIPT 05B: MATCH GG ASSESSMENTS TO POST-ACUTE CARE EPISODES\n")
cat("=============================================================================\n\n")

# Set working directory
setwd(project_base)

# ==============================================================================
# MODULE 1: LOAD AND PREPARE GG ASSESSMENT DATA
# ==============================================================================

cat("\n--- MODULE 1: Load and Prepare GG Assessment Data ---\n")

cat("Loading GG scale-level assessment data...\n")
assessment_data_raw <- load_parquet_safe("gg_complete_scale.parquet") %>%
  transmute(
    record_id,
    SCRSSN,
    source,
    time,
    admit_date,
    assessment_date,
    assessment_occur,
    count_not_na,
    count_valid,
    count_valid_ALL,
    GG_total
  )

log_count(assessment_data_raw, "GG assessments (raw)")

# Handle multiple assessments on same date
cat("\nHandling multiple assessments on same date...\n")
cat("Selection criteria (in order):\n")
cat("  1. Most non-missing items\n")
cat("  2. Most valid scores (01-06)\n")
cat("  3. Highest total GG score\n\n")

assessment_data <- assessment_data_raw %>%
  arrange(SCRSSN, source, admit_date, assessment_date, time, 
          desc(count_not_na), desc(count_valid), desc(GG_total)) %>% 
  group_by(SCRSSN, source, admit_date, assessment_date, time) %>%
  slice_head(n = 1) %>%
  ungroup()

log_count(assessment_data, "Assessments after deduplication")

# ==============================================================================
# MODULE 2: FILTER ASSESSMENTS BY TIMING
# ==============================================================================

cat("\n--- MODULE 2: Filter Assessments by Timing ---\n")

cat("Filtering OASIS assessments to appropriate timing codes...\n")
cat("  Admission: 01, 02, 03 (Start of care, Resumption)\n")
cat("  Follow-up: 04, 05\n")
cat("  Discharge: 06, 07, 09, 10 (Transfer, Discharge)\n\n")

setDT(assessment_data)
assessment_data_admit <- assessment_data[
  !(source == "OASIS" & 
    ((time == "ADMSN" & assessment_occur %!in% c("01", "02", "03")) |
     (time == "FLWP" & assessment_occur %!in% c("04", "05")) |
     (time == "DSCHRG" & assessment_occur %!in% c("06", "07", "09", "10"))))
]

log_count(assessment_data_admit, "Assessments after timing filter")

# Handle missing admission dates
cat("Filling missing admission dates with assessment dates...\n")
assessment_data_admit <- assessment_data_admit %>% 
  mutate(admit_date = if_else(is.na(admit_date), assessment_date, admit_date))

# Number multiple assessments per admission episode
cat("Numbering multiple assessments per admission episode...\n")
assessment_data_admit <- assessment_data_admit %>% 
  arrange(SCRSSN, source, admit_date, time, assessment_date)

setDT(assessment_data_admit)
assessment_data_admit[, num := seq_len(.N), by = .(SCRSSN, source, admit_date, time)]

log_count(assessment_data_admit, "Assessments with sequence numbers")

# ==============================================================================
# MODULE 3: CONVERT TO WIDE FORMAT
# ==============================================================================

cat("\n--- MODULE 3: Convert to Wide Format ---\n")

cat("Pivoting to wide format (one row per admission episode)...\n")
cat("Columns created for each timing point (ADMSN, FLWP, DSCHRG)...\n\n")

assessment_data_wide <- assessment_data_admit %>% 
  pivot_wider(
    names_from = c("time", "num"),
    values_from = c(
      "record_id",
      "assessment_date",
      "assessment_occur",
      "GG_total",
      "count_not_na",
      "count_valid",
      "count_valid_ALL"
    )
  )

log_count(assessment_data_wide, "Admission episodes in wide format")

# ==============================================================================
# MODULE 4: LOAD POST-ACUTE CARE EPISODES
# ==============================================================================

cat("\n--- MODULE 4: Load Post-Acute Care Episodes ---\n")

cat("Loading eligible post-acute care episodes...\n")
rhf_data <- load_parquet_safe("dt_post_acute_elig.parquet") %>%
  transmute(
    Grouping, 
    SCRSSN = scrssn, 
    ADMSNDT, 
    hee_from, 
    hee_thru,
    los_censored,
    care_thru
  ) %>%
  distinct()

log_count(rhf_data, "Post-acute care episodes")

# ==============================================================================
# MODULE 5: MATCH EPISODES TO ASSESSMENTS
# ==============================================================================

cat("\n--- MODULE 5: Match Episodes to Assessments ---\n")

cat("Matching rules:\n")
cat("  SNF episodes → MDS assessments\n")
cat("  HH episodes → OASIS assessments\n")
cat("  IRF episodes → IRF-PAI assessments\n")
cat("  Assessment admit date ≥ -1 day from episode start\n")
cat("  Assessment admit date ≤ end of post-acute care\n\n")

complete_match <- assessment_data_wide %>%
  inner_join(rhf_data, by = "SCRSSN") %>%
  mutate(
    setting_match = case_when(
      Grouping == "SNF" & source == "MDS" ~ 1,
      Grouping == "HH" & source == "OASIS" ~ 1,
      Grouping == "IRF" & source == "IRFPAI" ~ 1,
      TRUE ~ 0
    )
  ) %>%
  filter(setting_match == 1) %>%
  mutate(assessadmit_minus_rhfadmit = admit_date - hee_from)

log_count(complete_match, "Episode-assessment matches (all)")

# Select best match per episode
cat("\nSelecting best assessment match per episode...\n")
cat("Criteria:\n")
cat("  1. Admit date within 1 day before or after episode start\n")
cat("  2. Admit date before end of post-acute care sequence\n")
cat("  3. Smallest time difference between dates\n\n")

complete_match_best <- complete_match %>%
  filter(admit_date <= care_thru) %>%
  filter(assessadmit_minus_rhfadmit >= -1) %>%
  mutate(abs_diff = abs(assessadmit_minus_rhfadmit)) %>%
  group_by(SCRSSN, admit_date) %>%
  arrange(abs_diff) %>%
  slice_head(n = 1) %>%
  ungroup()

log_count(complete_match_best, "Best assessment matches")

# Select relevant variables
complete_match_best <- complete_match_best %>% 
  select(
    SCRSSN, 
    ADMSNDT,
    hee_from, 
    admit_date, 
    days_diff = assessadmit_minus_rhfadmit,
    record_id_ADMSN_1:count_valid_ALL_FLWP_24
  )

# ==============================================================================
# MODULE 6: JOIN TO FULL EPISODE DATA
# ==============================================================================

cat("\n--- MODULE 6: Join to Full Episode Data ---\n")

cat("Loading full eligible episode data...\n")
elig_rhf <- load_parquet_safe("dt_post_acute_elig.parquet") %>%
  mutate(setting_series = map_chr(settings, ~paste(.x, collapse = "|")))

log_count(elig_rhf, "Episodes (full data)")

# Load GG metadata
cat("Loading GG assessment metadata...\n")
gg_meta <- load_parquet_safe("gg_meta.parquet")

gg_meta <- gg_meta %>% 
  mutate(prior_cognition_val = prior_cognition,
         prior_self_care_val = prior_self_care,
         prior_mobility_val = prior_mobility,
         prior_cognition_score = as.numeric(if_else(prior_cognition_val %in% c("8", "9"), "1", prior_cognition_val)),
         prior_self_care_score = as.numeric(if_else(prior_self_care_val %in% c("8", "9"), "1", prior_self_care_val)),
         prior_mobility_score = as.numeric(if_else(prior_mobility_val %in% c("8", "9"), "1", prior_mobility)))

# Join assessment data to episodes
cat("\nJoining assessment data to episodes...\n")
elig_rhf_data_assessment <- elig_rhf %>%
  transmute(
    SCRSSN = scrssn, 
    ADMSNDT, 
    INP_hee_thru, 
    hee_from, 
    hee_thru, 
    Grouping, 
    setting_series, 
    only_VA, 
    episode_run, 
    los, 
    los_censored, 
    nx_Grouping, 
    days_to_nx_Grouping, 
    readmitted, 
    Age_ADMSNDT
  ) %>%
  left_join(complete_match_best, by = c("SCRSSN", "ADMSNDT", "hee_from")) %>% 
  mutate(
    # VA facility flags (different exclusion rules apply)
    SNF_VA_flag = if_else(Grouping == "SNF" & only_VA == 1, 1, 0),
    HH_VA_flag = if_else(
      Grouping == "HH" & setting_series == "10. PCS VA CDS GEC PSHC", 
      1, 0
    ),
    IRF_VA_flag = if_else(Grouping == "IRF" & only_VA == 1, 1, 0),
    # Check if any assessment has data
    has_data = if_else(
      !is.na(count_valid_ALL_ADMSN_1) & count_valid_ALL_ADMSN_1 > 0 | 
      !is.na(count_not_na_DSCHRG_1) & count_valid_ALL_DSCHRG_1 > 0 | 
      !is.na(count_not_na_FLWP_1) & count_valid_ALL_FLWP_1 > 0 , 
      1, 0
    )
  )

log_count(elig_rhf_data_assessment, "Episodes with assessment matches")

# ==============================================================================
# MODULE 7: CONVERT TO LONG FORMAT AND JOIN METADATA
# ==============================================================================

cat("\n--- MODULE 7: Convert to Long Format and Join Metadata ---\n")

cat("Pivoting assessment data to long format...\n")
elig_rhf_data_assessment <- elig_rhf_data_assessment %>% 
  tidyr::pivot_longer(
    cols = c(record_id_ADMSN_1:count_valid_ALL_FLWP_24),
    names_to = c(".value", "time", "num"),
    names_pattern = "(.+)_(ADMSN|FLWP|DSCHRG)_(\\d+)"
  )

cat("Joining assessment metadata (death, unplanned discharge)...\n")
elig_rhf_data_assessment <- elig_rhf_data_assessment %>%
  left_join(
    gg_meta %>% transmute(
      SCRSSN, 
      record_id, 
      exit_date, 
      admit_rec, 
      reentry_rec, 
      followup_rec, 
      exit_rec, 
      discharge_unpl, 
      died,
      prior_cognition_score,
      prior_self_care_score,
      prior_mobility_score
    ), 
    by = c("SCRSSN", "record_id")
  )

log_count(elig_rhf_data_assessment, "Episodes with metadata")

# Propagate death and unplanned discharge flags
cat("\nPropagating death and unplanned discharge indicators...\n")
cat("Rule: If ANY assessment indicates death/unplanned discharge,\n")
cat("      apply to ALL episodes from that hospitalization\n\n")

elig_rhf_data_assessment <- elig_rhf_data_assessment %>%
  mutate(
    has_data = coalesce(has_data, 0),
    died = coalesce(died, 0),
    discharge_unpl = coalesce(discharge_unpl, 0),
    prior_cognition_score = coalesce(prior_cognition_score, 0),
    prior_self_care_score = coalesce(prior_self_care_score, 0),
    prior_mobility_score = coalesce(prior_mobility_score, 0)
  ) %>%
  group_by(SCRSSN, ADMSNDT) %>% 
  mutate(
    died = max(died),
    discharge_unpl = max(discharge_unpl)
  ) %>%
  group_by(SCRSSN, ADMSNDT, hee_from) %>% 
  mutate(has_data = max(has_data)) %>%
  ungroup()

# Ensure that we take the highest score prior to admission for that Grouping:

elig_rhf_data_assessment <- elig_rhf_data_assessment %>%
  group_by(SCRSSN, ADMSNDT, admit_date,Grouping) %>%
  mutate(
    prior_cognition_score = max(prior_cognition_score),
    prior_self_care_score = max(prior_self_care_score),
    prior_mobility_score = max(prior_mobility_score)
  ) %>%
  ungroup()
# ==============================================================================
# MODULE 8: HANDLE HOME HEALTH FOLLOW-UP DISCHARGE DATES
# ==============================================================================

cat("\n--- MODULE 8: Handle Home Health Follow-up Discharge Dates ---\n")

cat("Assigning discharge dates to HH follow-up assessments...\n")
cat("Rule: Use nearest DSCHRG record exit date (forward then backward)\n\n")

elig_rhf_data_assessment_2 <- elig_rhf_data_assessment %>%
  mutate(
    exit_date = if_else(
      followup_rec == 1 & Grouping == "HH", 
      NA, 
      exit_date
    )
  ) %>%
  arrange(SCRSSN, ADMSNDT, hee_from, admit_date, assessment_date) %>%
  group_by(SCRSSN, ADMSNDT, hee_from, admit_date) %>%
  tidyr::fill(exit_date, .direction = "updown") %>%
  ungroup() %>%
  mutate(
    exit_date = if_else(
      is.na(exit_date) & time == "FLWP", 
      assessment_date, 
      exit_date
    )
  ) %>%
  arrange(SCRSSN, ADMSNDT, hee_from, admit_date, assessment_date) %>%
  group_by(SCRSSN, ADMSNDT, hee_from, admit_date) %>%
  tidyr::fill(exit_date, .direction = "updown") %>%
  ungroup()

# Calculate length of stay discrepancy
cat("Calculating RHF vs assessment length of stay discrepancy...\n")
elig_rhf_data_assessment_2 <- elig_rhf_data_assessment_2 %>%
  mutate(
    los_assess = as.numeric(exit_date - admit_date),
    discrep = abs(los - los_assess)
  ) %>%
  select(
    SCRSSN, ADMSNDT, hee_from, hee_thru, INP_hee_thru, admit_date, 
    exit_date, days_diff, los, los_censored, los_assess, discrep, 
    Grouping, setting_series, episode_run, Age_ADMSNDT,
    nx_Grouping, days_to_nx_Grouping, readmitted, died, discharge_unpl, SNF_VA_flag, 
    HH_VA_flag, IRF_VA_flag, time, num, record_id, assessment_date,
    prior_cognition_score, prior_self_care_score, prior_mobility_score,
    assessment_occur, admit_rec, reentry_rec, followup_rec, exit_rec, 
    GG_total, count_not_na, count_valid, count_valid_ALL,
  )

save_parquet_safe(elig_rhf_data_assessment_2, 
                  "elig_rhf_data_assessment_2.parquet")

log_count(elig_rhf_data_assessment_2, "Episodes with assessment metadata")

# ==============================================================================
# MODULE 9: SELECT BEST ADMISSION ASSESSMENT
# ==============================================================================

cat("\n--- MODULE 9: Select Best Admission Assessment ---\n")

elig_rhf_data_assessment_2 <- load_parquet_safe(
  "elig_rhf_data_assessment_2.parquet"
)

cat("Creating timing indicators for assessment selection...\n")
elig_rhf_data_assessment_2 <- elig_rhf_data_assessment_2 %>%
  mutate(
    days_after_admit = assessment_date - admit_date,
    days_closest_exit = abs(assessment_date - exit_date),
    days_after_exit = assessment_date - exit_date,
    week_before_exit = if_else(
      days_after_exit <= 0 & days_after_exit >= -7, 
      1, 0
    ),
    within_two_weeks_after_admit = if_else(days_after_admit <= 14, 1, 0),
    within_two_weeks_exit = if_else(days_closest_exit <= 14, 1, 0),
    any_non_na = if_else(count_not_na > 0, 1, 0),
    followup = if_else(time == "FLWP", 1, 0)
  )

# Convert FLWP to DSCHRG for discharge assessment selection
cat("\nConverting follow-up assessments to discharge timing...\n")
elig_rhf_data_assessment_2 <- elig_rhf_data_assessment_2 %>% 
  mutate(time = if_else(time == "FLWP", "DSCHRG", time))

cat("\nSelecting best admission assessment...\n")
cat("Selection criteria (in order):\n")
cat("  1. Within 2 weeks after admission\n")
cat("  2. Has any non-NA GG items\n")
cat("  3. Most non-NA items\n")
cat("  4. Most valid scores (01-06)\n")
cat("  5. Most valid scores (01-06) among all the GG items\n")
cat("  6. Closest to admission date\n\n")

elig_rhf_data_assessment_ADMSN <- elig_rhf_data_assessment_2 %>%
  filter(time == "ADMSN") %>% 
  group_by(SCRSSN, ADMSNDT, INP_hee_thru, hee_from, hee_thru, admit_date) %>%
  arrange(
    SCRSSN, ADMSNDT, hee_from, hee_thru, admit_date, 
    desc(within_two_weeks_after_admit), 
    desc(any_non_na), 
    desc(count_not_na), 
    desc(count_valid), 
    desc(count_valid_ALL), 
    days_after_admit
  ) %>%
  slice_head(n = 1) %>%
  ungroup()

log_count(elig_rhf_data_assessment_ADMSN, "Best admission assessments")

# ==============================================================================
# MODULE 10: SELECT BEST DISCHARGE ASSESSMENT
# ==============================================================================

cat("\n--- MODULE 10: Select Best Discharge Assessment ---\n")

cat("Selection criteria (in order):\n")
cat("  1. Within 7 days before discharge\n")
cat("  2. If none, within 2 weeks after discharge\n")
cat("  3. Has any non-NA GG items\n")
cat("  4. Most non-NA items\n")
cat("  5. Most valid scores (01-06)\n")
cat("  6. Most valid scores among all items (01-06)\n")
cat("  7. Closest to discharge date\n\n")

elig_rhf_data_assessment_DSCHRG <- elig_rhf_data_assessment_2 %>%
  filter(time == "DSCHRG") %>% 
  group_by(SCRSSN, ADMSNDT, INP_hee_thru, hee_from, hee_thru, admit_date) %>%
  arrange(
    SCRSSN, ADMSNDT, hee_from, hee_thru, admit_date, 
    desc(week_before_exit), 
    desc(within_two_weeks_exit), 
    desc(any_non_na), 
    desc(count_not_na), 
    desc(count_valid), 
    desc(count_valid_ALL), 
    days_closest_exit
  ) %>%
  slice_head(n = 1) %>%
  ungroup()

log_count(elig_rhf_data_assessment_DSCHRG, "Best discharge assessments")

# ==============================================================================
# MODULE 11: COMBINE ADMISSION AND DISCHARGE ASSESSMENTS
# ==============================================================================

cat("\n--- MODULE 11: Combine Admission and Discharge Assessments ---\n")

cat("Combining best admission and discharge assessments...\n")
elig_rhf_data_assessment_final <- rbind(
  elig_rhf_data_assessment_ADMSN, 
  elig_rhf_data_assessment_DSCHRG
) %>% 
  select(
    SCRSSN, ADMSNDT, INP_hee_thru, hee_from, hee_thru, admit_date, 
    exit_date, days_diff, los, los_censored, Grouping, setting_series, 
    episode_run, nx_Grouping, days_to_nx_Grouping, 
    readmitted, died, discharge_unpl, Age_ADMSNDT,
    SNF_VA_flag, HH_VA_flag, IRF_VA_flag, time, record_id, 
    prior_cognition_score, prior_self_care_score, prior_mobility_score,
    assessment_date, assessment_occur, GG_total, count_not_na, 
    count_valid, count_valid_ALL, days_after_admit, days_after_exit, followup
  )

# Ensure consistent exit date
cat("Ensuring consistent exit dates within episodes...\n")
elig_rhf_data_assessment_final <- elig_rhf_data_assessment_final %>% 
  group_by(SCRSSN, ADMSNDT, hee_from) %>% 
  mutate(exit_date = max(exit_date, na.rm = TRUE)) %>%
  ungroup()

# Convert to wide format
cat("Converting to wide format (one row per episode)...\n")
elig_rhf_data_assessment_final <- elig_rhf_data_assessment_final %>% 
  pivot_wider(
    names_from = "time", 
    values_from = c(
      "record_id", "assessment_occur", "days_after_admit", 
      "days_after_exit", "assessment_date", "GG_total", 
      "count_not_na", "count_valid", "count_valid_ALL", "followup"
    )
  ) %>%
  arrange(SCRSSN, ADMSNDT, hee_from, hee_thru, admit_date)

# Flag invalid discharge assessments (before admission)
cat("Flagging invalid discharge assessments...\n")
elig_rhf_data_assessment_final <- elig_rhf_data_assessment_final %>% 
  mutate(
    invalid_discharge = coalesce(
      if_else(assessment_date_ADMSN > assessment_date_DSCHRG, 1, 0), 
      0
    )
  )

log_count(elig_rhf_data_assessment_final, "Episodes with best assessments")

# Calculate length of stay variables
cat("Calculating final length of stay variables...\n")
elig_rhf_data_assessment_final <- elig_rhf_data_assessment_final %>% 
  mutate(
    los_assess = as.numeric(exit_date - admit_date), 
    los_pref_assess = coalesce(los_assess, los)
  ) %>% 
  group_by(SCRSSN, ADMSNDT, hee_from) %>%
  mutate(los_max = max(los_assess, los_censored, na.rm = TRUE)) %>%
  ungroup()

save_parquet_safe(elig_rhf_data_assessment_final, 
                  "elig_rhf_data_assessment_final.parquet")

# ==============================================================================
# MODULE 12: APPLY HOSPITALIZATION-LEVEL EXCLUSIONS
# ==============================================================================

cat("\n--- MODULE 12: Apply Hospitalization-Level Exclusions ---\n")

cat("Exclusion criteria:\n")
cat("  - Readmitted to acute care\n")
cat("  - All episodes ≤3 days (too short)\n")
cat("  - Age out of bounds (<65 or >99)\n")
cat("  - Inpatient stay >365 days\n")
cat("  - Died on assessment or within 1 day\n")
cat("  - Unplanned discharge\n\n")

elig_rhf_data_assessment_final <- load_parquet_safe("elig_rhf_data_assessment_final.parquet")

elig_rhf_data_assessment_final_hosp_excl <- 
  elig_rhf_data_assessment_final %>% 
  ungroup() %>%
  mutate(los = coalesce(los_assess, los),
         Too_Short = if_else(Grouping %in% c("SNF", "IRF", "HH") & los <= 3, 1L, 0L),
         SNF_Over_99_Flag = if_else(Grouping == "SNF" & los >= 100, 1L, 0L)
  ) %>%
  mutate(Age_OB = if_else(Age_ADMSNDT > 99 | Age_ADMSNDT < 65, 1, 0)) %>%
  group_by(SCRSSN, ADMSNDT, INP_hee_thru) %>%
  mutate(
    Too_Short_Only = min(Too_Short, na.rm = TRUE),
    SNF_Over_99_Flag = max(SNF_Over_99_Flag, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  distinct() %>%
  left_join(
    elig_rhf %>% 
      ungroup() %>% 
      transmute(
        SCRSSN = scrssn, 
        INP_hee_thru, 
        ADMSNDT, 
        dod,
        care_thru
        ) %>% 
      distinct(), 
    by = c("SCRSSN", "ADMSNDT", "INP_hee_thru")
  ) %>%
  mutate(
    final_thru = coalesce(exit_date, care_thru),
    Died_1_Day = replace_na(if_else(ADMSNDT <= dod & final_thru + 1 >= dod, 1L, 0L), 0),
    died = if_else(Died_1_Day == 1| died == 1, 1, 0),
    Inpatient_Too_Long = if_else(as.numeric(INP_hee_thru - ADMSNDT) > 365, 1L, 0L),
    excl_hosp = if_else(
      readmitted == 1 | 
      Too_Short_Only == 1 | 
      Age_OB == 1 | 
      Inpatient_Too_Long == 1 | 
      died == 1 | 
      Died_1_Day == 1 | 
      discharge_unpl == 1, 
      1, 0
    )
  )

# Summary of exclusions
cat("\nHospitalization-level exclusion summary:\n")
excl_summary <- elig_rhf_data_assessment_final_hosp_excl %>%
  mutate(
    readmitted = factor(readmitted),
    too_short = factor(Too_Short_Only),
    age_out_of_bounds = factor(Age_OB),
    inpatient_too_long = factor(Inpatient_Too_Long),
    died = factor(died),
    unplanned_discharge = factor(discharge_unpl),
    excluded = factor(excl_hosp)
  )

table1(~excluded+readmitted+too_short+age_out_of_bounds+inpatient_too_long+died+unplanned_discharge+excluded, data = excl_summary)

elig_rhf_data_assessment_final_hosp_excl <- 
  elig_rhf_data_assessment_final_hosp_excl %>% 
  distinct(SCRSSN, ADMSNDT, INP_hee_thru, excl_hosp)

# Apply exclusions
cat("\nApplying hospitalization-level exclusions...\n")
elig_rhf_data_assessment_final_epis_excl <- 
  elig_rhf_data_assessment_final %>% 
  inner_join(
    elig_rhf_data_assessment_final_hosp_excl, 
    by = c("SCRSSN", "ADMSNDT", "INP_hee_thru")
  ) %>% 
  filter(excl_hosp == 0)

log_count(elig_rhf_data_assessment_final_epis_excl, 
          "Episodes after hospitalization exclusions")


save_parquet_safe(elig_rhf_data_assessment_final_epis_excl, 
                  "elig_rhf_data_assessment_final_epis_excl.parquet")

cat("Unique hospitalizations:", 
    format(nrow(elig_rhf_data_assessment_final_epis_excl %>% 
           distinct(SCRSSN, ADMSNDT, INP_hee_thru)), big.mark = ","), "\n")

# ==============================================================================
# MODULE 13: APPLY EPISODE-LEVEL EXCLUSIONS: LEVEL 1
# ==============================================================================

cat("\n--- MODULE 13: Apply Episode-Level Exclusions Level 1 ---\n")

cat("Episode-level exclusion criteria:\n")
cat("  - No assessment record matched\n")
cat("  - VA SNF without external assessment\n")
cat("  - VA HH (purchased home health only)\n")
cat("  - VA IRF without assessment data\n\n")

elig_rhf_data_assessment_final_epis_excl1 <- 
  elig_rhf_data_assessment_final_epis_excl %>% 
  ungroup() %>%
  mutate(
    no_record = if_else(
      is.na(record_id_ADMSN) & is.na(record_id_DSCHRG), 
      1, 0
    ),
    has_record = if_else(
      !is.na(record_id_ADMSN) | !is.na(record_id_DSCHRG), 
      1, 0
    ),
    IRF_VA_flag2 = if_else(IRF_VA_flag == 1 & no_record == 1, 1, 0),
    excl_episode_lv1 = if_else(
      no_record == 1 | 
      HH_VA_flag == 1 | 
      SNF_VA_flag == 1 | 
      IRF_VA_flag2 == 1, 
      1, 0
    )
  )

epis_excl_summary1 <- elig_rhf_data_assessment_final_epis_excl1 %>%
  mutate(
    no_record = factor(no_record),
    va_hh = factor(HH_VA_flag),
    va_snf = factor(SNF_VA_flag),
    va_irf_no_data = factor(IRF_VA_flag2),
    excl_episode_lv1 = factor(excl_episode_lv1)
  )

table1(~no_record+va_hh+va_snf+va_irf_no_data+excl_episode_lv1, data = epis_excl_summary1)

# Apply exclusions
cat("\nApplying episode-level exclusions...\n")
elig_rhf_data_assessment_final_epis_excl1 <- 
  elig_rhf_data_assessment_final_epis_excl1 %>% 
  filter(excl_episode_lv1 == 0)

log_count(elig_rhf_data_assessment_final_epis_excl1, 
          "Final analytical sample")

cat("Unique hospitalizations:", 
    format(nrow(elig_rhf_data_assessment_final_epis_excl1 %>% 
           distinct(SCRSSN, ADMSNDT)), big.mark = ","), "\n")
cat("Unique patients:", 
    format(length(unique(elig_rhf_data_assessment_final_epis_excl1$SCRSSN)), 
           big.mark = ","), "\n\n")


save_parquet_safe(elig_rhf_data_assessment_final_epis_excl1, 
                  "elig_rhf_data_assessment_final_epis_excl1.parquet")


# ==============================================================================
# MODULE 14: APPLY EPISODE-LEVEL EXCLUSIONS: LEVEL 2
# ==============================================================================

cat("\n--- MODULE 14: Apply Episode-Level Exclusions: Level 2 ---\n")

cat("Episode-level exclusion criteria:\n")
cat("  - All GG items missing (empty assessments)\n")

elig_rhf_data_assessment_final_epis_excl1 <- load_parquet_safe("elig_rhf_data_assessment_final_epis_excl1.parquet")

elig_rhf_data_assessment_final_epis_excl2 <- 
  elig_rhf_data_assessment_final_epis_excl1 %>% 
  ungroup() %>%
  mutate(no_valid_items = coalesce(if_else(count_valid_ALL_ADMSN == 0 & count_valid_ALL_DSCHRG == 0, 1, 0), 0))

epis_excl_summary2 <- elig_rhf_data_assessment_final_epis_excl2 %>%
  mutate(
    no_valid_items = factor(no_valid_items)
  )

table1(~no_valid_items | Grouping, data = epis_excl_summary2)

# Apply exclusions
cat("\nApplying episode-level exclusions...\n")
elig_rhf_data_assessment_final_epis_excl2 <- 
  elig_rhf_data_assessment_final_epis_excl2 %>% 
  filter(no_valid_items == 0)

log_count(elig_rhf_data_assessment_final_epis_excl2, 
          "Final analytical sample")

cat("Unique hospitalizations:", 
    format(nrow(elig_rhf_data_assessment_final_epis_excl2 %>% 
                  distinct(SCRSSN, ADMSNDT)), big.mark = ","), "\n")
cat("Unique patients:", 
    format(length(unique(elig_rhf_data_assessment_final_epis_excl2$SCRSSN)), 
           big.mark = ","), "\n\n")


save_parquet_safe(elig_rhf_data_assessment_final_epis_excl2, 
                  "elig_rhf_data_assessment_final_epis_excl2.parquet")
# ==============================================================================
# DATA VALIDATION AND SUMMARY STATISTICS
# ==============================================================================

cat("\n--- Data Validation and Summary Statistics ---\n")

elig_rhf_data_assessment_final_epis_excl2 <- load_parquet_safe("elig_rhf_data_assessment_final_epis_excl2.parquet")

# Final sample by setting
cat("\nFinal sample by care setting:\n")
setting_summary <- elig_rhf_data_assessment_final_epis_excl2 %>%
  group_by(Grouping) %>%
  summarize(
    n = n(),
    with_admission = sum(!is.na(record_id_ADMSN)),
    with_discharge = sum(!is.na(record_id_DSCHRG)),
    with_both = sum(!is.na(record_id_ADMSN) & !is.na(record_id_DSCHRG)),
    mean_los = mean(los, na.rm = TRUE),
    median_los = median(los, na.rm = TRUE),
    .groups = 'drop'
  )

print(setting_summary)

# Assessment timing summary
cat("\nAssessment timing summary:\n")
timing_summary <- elig_rhf_data_assessment_final_epis_excl2 %>%
  group_by(Grouping) %>%
  summarize(
    mean_days_after_admit = mean(days_after_admit_ADMSN, na.rm = TRUE),
    median_days_after_admit = median(days_after_admit_ADMSN, na.rm = TRUE),
    mean_days_before_discharge = mean(-days_after_exit_DSCHRG, na.rm = TRUE),
    median_days_before_discharge = median(-days_after_exit_DSCHRG, na.rm = TRUE)
  )

print(timing_summary)

# GG total score summary
cat("\nGG total score summary:\n")
gg_summary <- elig_rhf_data_assessment_final_epis_excl2 %>%
  group_by(Grouping) %>%
  summarize(
    mean_gg_admit = mean(GG_total_ADMSN, na.rm = TRUE),
    median_gg_admit = median(GG_total_ADMSN, na.rm = TRUE),
    mean_gg_discharge = mean(GG_total_DSCHRG, na.rm = TRUE),
    median_gg_discharge = median(GG_total_DSCHRG, na.rm = TRUE),
    mean_change = mean(GG_total_DSCHRG - GG_total_ADMSN, na.rm = TRUE)
  )

print(gg_summary)

# ==============================================================================
# SESSION INFO
# ==============================================================================

cat("\n=============================================================================\n")
cat("SCRIPT 05B COMPLETED SUCCESSFULLY\n")
cat("=============================================================================\n")
cat("\nKey Output Files:\n")
cat("  - elig_rhf_data_assessment_2.parquet: All episode-assessment matches\n")
cat("  - elig_rhf_data_assessment_final.parquet: Best admission/discharge per episode\n")
cat("  - elig_rhf_data_assessment_final_epis_excl.parquet: Final analytical dataset\n")
cat("  - gg_complete_item.parquet: Item-level data for visualization\n\n")

cat("Summary Statistics:\n")
cat("  Episodes matched to assessments:", 
    format(nrow(elig_rhf_data_assessment_final), big.mark = ","), "\n")
cat("  Episodes after exclusions:", 
    format(nrow(elig_rhf_data_assessment_final_epis_excl), big.mark = ","), "\n")
cat("  Unique patients:", 
    format(length(unique(elig_rhf_data_assessment_final_epis_excl$SCRSSN)), 
           big.mark = ","), "\n")
cat("  IRF episodes:", 
    sum(elig_rhf_data_assessment_final_epis_excl$Grouping == "IRF"), "\n")
cat("  SNF episodes:", 
    sum(elig_rhf_data_assessment_final_epis_excl$Grouping == "SNF"), "\n")
cat("  HH episodes:", 
    sum(elig_rhf_data_assessment_final_epis_excl$Grouping == "HH"), "\n")
cat("=============================================================================\n\n")

################################################################################
# END OF SCRIPT
################################################################################
