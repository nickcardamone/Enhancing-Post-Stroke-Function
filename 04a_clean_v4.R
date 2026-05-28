################################################################################
# SCRIPT 04A: APPLY EXCLUSION CRITERIA AND CREATE ANALYTICAL DATASET
# Stroke Rehabilitation Research - CDA Study
# ORD Waddell 202408036D
################################################################################
#
# Purpose:
#   Combine post-acute care episodes with demographic data and apply exclusion
#   criteria to create the final analytical dataset. Extract first post-acute
#   care episode per stroke hospitalization for primary analysis.
#
# Author: Nick Cardamone
# Created: 2025-07-30
# Last Modified: 2026-01-16
# Version: 4.0
#
# Processing Steps:
#   1. Load post-acute care episodes and demographics
#   2. Join demographics to post-acute care episodes
#   3. Number post-acute care runs per hospitalization
#   4. Apply exclusion criteria (if defined)
#   5. Extract eligible post-acute care episodes
#   6. Identify first post-acute care setting per hospitalization
#   7. Create analytical datasets for modeling
#
# Inputs:
#   - rhf_post_acute_final.parquet (from Script 02a)
#   - demo_cohort.parquet (from Script 03a)
#   - incl_excl_list (optional exclusion criteria file)
#
# Outputs:
#   - dt_included.parquet: All episodes with demographics
#   - dt_post_acute_elig.parquet: Eligible IRF/SNF/HH episodes
#   - dt_first_post_acute_elig.parquet: First post-acute episode per hospitalization
#
# Expected Results:
#   - ~98K post-acute care episodes with demographics
#   - ~74K first post-acute episodes (one per hospitalization)
#
# Key Variables Created:
#   - post_acute_care_run: Episode sequence number per hospitalization
#   - days_from_stroke_stay: Days from acute discharge to post-acute admission
#   - los_censored: Length of stay censored at 100 days for SNF
#   - setting_run: Consecutive runs of same care setting
#
################################################################################

# ==============================================================================
# SETUP
# ==============================================================================

# Source configuration
source("00_config.R")

# Load required packages
load_packages(c("dplyr", "data.table", "dbplyr", "DBI", "VINCI", 
                "arrow", "table1", "gt", "scales", "tidyr"))

cat("\n=============================================================================\n")
cat("SCRIPT 04A: APPLY EXCLUSION CRITERIA AND CREATE ANALYTICAL DATASET\n")
cat("=============================================================================\n\n")

# Set working directory
setwd(project_base)

# ==============================================================================
# MODULE 1: LOAD POST-ACUTE CARE EPISODES AND DEMOGRAPHICS
# ==============================================================================

cat("\n--- MODULE 1: Load Post-Acute Care Episodes and Demographics ---\n")

# Load post-acute care episodes
cat("Loading post-acute care episodes...\n")
rhf_post_acute_final <- load_parquet_safe("rhf_post_acute_final.parquet") %>% 
  distinct()

log_count(rhf_post_acute_final, "Post-acute care episodes")
cat("Unique hospitalizations:", 
    format(nrow(rhf_post_acute_final %>% 
                distinct(scrssn, ADMSNDT, INP_hee_thru)), big.mark = ","), "\n")
cat("Unique patients:", 
    format(length(unique(rhf_post_acute_final$scrssn)), big.mark = ","), "\n\n")

# Load demographics
cat("Loading demographic data...\n")
demo_cohort <- load_parquet_safe("demo_cohort.parquet") %>% 
  select(-PERSON_ID)  # Remove duplicate ID, keep PatientICN and scrssn

log_count(demo_cohort, "Demographic records")

# ==============================================================================
# MODULE 2: JOIN DEMOGRAPHICS TO POST-ACUTE CARE EPISODES
# ==============================================================================

cat("\n--- MODULE 2: Join Demographics to Post-Acute Care Episodes ---\n")

cat("Joining demographics to post-acute care episodes...\n")
rhf_post_acute_final <- rhf_post_acute_final %>% 
  left_join(demo_cohort, by = c("PatientICN", "ADMSNDT"))

log_count(rhf_post_acute_final, "Episodes with demographics")

# Check for missing demographics
missing_demo <- rhf_post_acute_final %>%
  filter(is.na(Age_ADMSNDT) | is.na(Gender))

if (nrow(missing_demo) > 0) {
  warning(sprintf("%d episodes missing demographic data", nrow(missing_demo)))
  cat("Episodes with missing demographics:", nrow(missing_demo), "\n")
} else {
  cat("All episodes have demographic data\n")
}

# ==============================================================================
# MODULE 3: NUMBER POST-ACUTE CARE EPISODES PER HOSPITALIZATION
# ==============================================================================

cat("\n--- MODULE 3: Number Post-Acute Care Episodes ---\n")

cat("Creating episode sequence numbers and care end dates...\n")
cat("Note: Episode 0 is inpatient, episodes 1+ are post-acute care\n\n")

dt_included <- rhf_post_acute_final %>%  
  # Create unique ID for each episode
  unite("id", c(PatientICN, ADMSNDT, INP_hee_thru), sep = "_", remove = FALSE) %>% 
  group_by(scrssn, ADMSNDT, INP_hee_thru) %>%
  arrange(scrssn, ADMSNDT, INP_hee_thru, episode_run) %>%
  distinct() %>%
  mutate(
    # Episode 0 = inpatient, 1 = first post-acute, 2 = second post-acute, etc.
    post_acute_care_run = row_number() - 1,
    # End date of entire post-acute care sequence
    care_thru = max(hee_thru)
  ) %>% 
  ungroup() %>% 
  mutate(Acute_Inpatient_ICU = if_else(icu == 1 & post_acute_care_run == 0, 1L, 0L)) %>% # only tell me if acute INP has an ICU flag.
  group_by(PatientICN, ADMSNDT) %>%
  mutate(Acute_Inpatient_ICU = max(Acute_Inpatient_ICU, na.rm = T))

log_count(dt_included, "Episodes with sequence numbers")

# Summary of episode sequences
cat("\nPost-acute care episode sequences:\n")
episode_summary <- dt_included %>%
  group_by(scrssn, ADMSNDT, INP_hee_thru) %>%
  summarize(
    total_episodes = n(),
    post_acute_episodes = sum(post_acute_care_run > 0),
    .groups = 'drop'
  )

cat("Mean episodes per hospitalization:", 
    round(mean(episode_summary$total_episodes), 2), "\n")
cat("Mean post-acute episodes:", 
    round(mean(episode_summary$post_acute_episodes), 2), "\n\n")

# Save full dataset with episode numbers
save_parquet_safe(dt_included, "dt_included.parquet")

# ==============================================================================
# MODULE 4: INSPECT LOS
# ==============================================================================

cat("\n--- MODULE 4: Post-acute care length of stay ---\n")

cat("Filtering to IRF, SNF, and HH episodes only...\n")

dt_post_acute_elig <- dt_included %>%  
  filter(Grouping %in% c("IRF", "SNF", "HH")) %>% 
  mutate(
    # Calculate days from acute discharge to post-acute admission
    days_from_stroke_stay = as.numeric(hee_from - INP_hee_thru),
    # Censor SNF length of stay at 100 days (Medicare coverage limit)
    los_censored = case_when(
      Grouping == "SNF" & los > 99 ~ 100,
      TRUE ~ los
    )
  )

log_count(dt_post_acute_elig, "Eligible post-acute care episodes")

# Summary by care setting
cat("\nEpisodes by care setting:\n")
setting_summary <- dt_post_acute_elig %>%
  group_by(Grouping) %>%
  summarize(
    n = n(),
    mean_los = mean(los, na.rm = TRUE),
    mean_los_censored = mean(los_censored, na.rm = TRUE),
    median_los = median(los, na.rm = TRUE),
    .groups = 'drop'
  )

print(setting_summary)

# Check for SNF stays over 99 days
if ("SNF_Over_99_Flag" %in% names(dt_post_acute_elig)) {
  snf_censored <- sum(dt_post_acute_elig$SNF_Over_99_Flag == 1, na.rm = TRUE)
  cat("\nSNF stays censored at 100 days:", snf_censored, "\n")
}

# Save eligible post-acute episodes
save_parquet_safe(dt_post_acute_elig, "dt_post_acute_elig.parquet")

# ==============================================================================
# MODULE 5: EXTRACT FIRST POST-ACUTE CARE EPISODE PER HOSPITALIZATION
# ==============================================================================

cat("\n--- MODULE 5: Extract First Post-Acute Care Episode ---\n")

# Reload dataset
dt_post_acute_elig <- load_parquet_safe("dt_post_acute_elig.parquet")

cat("Extracting first post-acute care episode per hospitalization...\n")
cat("Note: If patient transfers between settings, this captures first setting\n\n")

dt_first_post_acute_elig <- dt_post_acute_elig %>%  
  group_by(scrssn, ADMSNDT) %>%
  filter(Grouping %in% c("IRF", "SNF", "HH")) %>%
  mutate(
    # Create runs of consecutive same settings
    setting_run = data.table::rleid(Grouping)
  ) %>%
  arrange(setting_run) %>% 
  # Take first post-acute episode (setting_run 1)
  slice_head(n = 1) %>% 
  ungroup()

log_count(dt_first_post_acute_elig, "First post-acute care episodes")
cat("Unique hospitalizations:", 
    format(nrow(dt_first_post_acute_elig %>% distinct(scrssn, ADMSNDT)), 
           big.mark = ","), "\n")
cat("Unique patients:", 
    format(length(unique(dt_first_post_acute_elig$scrssn)), big.mark = ","), 
    "\n\n")

# Distribution of first discharge location
cat("First post-acute care setting distribution:\n")
first_setting_dist <- dt_first_post_acute_elig %>%
  group_by(Grouping) %>%
  summarize(
    n = n(),
    pct = n() / nrow(dt_first_post_acute_elig) * 100,
    .groups = 'drop'
  ) %>%
  arrange(desc(n))

print(first_setting_dist)

# Save first episode dataset
save_parquet_safe(dt_first_post_acute_elig, "dt_first_post_acute_elig.parquet")

# ==============================================================================
# MODULE 6: DATA VALIDATION AND QUALITY CHECKS
# ==============================================================================

cat("\n--- MODULE 6: Data Validation and Quality Checks ---\n")

# Check for duplicate hospitalizations
cat("Checking for duplicate hospitalizations in first episode dataset...\n")
dup_check <- dt_first_post_acute_elig %>%
  group_by(scrssn, ADMSNDT) %>%
  summarize(n = n(), .groups = 'drop') %>%
  filter(n > 1)

if (nrow(dup_check) > 0) {
  warning(sprintf("%d duplicate hospitalizations found", nrow(dup_check)))
} else {
  cat("No duplicate hospitalizations found ✓\n")
}

# Check for missing key variables
cat("\nChecking for missing key variables...\n")
key_vars <- c("PatientICN", "scrssn", "ADMSNDT", "DSCHRGDT", "Grouping", 
              "hee_from", "hee_thru", "los", "Age_ADMSNDT", "Gender")

missing_summary <- dt_first_post_acute_elig %>%
  summarize(across(
    all_of(key_vars[key_vars %in% names(dt_first_post_acute_elig)]),
    ~sum(is.na(.)),
    .names = "missing_{.col}"
  ))

missing_vars <- missing_summary %>%
  select(where(~. > 0))

if (ncol(missing_vars) > 0) {
  cat("Variables with missing values:\n")
  print(missing_vars)
} else {
  cat("No missing values in key variables ✓\n")
}

# Validate date logic
cat("\nValidating date logic...\n")
date_issues <- dt_first_post_acute_elig %>%
  filter(
    hee_thru < hee_from |  # End before start
    days_from_stroke_stay < 0  # Negative gap
  )

if (nrow(date_issues) > 0) {
  warning(sprintf("%d episodes with date logic issues", nrow(date_issues)))
} else {
  cat("All date logic valid ✓\n")
}

# Age distribution check
cat("\nAge distribution:\n")
age_summary <- dt_first_post_acute_elig %>%
  summarize(
    n = n(),
    mean_age = mean(Age_ADMSNDT, na.rm = TRUE),
    median_age = median(Age_ADMSNDT, na.rm = TRUE),
    min_age = min(Age_ADMSNDT, na.rm = TRUE),
    max_age = max(Age_ADMSNDT, na.rm = TRUE)
  )

print(age_summary)

# Gender distribution
cat("\nGender distribution:\n")
if ("Gender" %in% names(dt_first_post_acute_elig)) {
  gender_dist <- dt_first_post_acute_elig %>%
    group_by(Gender) %>%
    summarize(n = n(), pct = n() / nrow(dt_first_post_acute_elig) * 100, 
              .groups = 'drop')
  print(gender_dist)
}

# Care setting by year
cat("\nFirst post-acute care setting by admission year:\n")
if ("ADMSNDT" %in% names(dt_first_post_acute_elig)) {
  yearly_setting <- dt_first_post_acute_elig %>%
    mutate(admit_year = lubridate::year(ADMSNDT)) %>%
    group_by(admit_year, Grouping) %>%
    summarize(n = n(), .groups = 'drop') %>%
    pivot_wider(names_from = Grouping, values_from = n, values_fill = 0) %>%
    mutate(Total = rowSums(across(where(is.numeric))))
  
  print(yearly_setting)
}

# Length of stay summary by setting
cat("\nLength of stay summary by care setting:\n")
los_by_setting <- dt_first_post_acute_elig %>%
  group_by(Grouping) %>%
  summarize(
    n = n(),
    mean_los = round(mean(los, na.rm = TRUE), 1),
    median_los = median(los, na.rm = TRUE),
    q25_los = quantile(los, 0.25, na.rm = TRUE),
    q75_los = quantile(los, 0.75, na.rm = TRUE),
    min_los = min(los, na.rm = TRUE),
    max_los = max(los, na.rm = TRUE),
    .groups = 'drop'
  )

print(los_by_setting)

# VA vs non-VA care
if ("inpatient_setting" %in% names(dt_first_post_acute_elig)) {
  cat("\nInpatient care setting distribution:\n")
  inpatient_setting_dist <- dt_first_post_acute_elig %>%
    group_by(inpatient_setting, Grouping) %>%
    summarize(n = n(), .groups = 'drop') %>%
    pivot_wider(names_from = Grouping, values_from = n, values_fill = 0)
  
  print(inpatient_setting_dist)
}

# Days from acute discharge to post-acute admission
cat("\nDays from acute discharge to post-acute admission:\n")
gap_summary <- dt_first_post_acute_elig %>%
  group_by(Grouping) %>%
  summarize(
    mean_gap = round(mean(days_from_stroke_stay, na.rm = TRUE), 1),
    median_gap = median(days_from_stroke_stay, na.rm = TRUE),
    min_gap = min(days_from_stroke_stay, na.rm = TRUE),
    max_gap = max(days_from_stroke_stay, na.rm = TRUE),
    .groups = 'drop'
  )

print(gap_summary)

# ==============================================================================
# SESSION INFO
# ==============================================================================

cat("\n=============================================================================\n")
cat("SCRIPT 04A COMPLETED SUCCESSFULLY\n")
cat("=============================================================================\n")
cat("\nKey Output Files:\n")
cat("  - dt_included.parquet: All episodes with demographics and sequence numbers\n")
cat("  - dt_post_acute_elig.parquet: Eligible IRF/SNF/HH episodes\n")
cat("  - dt_first_post_acute_elig.parquet: First post-acute episode per hospitalization\n\n")

cat("Summary Statistics:\n")
cat("  Total post-acute episodes:", 
    format(nrow(dt_post_acute_elig), big.mark = ","), "\n")
cat("  First post-acute episodes:", 
    format(nrow(dt_first_post_acute_elig), big.mark = ","), "\n")
cat("  Unique patients:", 
    format(length(unique(dt_first_post_acute_elig$scrssn)), big.mark = ","), "\n")
cat("  IRF episodes:", 
    sum(dt_first_post_acute_elig$Grouping == "IRF"), "\n")
cat("  SNF episodes:", 
    sum(dt_first_post_acute_elig$Grouping == "SNF"), "\n")
cat("  HH episodes:", 
    sum(dt_first_post_acute_elig$Grouping == "HH"), "\n")
cat("=============================================================================\n\n")

################################################################################
# END OF SCRIPT
################################################################################
