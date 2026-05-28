# Waddell CDA
# 6a v4: Data prep + imputation scaffolding
# Nick Cardamone / Updated: 2026-01-12

# Purpose:
#   1. Create an analysis-ready cohort derived from 05a/05b outputs
#   2. Enforce GG admission/discharge completeness rules (>=1 scored item each)
#   3. Re-build missingness tables for the 10 discharge functional items by setting
#   4. Quantify functional discharge-score completeness (10 core items)
#   5. Prepare covariate tables for modeling (moved to 7b)
#   6. Set up three imputation/estimation strategies:
#        a. Primary: setting-aware PMM using 2-level mice (miceadds)
#        b. CMS sensitivity: simple replacement of error codes with "01"
#        c. Hybrid sensitivity: deterministic replacement for MNAR codes (e.g., 88) then PMM
#   7. Persist tidy objects + metadata for 7b (visualizations & regression)

# ============================================================================
# SETUP
# ============================================================================

source("00_config.R")

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(lubridate)
  library(arrow)
  library(openxlsx)
  library(mice)
  library(miceadds)
})

if (!exists("log_count")) {
  log_count <- function(df, label) {
    cat(sprintf("%s: %s\n", label, format(nrow(df), big.mark = ",")))
  }
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}


project_outputs <- file.path(project_base, "derived_data")
analysis_dir <- file.path(project_outputs, "analysis")
ensure_dir(analysis_dir)

required_inputs <- c("dt_first_post_acute_elig.parquet", "gg_complete_item.parquet")

missing_inputs <- function() {
  present <- file.exists(file.path(dir_parquet, required_inputs))
  required_inputs[!present]
}

# ============================================================================
# LOAD DATA
# ============================================================================

# Patient and hospitalization-level variables

dt_post_acute_elig <- load_parquet_safe("dt_post_acute_elig.parquet") %>%
  mutate(
    ADMSNDT = as.Date(ADMSNDT),
    StrokeType = case_when(
      ICD10 %in% c("I63", "I66", "I67", "I97", "G46") ~ "Ischemic",
      ICD10 %in% c("I60", "I61", "I62") ~ "Hemorrhagic",
      TRUE ~ "Other"
    )
    #episode_id = str_c(scrssn, hee_from, format(ADMSNDT, "%Y%m%d"), sep = "_")
  ) %>% 
  transmute(PatientICN, SCRSSN = scrssn, ADMSNDT, DSCHRGDT, Age_ADMSNDT, inpatient_setting, dob, dod, Acute_Inpatient_ICU, any_VA, only_VA, any_NonVA, only_NonVA, ICD10, StrokeType, Gender, Race, Non_White, Ethnicity, MaritalStatus, Married, GISURH, loc_source, Rural, ADI_NATRANK, dual, FFS_only, MA, PriorityGroupName) %>% distinct()

# 95,911 post-acute episodes

elig_rhf_data_assessment_final_epis_excl2 <- load_parquet_safe("elig_rhf_data_assessment_final_epis_excl2.parquet") %>%
  select(SCRSSN, ADMSNDT, hee_from, hee_thru, admit_date, exit_date, days_diff, los, los_assess, episode_run, nx_Grouping, days_to_nx_Grouping, readmitted, died, discharge_unpl, prior_cognition_score, prior_self_care_score, prior_mobility_score, Grouping, record_id_ADMSN, record_id_DSCHRG, assessment_date_ADMSN, assessment_date_DSCHRG) %>%
  inner_join(dt_post_acute_elig, by = c("SCRSSN", "ADMSNDT")) %>% 
  select(PatientICN, SCRSSN, ADMSNDT, DSCHRGDT, inpatient_setting, Age_ADMSNDT, dob, dod, Acute_Inpatient_ICU, any_VA, only_VA, any_NonVA, only_NonVA, ICD10, StrokeType, Gender, Race, Non_White, Ethnicity, MaritalStatus, Married, GISURH, loc_source, Rural, ADI_NATRANK, dual, FFS_only, MA, PriorityGroupName,  Grouping, hee_from, hee_thru, admit_date, exit_date, days_diff, los, los_assess, episode_run, nx_Grouping, days_to_nx_Grouping, readmitted, died, discharge_unpl, prior_cognition_score, prior_self_care_score, prior_mobility_score, record_id_ADMSN, record_id_DSCHRG, assessment_date_ADMSN, assessment_date_DSCHRG) %>% distinct()

# 43,155 rows

ADMSN_DATA <- elig_rhf_data_assessment_final_epis_excl2 %>% 
  transmute(PatientICN, SCRSSN, ADMSNDT, DSCHRGDT, inpatient_setting, Age_ADMSNDT, dob, dod, Acute_Inpatient_ICU, any_VA, only_VA, any_NonVA, only_NonVA, ICD10, StrokeType, Gender, Race, Non_White, Ethnicity, 
            MaritalStatus, Married, GISURH, loc_source, Rural, ADI_NATRANK, dual, FFS_only, MA, PriorityGroupName,  
            Grouping, hee_from, hee_thru, admit_date, exit_date, days_diff, los, los_assess, episode_run, nx_Grouping, days_to_nx_Grouping, readmitted, died, discharge_unpl, 
            prior_cognition_score, prior_self_care_score, prior_mobility_score, 
            assessment_date = assessment_date_ADMSN,
            record_id = record_id_ADMSN, time = "ADMSN") %>% distinct()

DSCHRG_DATA <- elig_rhf_data_assessment_final_epis_excl2 %>% 
  transmute(PatientICN, SCRSSN, ADMSNDT, DSCHRGDT, inpatient_setting, Age_ADMSNDT, dob, dod, Acute_Inpatient_ICU, any_VA, only_VA, any_NonVA, only_NonVA, ICD10, StrokeType, Gender, Race, Non_White, Ethnicity, 
            MaritalStatus, Married, GISURH, loc_source, Rural, ADI_NATRANK, dual, FFS_only, MA, PriorityGroupName,  
            Grouping, hee_from, hee_thru, admit_date, exit_date, days_diff, los, los_assess, episode_run, nx_Grouping, days_to_nx_Grouping, readmitted, died, discharge_unpl, 
            prior_cognition_score, prior_self_care_score, prior_mobility_score, 
            assessment_date = assessment_date_DSCHRG,
            record_id = record_id_DSCHRG, time = "DSCHRG") %>% distinct()

FULL_DATA <- rbind(DSCHRG_DATA, ADMSN_DATA)

# Load item data:
gg_complete_wide <- load_parquet_safe("gg_complete_wide.parquet")

gg_complete_wide <- gg_complete_wide %>% 
  mutate(time = if_else(time == "FLWP", "DSCHRG", time))

gg_complete_wide <- gg_complete_wide %>% 
  mutate(count_valid_10 = count_valid) %>% 
  select(-starts_with("gg_score"), -any_IJRS, -count_valid, -admit_date) %>% 
  filter(count_not_na > 0) %>% 
  distinct()

# Load comorbidity:
cohort_comorb_frailty <- load_parquet_safe("cohort_comorb_frailty.parquet") %>% 
  select(PatientICN, ADMSNDT, p1_frailty_index, p2_charlson_cindex, acu_stroke_severity) %>% distinct()

# Load prior service use:
cohort_prevservice_use <- load_parquet_safe("cohort_prevservice_use.parquet")

# Load va primary care enrollment:
cohort_with_pact <- load_parquet_safe("cohort_with_pact.parquet")

# Load race/ethnicity:
cohort_race_eth <- load_parquet_safe("full_dat_race_eth.parquet")

# Load primary stroke hospitalization:
cohort_prior_stroke <- load_parquet_safe("prior_stroke_visit_summary.parquet") %>% distinct()

# Join to episode data:

FULL_DATA <- FULL_DATA %>% 
  select(-Race, -Ethnicity) %>%
  left_join(cohort_comorb_frailty, by = c("PatientICN", "ADMSNDT")) %>%
  left_join(cohort_prevservice_use, by = c("PatientICN", "ADMSNDT")) %>%
  left_join(cohort_with_pact, by = c("PatientICN", "ADMSNDT")) %>%
  left_join(cohort_race_eth %>% select(PatientICN, race, eth), by = "PatientICN") %>%
  left_join(cohort_prior_stroke, by = c("PatientICN", "ADMSNDT")) %>%
  left_join(gg_complete_wide, by = c("SCRSSN", "record_id", "time", "assessment_date"))

log_count(elig_rhf_data_assessment_final_epis_excl2, "Eligible cohort (post exclusions)")
log_count(FULL_DATA, "GG items (all sources)")

selfcare_codes <- c("GG0130A", "GG0130B", "GG0130C", "GG0130E", "GG0130F", "GG0130G", "GG0130H")
mobility_codes <- c("GG0170A", "GG0170B", "GG0170C", "GG0170D", "GG0170E", "GG0170F",
                    "GG0170G", "GG0170I", "GG0170J", "GG0170K", "GG0170L", "GG0170M",
                    "GG0170N", "GG0170O", "GG0170P")
all_gg_codes <- c(selfcare_codes, mobility_codes)


# ============================================================================
# BUILD EPISODE-LEVEL GG TOTALS + FILTER BY >=1 ADMSN & >=1 DSCHRG ITEM
# ============================================================================

cat("Building admission/discharge totals (22-item scale)...\n")
analysis_data <- FULL_DATA %>%
  mutate(
    acute_los = as.numeric(DSCHRGDT - ADMSNDT),
    acute_los = if_else(acute_los < 0, 0, acute_los),
    count_not_na = coalesce(count_not_na, 0),
    SNF_Over_99_Flag = if_else(Grouping == "SNF" & los_assess >= 100, 1L, 0L)
  ) %>%
  select(time, p1_frailty_index:p1_count_pc, py2_any_pdx_stroke, py2_count_pdx_stroke, Acute_Inpatient_ICU, va_primary_care_p1, va_primary_care_p2, va_primary_care_ever, acute_los,SNF_Over_99_Flag, race, eth, PatientICN:record_id,  gg_val_GG0130A:count_valid_10) %>%
  pivot_wider(
    names_from = "time",
    values_from = assessment_date:count_valid_10
  )

log_count(analysis_data, "Episodes with >=1 admission & discharge item")

save_parquet_safe(analysis_data, "analysis_data.parquet")
save_parquet_safe(FULL_DATA, "FULL_DATA.parquet")

# ============================================================================
# MISSINGNESS TABLE FOR FUNCTIONAL ITEMS
# ============================================================================

cat("Recomputing missing GG tables (10 discharge items) by setting...\n")
functional_long <- FULL_DATA %>% 
  select(SCRSSN, Grouping, record_id, admit_date, time, starts_with("gg_val")) %>% 
  pivot_longer(cols = starts_with("gg_val_"),
               names_to = "gg_item",
               names_prefix = "gg_val_",
               values_to = "gg_val")

save_parquet_safe(functional_long, "functional_long.parquet")

missing_gg_codes <- functional_long %>%
  dplyr::mutate(
    error_flag = if_else(as.character(gg_val) %in% c("07", "08", "09", "10", "88"), 1, 0),
    missing_flag = if_else(is.na(gg_val), 1, 0),
    error_or_missing_flag = if_else(as.character(gg_val) %in% c("07", "08", "09", "10", "88") | is.na(gg_val), 1, 0)
  ) %>%
  dplyr::group_by(Grouping, time, gg_item) %>%
  dplyr::summarise(
    n_records = dplyr::n(),
    n_error = sum(error_flag, na.rm = TRUE),
    pct_error = round(100 * n_error / n_records, 2),
    n_missing = sum(missing_flag, na.rm = TRUE),
    pct_missing = round(100 * n_missing / n_records, 2),
    n_error_or_missing = sum(error_or_missing_flag, na.rm = TRUE),
    pct_error_or_missing = round(100 * n_error_or_missing / n_records, 2),
    .groups = "drop"
  ) %>% distinct()

missing_table_path <- file.path("P:/ORD_Waddell_202408036D/nick/R/Stroke_Rehabilitation/missing_gg_codes_by_setting.csv")
write_csv(missing_gg_codes, missing_table_path)

# ============================================================================
# PATIENT-LEVEL SAMPLING (1 EPISODE PER SCRSSN)
# ============================================================================

# Identify patients with multiple episodes
analysis_data <- load_parquet_safe("analysis_data.parquet")

# Limit to final post-acute care location:
analysis_data <- analysis_data %>% 
  arrange(SCRSSN, ADMSNDT, desc(episode_run)) %>%
  group_by(SCRSSN, ADMSNDT) %>%
  slice_head(n = 1) # select final episode of post-acute care run

patient_episode_counts <- analysis_data %>%
  group_by(SCRSSN) %>%
  summarise(n_episodes = n(), .groups = "drop")

# Randomly select one episode per patient
set.seed(12345)  # for reproducibility
analysis_data_patient <- analysis_data %>%
  left_join(patient_episode_counts, by = "SCRSSN") %>%
  mutate(multiple_hospitalizations = if_else(n_episodes > 1, 1, 0)) %>%
  group_by(SCRSSN, ADMSNDT) %>%
  mutate(total_post_acute = max(episode_run)) %>%
  ungroup() %>%
  group_by(SCRSSN) %>%
  slice_sample(n = 1) %>%  # randomly select 1 episode per patient
  ungroup()

analysis_data_patient <- analysis_data_patient %>% 
    transmute(
      PatientICN,
      SCRSSN,
      calendar_year = factor(year(ADMSNDT)),
      acute_admsndt = ADMSNDT,
      acute_dschrgdt = DSCHRGDT,
      acute_los = acute_los,
      acute_icu = factor(if_else(Acute_Inpatient_ICU == 1, "Yes", "No"), levels = c("No", "Yes")),
      acute_setting = factor(inpatient_setting, levels = c("VA", "Non-VA", "Both")),
      acute_stroke_severity = acu_stroke_severity,
      dob,
      dod,
      stroke_type = factor(case_when(
        StrokeType == "Ischemic" ~ "Ischemic",
        StrokeType == "Hemorrhagic" ~ "Hemorrhagic",
        TRUE ~ "Other"
      ), levels = c("Ischemic", "Hemorrhagic", "Other")),
      age = Age_ADMSNDT,
      age_category = case_when(
        Age_ADMSNDT >= 65 & Age_ADMSNDT < 70 ~ "65-69",
        Age_ADMSNDT >= 70 & Age_ADMSNDT < 75 ~ "70-74",
        Age_ADMSNDT >= 75 & Age_ADMSNDT < 80 ~ "75-79",
        Age_ADMSNDT >= 80 & Age_ADMSNDT < 85 ~ "80-84",
        Age_ADMSNDT >= 85 ~ "85+",
        TRUE ~ NA
      ),
      age_category = factor(age_category, levels = c("65-69", "70-74", "75-79", "80-84", "85+")),
      Gender = factor(case_when(
        Gender == "MALE" ~ "Male",
        Gender == "FEMALE" ~ "Female",
      ), levels = c("Male", "Female")),
      marital_status = case_when(
        MaritalStatus == "MARRIED" ~ "Married",
        MaritalStatus == "DIVORCED" ~ "Separated/Widowed/Divorced",
        MaritalStatus == "SEPARATED" ~ "Separated/Widowed/Divorced",
        MaritalStatus == "WIDOWED" ~ "Separated/Widowed/Divorced",
        MaritalStatus == "UNKNOWN" ~ "Unknown",
        MaritalStatus == "NEVER MARRIED" ~ "Never Married",
        TRUE ~ NA
        ),
      marital_status = factor(marital_status, levels = c("Married", "Separated/Widowed/Divorced", "Never Married", "Unknown")),
      race_category = case_when(
        race == "White" ~ "White",
        race == "Black" ~ "Black",
        race %in% c("American_Indian_AN", "Asian_PI", "Other") ~ "Other",
        TRUE ~ NA
      ),
      race_category = factor(race_category, levels = c("White", "Black", "Other")),
      ethnicity_category = case_when(
        eth == "NotHispanic" ~ "Not Hispanic/Latino",
        eth == "Hispanic" ~ "Hispanic/Latino",
        TRUE ~ NA
      ),
      ethnicity_category = factor(ethnicity_category, levels = c("Not Hispanic/Latino", "Hispanic/Latino")),
      rurality = case_when(
        GISURH == "U" ~ "Urban",
        GISURH %in% c("R", "H", "I") ~ "Rural/Highly Rural/Islander",
        TRUE ~ NA
      ),
      rurality = factor(rurality, levels = c("Urban", "Rural/Highly Rural/Islander")),
      adi = ADI_NATRANK,
      va_hospitalization = factor(if_else(any_VA == 1, "Yes", "No"), levels = c("No", "Yes")),
      va_primary_care_ever = factor(if_else(va_primary_care_ever == 1, "Yes", "No"), levels = c("No", "Yes")),
      
      initial_discharge_loc = factor(Grouping, levels = c("IRF", "SNF", "HH")),
      post_acute_los = los,
      total_post_acute = total_post_acute - 1,
      multiple_post_acute = factor(if_else(total_post_acute > 2, "Multiple", "One"), levels = c("One", "Multiple")),
      prior_twoyear_pdx_stroke = factor(if_else(py2_any_pdx_stroke == 1, "Yes", "No"), levels = c("No", "Yes")),
      prior_twoyear_charlson = p2_charlson_cindex,
      prior_year_kim_frailty = p1_frailty_index,
      prior_year_hospitalization = factor(if_else(p1_count_inpat > 0, "Yes", "No"), levels = c("No", "Yes")),
      prior_cognition_score = as.factor(if_else(prior_cognition_score == 0, NA, as.character(prior_cognition_score))),
      prior_self_care_score = as.factor(if_else(prior_self_care_score == 0, NA, as.character(prior_self_care_score))),
      prior_mobility_score = as.factor(if_else(prior_mobility_score == 0, NA, as.character(prior_mobility_score))),
      baseline_gg = GG_total_ADMSN,
      discharge_gg = GG_total_DSCHRG
    )

save_parquet_safe(analysis_data_patient, "analysis_data_patient.parquet")

