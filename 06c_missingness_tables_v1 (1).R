################################################################################
# SCRIPT 06B: CREATE MISSINGNESS TABLES
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
# Created: 2025-12-16
# Last Modified: 2025-12-16
# Version: 1.0
#
################################################################################

# ==============================================================================
# SETUP
# ==============================================================================

# Source configuration
source("00_config.R")

# Load required packages
load_packages(c("dplyr", "data.table", "dbplyr", "DBI", "VINCI", 
                "arrow", "table1", "gt", "scales", "tidyr", "mice"))

cat("\n=============================================================================\n")
cat("SCRIPT 04A: APPLY EXCLUSION CRITERIA AND CREATE ANALYTICAL DATASET\n")
cat("=============================================================================\n\n")

# Set working directory
setwd(project_base)

# ==============================================================================
#  LOAD PARQUET DATA
# ==============================================================================

# Load post-acute care episodes and assessment data
cat("Loading post-acute care episodes...\n")
elig_rhf_data_assessment_final <- load_parquet_safe("elig_rhf_data_assessment_2.parquet") %>% distinct()


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

gg_complete_item <- load_parquet_safe("gg_complete_item.parquet")

# Item analyses - RAW:
ggplt_items <- gg_complete_item %>% 
  group_by(Grouping, time, gg_item) %>%
  mutate(cnt = n()) %>%
  group_by(Grouping, time, gg_item, gg_val) %>%
  summarize(perc = n()/cnt) %>% distinct()

# HH
# Admission and discharge
ggplot(ggplt_items %>% filter(Grouping == "HH"), aes(x = perc, y=reorder(gg_item, desc(gg_item)), fill = gg_val)) + 
  geom_col() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Percent") +
  ylab("GG Item") + 
  ggtitle("Home Health GG Item Missingness", subtitle = "2019-2022") +
  facet_grid(cols = vars(time)) + 
  theme(legend.justification = "center", legend.key = element_rect(fill = "white", color = "black")) 

# IRF
# Admission and discharge
ggplot(ggplt_items %>% filter(Grouping == "IRF"), aes(x = perc, y=reorder(gg_item, desc(gg_item)), fill = gg_val)) + 
  geom_col() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Percent") +
  ylab("GG Item") + 
  ggtitle("IRF GG Item Missingness", subtitle = "2019-2022") +
  facet_grid(cols = vars(time)) + 
  theme(legend.justification = "center", legend.key = element_rect(fill = "white", color = "black"))


# SNF
# Admission and discharge
ggplot(ggplt_items %>% filter(Grouping == "SNF"), aes(x = perc, y=reorder(gg_item, desc(gg_item)), fill = gg_val)) + 
  geom_col() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Percent") +
  ylab("GG Item") + 
  ggtitle("SNF GG Item Missingness", subtitle = "2019-2022") +
  facet_grid(cols = vars(time)) + 
  theme(legend.justification = "center", legend.key = element_rect(fill = "white", color = "black"))

# Missing data analysis:

library(naniar)


cat("Loading data...\n")

cohort_comorb_frailty <- load_parquet_safe("cohort_comorb_frailty.parquet")

cohort_prevservice_use <- load_parquet_safe("cohort_prevservice_use.parquet") 


# Load eligible cohort with exclusions applied
dt_first_post_acute_elig <- load_parquet_safe("dt_first_post_acute_elig.parquet") %>% 
  mutate(
    StrokeType = if_else(ICD10 %in% c('I63','I66','I67','I97', 'G46'), "Ischemic", 
                         if_else(ICD10 %in% c('I60','I61','I62'), "Hemorrhagic", "Other"))
  ) %>% 
  left_join(cohort_comorb_frailty, by = c("PatientICN", "ADMSNDT")) %>% 
  left_join(cohort_prevservice_use, by = c("PatientICN", "ADMSNDT"))

cat("  Loaded", nrow(dt_first_post_acute_elig), "eligible episodes\n")

# Load GG functional assessment codes
elig_rhf_data_assessment_final_epis_excl <- load_parquet_safe("elig_rhf_data_assessment_final_epis_excl.parquet") %>% 
  dplyr::select(SCRSSN, ADMSNDT, INP_hee_thru, admit_date, exit_date, days_diff, los, 
                days_after_admit_ADMSN:followup_DSCHRG
  )

analysis_data <- dt_first_post_acute_elig %>% mutate(SCRSSN = scrssn) %>%
  inner_join(elig_rhf_data_assessment_final_epis_excl, by = c("SCRSSN", "ADMSNDT", "INP_hee_thru"))

analysis_data <- analysis_data %>% distinct()

# Missingness analysis (exploratory)

library(naniar)

gg_miss_span(analysis_data, GG_total_DSCHRG, span_every = 1000)

misschk <- analysis_data %>% 
  group_by(Grouping) %>%
  miss_var_summary()


analysis_data %>% dplyr::select(Grouping, GG_total_ADMSN, GG_total_DSCHRG) %>% bind_shadow() %>%
  ggplot(aes(x = GG_total_ADMSN, fill = GG_total_DSCHRG_NA)) + geom_density(alpha = 0.5)

# Acute strok severity
analysis_data %>% dplyr::select(Grouping, acu_stroke_severity, GG_total_DSCHRG) %>% bind_shadow() %>%
  ggplot(aes(x = acu_stroke_severity, fill = GG_total_DSCHRG_NA)) + geom_density(alpha = 0.5)







