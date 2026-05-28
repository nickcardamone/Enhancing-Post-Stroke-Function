################################################################################
# SCRIPT 06B: CREATE ANALYSIS TABLES
# Stroke Rehabilitation Research - CDA Study
# ORD Waddell 202408036D
################################################################################
#
# Purpose:
#   Generate comprehensive analysis tables for stroke rehabilitation study,
#   including cohort summaries, demographics, functional outcomes, and 
#   GG code analysis across IRF, SNF, and HH settings.
#
# Author: Nick Cardamone
# Created: 2025-06-10
# Last Modified: 2025-12-11
# Version: 3.0
#
# Inputs:
#   - pdx_stroke_visit_summary.parquet (from Script 1)
#   - rhf_cohort.parquet (from Script 2a)
#   - rhf_post_acute_final.parquet (from Script 2a)
#   - dt_first_post_acute_elig.parquet (from Script 4a)
#   - gg_item_analysis_full.parquet (from Script 5b)
#
# Outputs:
#   - Stroke_Rehabilitation_Tables.xlsx: Excel workbook with 10 tabs
#
# Tables Created:
#   Tab 1: By Year Summary (cohort counts and discharge patterns)
#   Tab 2: CONSORT Detail (stepwise exclusions)
#   Tab 3: Overall Characteristics (demographics, comorbidities)
#   Tab 4: Data Quality Checks (quantiles for key measures)
#   Tab 5: Length of Stay by VA/Non-VA Setting
#   Tab 6: Rehabilitation Setting Utilization
#   Tab 7: Stratified Analysis by Acute Care Setting
#   Tab 8a: Demographic Summary (table1 PNG)
#   Tab 8b: Acute Care & LOS Summary (table1 PNG)
#   Tab 8c: Functional Scores Summary (table1 PNG)
#
################################################################################

# ==============================================================================
# SETUP
# ==============================================================================

library(tidyverse)
library(dplyr)
library(openxlsx)
library(data.table)
library(arrow)
library(tableone)
library(gt)

cat("\n=============================================================================\n")
cat("SCRIPT 06B: CREATE ANALYSIS TABLES\n")
cat("=============================================================================\n\n")
source("00_config.R")
analysis_dir <- file.path(project_base, "derived_data", "analysis")

# ==============================================================================
# LOAD DATA
# ==============================================================================

cat("Loading data...\n")

# Load stroke visit summary
pdx_stroke_visit_summary <- load_parquet_safe('pdx_stroke_visit_summary.parquet') %>%
  transmute(scrssn = SCRSSN, 
            ADMSNDT = ACUTE_INPATIENT_VISIT_START, 
            DSCHRGDT = ACUTE_INPATIENT_VISIT_END,
            Inpatient_Setting = if_else(CDW_pdx == 1 & CMS_pdx == 0, "VA", 
                                        if_else(CMS_pdx == 1 & CDW_pdx == 0, "Non-VA", "Both"))) %>% 
  collect() %>% 
  mutate(Inpatient_Setting = factor(Inpatient_Setting, levels = c("VA", "Non-VA", "Both")))

# Load RHF cohort data
con <- connect_db(database = db_project, server = db_server)

groupings_file <- file.path(project_base, "Groupings.xlsx")

Groupings <- readxl::read_xlsx(groupings_file) %>% 
  mutate(Grouping = if_else(Grouping == "EXCL", "OTHER", Grouping))

RHFB <- tbl(con, in_schema(schema_dflt, 'RHFB_SHIP2393')) %>% 
  collect() %>%
  left_join(Groupings, by = c("hee_type1" = "type1"))

RHFB_cohort <- RHFB %>%
  inner_join(
    pdx_stroke_visit_summary %>% transmute(scrssn) %>% distinct(), 
    by = "scrssn"
  )

log_count(RHFB_cohort, "RHF records for stroke patients")
cat("Unique patients:", format(length(unique(RHFB_cohort$scrssn)), big.mark = ","), "\n")

# Check grouping distribution
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

# Filter cohort to patients in RHF
rhf_cohort <- pdx_stroke_visit_summary %>% 
  filter(scrssn %in% RHFB_cohort$scrssn) %>% 
  transmute(scrssn, ADMSNDT, DSCHRGDT, Inpatient_Setting = factor(Inpatient_Setting, levels = c("VA", "Non-VA", "Both"))) %>% distinct()

# Load full post-acute episode file for consort comparisons and downstream derivations
rhf_post_acute_final <- load_parquet_safe("rhf_post_acute_final.parquet")

# Load first post-acute discharge
dt_first_post_acute <- rhf_post_acute_final %>% 
  dplyr::group_by(scrssn, ADMSNDT, INP_hee_thru) %>%
  dplyr::arrange(scrssn, ADMSNDT, INP_hee_thru, episode_run) %>%
  dplyr::slice(2) %>% 
  dplyr::ungroup() %>%
  dplyr::transmute(scrssn, ADMSNDT, DSCHRGDT, INP_hee_thru, Grouping, episode_run) %>% distinct()

# Load first post-acute episodes that meet eligibility criteria
dt_first_post_acute_elig <- load_parquet_safe("dt_first_post_acute_elig.parquet")

# Load rehabilitation setting combinations
rhf_setting_grid <- rhf_post_acute_final %>%
  collect() %>%  
  transmute(scrssn, 
            inpatient_setting,
            ADMSNDT, 
            IRF = if_else(Grouping == "IRF", 1, 0),
            SNF = if_else(Grouping == "SNF", 1, 0),
            HH = if_else(Grouping == "HH", 1, 0)) %>% distinct() %>%
  group_by(scrssn, ADMSNDT, inpatient_setting) %>% 
  summarize(IRF = max(IRF), 
            SNF = max(SNF), 
            HH = max(HH), .groups = "drop") %>% 
  mutate(year = year(ADMSNDT),
         multi = if_else(IRF+SNF+HH > 1, 1, 0),
         IRF_SNF = if_else(IRF+SNF > 1, 1, 0),
         IRF_HH = if_else(IRF+HH>1, 1, 0),
         SNF_HH = if_else(SNF+HH>1, 1, 0))

elig_rhf_data_assessment_final <- load_parquet_safe("elig_rhf_data_assessment_final.parquet")
elig_rhf_data_assessment_final_epis_excl <- load_parquet_safe("elig_rhf_data_assessment_final_epis_excl.parquet")
elig_rhf_data_assessment_final_epis_excl1 <- load_parquet_safe("elig_rhf_data_assessment_final_epis_excl1.parquet")
elig_rhf_data_assessment_final_epis_excl2 <- load_parquet_safe("elig_rhf_data_assessment_final_epis_excl2.parquet")

# Load eligible cohort
analysis_data <- load_parquet_safe("analysis_data.parquet") 

# Create subsets by rehabilitation setting
IRF = analysis_data %>% filter(Grouping == "IRF")
SNF = analysis_data %>% filter(Grouping == "SNF")
HH = analysis_data %>% filter(Grouping == "HH")

# Load rehabilitation setting utilization
rehab_setting <- analysis_data %>%
  transmute(year = year(ADMSNDT), any_VA, only_VA, any_NonVA, only_NonVA) %>%
  mutate(across(everything(), as.factor))

# Load GG functional assessment codes
functional_long <- load_parquet_safe("functional_long.parquet") 

to_binary_flag <- function(x) {
  vals <- as.character(x)
  vals[is.na(vals)] <- "0"
  as.numeric(vals %in% c("1", "TRUE", "True", "true"))
}

# ==============================================================================
# HELPER FUNCTIONS MODULE
# ==============================================================================

# Format n (%) for categorical variables
fmt_n_pct <- function(n, total) {
  pct <- round((n / total) * 100, 2)
  paste0(n, " (", pct, "%)")
}

# Format mean (SD) for continuous variables
fmt_mean_sd <- function(x) {
  paste0(round(mean(x, na.rm = TRUE), 2), " (", round(sd(x, na.rm = TRUE), 2), ")")
}

# Format median [IQR] for continuous variables
fmt_median_iqr <- function(x) {
  paste0(round(median(x, na.rm = TRUE), 2), " [",
         round(quantile(x, 0.25, na.rm = TRUE), 2), ", ",
         round(quantile(x, 0.75, na.rm = TRUE), 2), "]")
}

# Add total column to year-based tables
add_total_col <- function(df) {
  year_cols <- grep("^Y", names(df), value = TRUE)
  if(length(year_cols) > 0) {
    numeric_rows <- 2:(nrow(df)-1)
    totals <- character(nrow(df))
    totals[1] <- ""
    totals[length(totals)] <- ""
    
    for(i in numeric_rows) {
      if(i <= nrow(df)) {
        vals <- suppressWarnings(as.numeric(gsub("%", "", df[i, year_cols])))
        if(all(!is.na(vals)) && !any(grepl("%", df[i, year_cols[1]]))) {
          if(i == 2 && grepl("Unique", df$Category[i], ignore.case = TRUE)) {
            totals[i] <- ""
          } else {
            totals[i] <- as.character(sum(vals, na.rm = TRUE))
          }
        } else {
          totals[i] <- ""
        }
      }
    }
    df$Total <- totals
  }
  return(df)
}

# Rename year columns from Y2018 to 2018
rename_year_cols <- function(df) {
  names(df) <- gsub("^Y", "", names(df))
  return(df)
}

# Count unique Veterans for any dataset with SCRSSN variants
n_unique_scrssn <- function(df) {
  if(is.null(df)) {
    return(NA_integer_)
  }
  scr_cols <- intersect(c("SCRSSN", "scrssn"), names(df))
  if(length(scr_cols) == 0) {
    return(NA_integer_)
  }
  n_distinct(df[[scr_cols[1]]])
}

set_unique_total <- function(df, data_source, label = "    Unique Veterans") {
  if(label %in% df$Category) {
    df$Total[df$Category == label] <- as.character(n_unique_scrssn(data_source))
  }
  df
}

# Utility helpers for consort flow
format_big <- function(x) {
  if(length(x) == 0) {
    return(character())
  }
  out <- ifelse(is.na(x), NA_character_, format(x, big.mark = ",", trim = TRUE))
  return(out)
}

pick_first_col <- function(df, candidates) {
  cols <- intersect(candidates, names(df))
  if(length(cols) == 0) {
    return(NA_character_)
  }
  cols[1]
}

standardize_ids <- function(df) {
  scr_col <- pick_first_col(df, c("SCRSSN", "scrssn"))
  adm_col <- pick_first_col(df, c("ADMSNDT", "admsndt", "ADMSN", "admit_date", "ADMIT_DATE"))
  if(is.na(scr_col) || is.na(adm_col)) {
    return(tibble(SCRSSN = character(), ADMSNDT = as.Date(character())))
  }
  tibble(
    SCRSSN = df[[scr_col]],
    ADMSNDT = as.Date(df[[adm_col]])
  ) %>%
    filter(!is.na(SCRSSN), !is.na(ADMSNDT)) %>%
    distinct()
}

count_people_hosp <- function(df) {
  ids <- standardize_ids(df)
  tibble(
    people = n_distinct(ids$SCRSSN),
    hospitalizations = nrow(ids)
  )
}

format_people_hosp <- function(people, hosp) {
  paste0(format_big(coalesce(people, 0)), " Veterans (", format_big(coalesce(hosp, 0)), " hospitalizations)")
}

build_exclusion_text <- function(total_people, total_hosp, reasons = character()) {
  if((is.na(total_people) || total_people == 0) && (is.na(total_hosp) || total_hosp == 0)) {
    if(length(reasons) == 0) {
      return("No additional exclusions applied.")
    }
  }
  total_line <- paste0("Excluded ", format_people_hosp(total_people, total_hosp), ".")
  if(length(reasons) == 0) {
    return(total_line)
  }
  paste(c(total_line, paste0("- ", reasons)), collapse = "\n")
}

diff_counts <- function(prev_counts, next_counts) {
  tibble(
    people = max(coalesce(prev_counts$people, 0) - coalesce(next_counts$people, 0), 0),
    hospitalizations = max(coalesce(prev_counts$hospitalizations, 0) - coalesce(next_counts$hospitalizations, 0), 0)
  )
}

ensure_SCRSSN <- function(df) {
  if(is.null(df)) {
    return(df)
  }
  has_upper <- "SCRSSN" %in% names(df)
  has_lower <- "scrssn" %in% names(df)
  if(has_upper && has_lower) {
    df <- df %>% mutate(SCRSSN = coalesce(.data[["SCRSSN"]], .data[["scrssn"]])) %>% select(-scrssn, everything())
  } else if(!has_upper && has_lower) {
    df <- df %>% rename(SCRSSN = scrssn)
  }
  df
}



# Summarize episode and Veteran counts by rehabilitation grouping for a given step
summarise_by_setting <- function(df, step_label) {
  if(is.null(df) || !"Grouping" %in% names(df)) {
    return(tibble())
  }
  scr_cols <- intersect(c("SCRSSN", "scrssn"), names(df))
  grouping_ready <- df %>%
    mutate(Grouping = if_else(is.na(Grouping) | Grouping == "", "Unknown", Grouping))
  episode_counts <- grouping_ready %>%
    count(Grouping, name = "Episodes")
  unique_counts <- if(length(scr_cols) > 0) {
    grouping_ready %>%
      group_by(Grouping) %>%
      summarise(Unique_Veterans = n_distinct(.data[[scr_cols[1]]]), .groups = "drop")
  } else {
    tibble(Grouping = unique(grouping_ready$Grouping), Unique_Veterans = NA_integer_)
  }
  full_join(episode_counts, unique_counts, by = "Grouping") %>%
    mutate(Step = step_label) %>%
    select(Step, Grouping, Episodes, Unique_Veterans)
}

# Ensure SCRSSN is available across all key datasets
id_datasets <- c(
  "pdx_stroke_visit_summary", "RHFB", "RHFB_cohort", "rhf_cohort",
  "dt_first_post_acute", "rhf_post_acute_final", "rhf_setting_grid", "dt_first_post_acute_elig",
  "analysis_data", "IRF", "SNF", "HH", "rehab_setting",
  "functional_long", "elig_rhf_data_assessment_final",
  "elig_rhf_data_assessment_final_epis_excl",
  "elig_rhf_data_assessment_final_epis_excl1",
  "elig_rhf_data_assessment_final_epis_excl2"
)

for(obj_name in id_datasets) {
  if(exists(obj_name)) {
    assign(obj_name, ensure_SCRSSN(get(obj_name)))
  }
}

# Refresh rehabilitation setting subsets after enforcing SCRSSN
if(exists("analysis_data")) {
  IRF <- analysis_data %>% filter(Grouping == "IRF")
  SNF <- analysis_data %>% filter(Grouping == "SNF")
  HH <- analysis_data %>% filter(Grouping == "HH")
}

# ==============================================================================
# CREATE EXCEL WORKBOOK
# ==============================================================================

cat("Creating Excel workbook...\n")
wb <- createWorkbook()

# ==============================================================================
# TAB 1: BY YEAR SUMMARY
# ==============================================================================

cat("  Building Tab 1: By Year Summary...\n")
addWorksheet(wb, "Tab1_By_Year")

# Section A - Acute stroke hospitalizations
tab1a_va <- pdx_stroke_visit_summary %>%
  mutate(year = year(ADMSNDT)) %>%
  filter(Inpatient_Setting == "VA") %>%
  group_by(year) %>%
  summarise(n = n(), unique_vets = n_distinct(SCRSSN), .groups = "drop")

tab1a_nonva <- pdx_stroke_visit_summary %>%
  mutate(year = year(ADMSNDT)) %>%
  filter(Inpatient_Setting == "Non-VA") %>%
  group_by(year) %>%
  summarise(n = n(), unique_vets = n_distinct(SCRSSN), .groups = "drop")

tab1a <- data.frame(
  Category = c("A. Number of acute stroke hospitalizations, n", "    Unique Veterans", "    In VA", "    In Community (non-VA)", ""),
  stringsAsFactors = FALSE
)

years <- sort(unique(pdx_stroke_visit_summary %>% mutate(year = year(ADMSNDT)) %>% pull(year)))
for(yr in years) {
  va_n <- tab1a_va %>% filter(year == yr) %>% pull(n)
  nonva_n <- tab1a_nonva %>% filter(year == yr) %>% pull(n)
  
  # Calculate unique Veterans across both settings for this year
  unique_vets_yr <- pdx_stroke_visit_summary %>%
    mutate(year = year(ADMSNDT)) %>%
    filter(year == yr) %>%
    pull(SCRSSN) %>%
    n_distinct()
  
  tab1a[[paste0("Y", yr)]] <- c("", 
                                as.character(unique_vets_yr),
                                if(length(va_n) > 0) as.character(va_n) else "0",
                                if(length(nonva_n) > 0) as.character(nonva_n) else "0",
                                "")
}

# Section B - With discharge to IRF/SNF/HH
tab1b_va <- rhf_cohort %>%
  mutate(year = year(ADMSNDT)) %>%
  filter(Inpatient_Setting == "VA") %>%
  group_by(year) %>%
  summarise(n = n(), unique_vets = n_distinct(SCRSSN), .groups = "drop")

tab1b_nonva <- rhf_cohort %>%
  mutate(year = year(ADMSNDT)) %>%
  filter(Inpatient_Setting == "Non-VA") %>%
  group_by(year) %>%
  summarise(n = n(), unique_vets = n_distinct(SCRSSN), .groups = "drop")

tab1b <- data.frame(
  Category = c("B. Number of acute stroke hospitalizations with discharge to IRF or SNF w/in 3 days or HH w/in 14 days", 
               "    Unique Veterans", "    In VA", "    In Community (non-VA)", ""),
  stringsAsFactors = FALSE
)

for(yr in years) {
  va_n <- tab1b_va %>% filter(year == yr) %>% pull(n)
  nonva_n <- tab1b_nonva %>% filter(year == yr) %>% pull(n)
  
  # Calculate unique Veterans across both settings for this year
  unique_vets_yr <- rhf_cohort %>%
    mutate(year = year(ADMSNDT)) %>%
    filter(year == yr) %>%
    pull(SCRSSN) %>%
    n_distinct()
  
  tab1b[[paste0("Y", yr)]] <- c("", 
                                as.character(unique_vets_yr),
                                if(length(va_n) > 0) as.character(va_n) else "0",
                                if(length(nonva_n) > 0) as.character(nonva_n) else "0",
                                "")
}

# Section C - Initial discharge location
tab1c_data <- dt_first_post_acute_elig %>%
  dplyr::mutate(year = year(ADMSNDT)) %>%
  dplyr::group_by(year, Grouping) %>%
  dplyr::summarise(n = dplyr::n(), unique_vets = n_distinct(SCRSSN), .groups = "drop")

tab1c <- data.frame(
  Category = c("C. Initial Discharge Location after acute hospitalization, n",
               "    Unique Veterans",
               "    Inpatient Rehabilitation Facility (IRF)",
               "    Skilled Nursing Facility (SNF)",
               "    Home Health (HH)", ""),
  stringsAsFactors = FALSE
)

for(yr in years) {
  irf_n <- tab1c_data %>% filter(year == yr, Grouping == "IRF") %>% pull(n)
  snf_n <- tab1c_data %>% filter(year == yr, Grouping == "SNF") %>% pull(n)
  hh_n <- tab1c_data %>% filter(year == yr, Grouping == "HH") %>% pull(n)
  
  # Calculate unique Veterans across all discharge locations for this year
  unique_vets_yr <- dt_first_post_acute_elig %>%
    dplyr::mutate(year = year(ADMSNDT)) %>%
    filter(year == yr) %>%
    pull(SCRSSN) %>%
    n_distinct()
  
  tab1c[[paste0("Y", yr)]] <- c("",
                                as.character(unique_vets_yr),
                                if(length(irf_n) > 0) as.character(irf_n) else "0",
                                if(length(snf_n) > 0) as.character(snf_n) else "0",
                                if(length(hh_n) > 0) as.character(hh_n) else "0",
                                "")
}

# Section D - Multiple services
tab1d_data <- rhf_setting_grid %>%
  group_by(year) %>%
  summarise(
    multi = round(mean(multi, na.rm = TRUE) * 100, 2),
    IRF_SNF = round(mean(IRF_SNF, na.rm = TRUE) * 100, 2),
    IRF_HH = round(mean(IRF_HH, na.rm = TRUE) * 100, 2),
    SNF_HH = round(mean(SNF_HH, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  )

tab1d <- data.frame(
  Category = c("D. Percent receiving more than 1 continuous post-acute rehabilitation service",
               "    Overall", "    IRF + SNF", "    IRF + HH", "    SNF + HH"),
  stringsAsFactors = FALSE
)

for(yr in years) {
  row_data <- tab1d_data %>% filter(year == yr)
  if(nrow(row_data) > 0) {
    tab1d[[paste0("Y", yr)]] <- c("",
                                  paste0(row_data$multi, "%"),
                                  paste0(row_data$IRF_SNF, "%"),
                                  paste0(row_data$IRF_HH, "%"),
                                  paste0(row_data$SNF_HH, "%"))
  } else {
    tab1d[[paste0("Y", yr)]] <- c("", "0%", "0%", "0%", "0%")
  }
}

# Add total columns and rename year columns
tab1a <- add_total_col(tab1a)
tab1b <- add_total_col(tab1b)
tab1c <- add_total_col(tab1c)

tab1a <- rename_year_cols(tab1a)
tab1b <- rename_year_cols(tab1b)
tab1c <- rename_year_cols(tab1c)
tab1d <- rename_year_cols(tab1d)

tab1a <- set_unique_total(tab1a, pdx_stroke_visit_summary)
tab1b <- set_unique_total(tab1b, rhf_cohort)
tab1c <- set_unique_total(tab1c, dt_first_post_acute_elig)

# Combine all sections
tab1_combined <- bind_rows(tab1a, tab1b, tab1c, tab1d)

writeData(wb, "Tab1_By_Year", tab1_combined, startRow = 1, startCol = 1)

# ============================================================================
# TAB 2: CONSORT DETAIL (FLOW)
# ============================================================================

cat("  Building Tab 2: CONSORT Detail...\n")
addWorksheet(wb, "Tab2_CONSORT_Flow")

pdx_ids <- standardize_ids(pdx_stroke_visit_summary)
rhf_ids <- standardize_ids(rhf_post_acute_final)
qualifying_ids <- standardize_ids(dt_first_post_acute_elig)

step_counts <- list(
  A = count_people_hosp(pdx_stroke_visit_summary),
  B = count_people_hosp(rhf_cohort),
  C = count_people_hosp(dt_first_post_acute_elig),
  D = count_people_hosp(elig_rhf_data_assessment_final_epis_excl),
  E = count_people_hosp(elig_rhf_data_assessment_final_epis_excl1),
  `F` = count_people_hosp(elig_rhf_data_assessment_final_epis_excl2)
)

reason_line <- function(label, counts_tbl) {
  if(is.null(counts_tbl) || is.na(counts_tbl$people) || counts_tbl$people == 0) {
    return(NA_character_)
  }
  paste0(label, ": ", format_people_hosp(counts_tbl$people, counts_tbl$hospitalizations))
}

filter_reasons <- function(reasons_vec) {
  Filter(function(x) !is.na(x) && x != "", reasons_vec)
}

not_in_rhf_counts <- count_people_hosp(anti_join(pdx_ids, rhf_ids, by = c("SCRSSN", "ADMSNDT")))
no_overlap_counts <- count_people_hosp(anti_join(rhf_ids, qualifying_ids, by = c("SCRSSN", "ADMSNDT")))

hospital_level_flags <- elig_rhf_data_assessment_final %>%
  ungroup() %>%
  mutate(
    ADMSNDT = as.Date(ADMSNDT),
    INP_hee_thru = as.Date(INP_hee_thru),
    los = coalesce(los_assess, los)
  ) %>%
  mutate(
    Too_Short = if_else(Grouping %in% c("SNF", "IRF", "HH") & los <= 3, 1L, 0L),
    SNF_Over_99_Flag = if_else(Grouping == "SNF" & los >= 100, 1L, 0L)
  ) %>%
  mutate(Age_OB = if_else(Age_ADMSNDT > 99 | Age_ADMSNDT < 65, 1L, 0L)) %>%
  group_by(SCRSSN, ADMSNDT, INP_hee_thru) %>%
  mutate(
    Too_Short_Only = min(Too_Short, na.rm = TRUE),
    SNF_Over_99_Flag = max(SNF_Over_99_Flag, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  distinct() %>%
  left_join(
    load_parquet_safe("dt_post_acute_elig.parquet") %>%
      transmute(SCRSSN = scrssn, ADMSNDT, INP_hee_thru, dod, care_thru) %>%
      distinct(),
    by = c("SCRSSN", "ADMSNDT", "INP_hee_thru")
  ) %>%
  mutate(
    final_thru = coalesce(exit_date, care_thru),
    Died_1_Day = replace_na(if_else(ADMSNDT <= dod & final_thru + 1 >= dod, 1L, 0L), 0L),
    Inpatient_Too_Long = if_else(as.numeric(INP_hee_thru - ADMSNDT) > 365, 1L, 0L),
    excl_hosp = if_else(
      coalesce(readmitted, 0) == 1L |
        Too_Short_Only == 1L |
        Age_OB == 1L |
        Inpatient_Too_Long == 1L |
        coalesce(died, 0) == 1L |
        Died_1_Day == 1L |
        coalesce(discharge_unpl, 0) == 1L,
      1L, 0L
    )
  )

episode_level_lv1 <- elig_rhf_data_assessment_final_epis_excl %>%
  ungroup() %>%
  mutate(
    ADMSNDT = as.Date(ADMSNDT),
    no_record = if_else(is.na(record_id_ADMSN) & is.na(record_id_DSCHRG), 1L, 0L),
    HH_VA_flag = coalesce(HH_VA_flag, 0L),
    SNF_VA_flag = coalesce(SNF_VA_flag, 0L),
    IRF_VA_flag = coalesce(IRF_VA_flag, 0L)
  ) %>%
  mutate(
    IRF_VA_flag2 = if_else(IRF_VA_flag == 1L & no_record == 1L, 1L, 0L),
    excl_episode_lv1 = if_else(no_record == 1L | HH_VA_flag == 1L | SNF_VA_flag == 1L | IRF_VA_flag2 == 1L, 1L, 0L
    )
  )

episode_level_lv2 <- elig_rhf_data_assessment_final_epis_excl1 %>%
  ungroup() %>%
  mutate(
    ADMSNDT = as.Date(ADMSNDT),
    count_valid_ALL_ADMSN = coalesce(count_valid_ALL_ADMSN, 0),
    count_valid_ALL_DSCHRG = coalesce(count_valid_ALL_DSCHRG, 0),
    no_valid_items = if_else(count_valid_ALL_ADMSN == 0 & count_valid_ALL_DSCHRG == 0, 1L, 0L)
  )

step_diffs <- list(
  AB = diff_counts(step_counts$A, step_counts$B),
  BC = diff_counts(step_counts$B, step_counts$C),
  CD = diff_counts(step_counts$C, step_counts$D),
  DE = diff_counts(step_counts$D, step_counts$E),
  EF = diff_counts(step_counts$E, step_counts$`F`)
)

hosp_reasons <- filter_reasons(c(
  reason_line("Readmitted to acute care", count_people_hosp(filter(hospital_level_flags, coalesce(readmitted, 0) == 1L))),
  reason_line("All qualifying episodes ≤3 days", count_people_hosp(filter(hospital_level_flags, Too_Short_Only == 1L))),
  reason_line("Age out of bounds (<65 or >99)", count_people_hosp(filter(hospital_level_flags, Age_OB == 1L))),
  reason_line("Inpatient stay >365 days", count_people_hosp(filter(hospital_level_flags, Inpatient_Too_Long == 1L))),
  reason_line("Died during episode", count_people_hosp(filter(hospital_level_flags, coalesce(died, 0) == 1L))),
  reason_line("Died within 1 day of discharge", count_people_hosp(filter(hospital_level_flags, Died_1_Day == 1L))),
  reason_line("Unplanned discharge", count_people_hosp(filter(hospital_level_flags, coalesce(discharge_unpl, 0) == 1L)))
))

lv1_reasons <- filter_reasons(c(
  reason_line("No assessment record", count_people_hosp(filter(episode_level_lv1, no_record == 1L))),
  reason_line("VA Home Health without assessment", count_people_hosp(filter(episode_level_lv1, HH_VA_flag == 1L))),
  reason_line("VA Skilled Nursing without external assessment", count_people_hosp(filter(episode_level_lv1, SNF_VA_flag == 1L))),
  reason_line("VA IRF without assessment data", count_people_hosp(filter(episode_level_lv1, IRF_VA_flag2 == 1L)))
))

lv2_reasons <- filter_reasons(c(
  reason_line("All GG items missing or error", count_people_hosp(filter(episode_level_lv2, no_valid_items == 1L)))
))

stepA_exclusions <- build_exclusion_text(step_diffs$AB$people, step_diffs$AB$hospitalizations,
                                         filter_reasons(c(
                                           reason_line("Not matched to RHF encounters", not_in_rhf_counts),
                                           reason_line("No qualifying overlap with RHF episode", no_overlap_counts)
                                         )))

stepB_exclusions <- build_exclusion_text(step_diffs$BC$people, step_diffs$BC$hospitalizations)
stepC_exclusions <- build_exclusion_text(step_diffs$CD$people, step_diffs$CD$hospitalizations, hosp_reasons)
stepD_exclusions <- build_exclusion_text(step_diffs$DE$people, step_diffs$DE$hospitalizations, lv1_reasons)
stepE_exclusions <- build_exclusion_text(step_diffs$EF$people, step_diffs$EF$hospitalizations, lv2_reasons)

consort_flow <- tibble(
  Step = c("Step A", "Step B", "Step C", "Step D", "Step E", "Step F"),
  Cohort = c(
    "Acute stroke hospitalizations (VA + Medicare claims)",
    "Hospitalizations found in RHF data (any encounter)",
    "Hospitalizations with IRF/SNF discharge ≤3 days or HH ≤14 days",
    "After hospitalization-level exclusions",
    "After episode-level exclusions (Level 1)",
    "After episode-level exclusions (Level 2) – analytic cohort"
  ),
  Veterans = format_big(c(step_counts$A$people, 
                          step_counts$B$people, 
                          step_counts$C$people,
                          step_counts$D$people, 
                          step_counts$E$people, 
                          step_counts$`F`$people)),
  Hospitalizations = format_big(c(step_counts$A$hospitalizations, step_counts$B$hospitalizations, step_counts$C$hospitalizations,
                                  step_counts$D$hospitalizations, step_counts$E$hospitalizations, step_counts$`F`$hospitalizations)),
  Exclusions = c(stepA_exclusions, stepB_exclusions, stepC_exclusions, stepD_exclusions, stepE_exclusions,
                 "Final analytic cohort used for Tab3_Table1 and all downstream analyses.")
)

writeData(wb, "Tab2_CONSORT_Flow", consort_flow, startRow = 1, startCol = 1)

# ==============================================================================
# TAB 3: OVERALL CHARACTERISTICS (DEMOGRAPHICS & COMORBIDITIES)
# ==============================================================================

cat("  Building Tab 3: Overall Characteristics...\n")
addWorksheet(wb, "Tab3_Table1")# Get overall and by group
overall_data <- analysis_data %>% 
mutate(PriorityGroupName = as.character(if_else(is.na(PriorityGroupName), "Missing", PriorityGroupName)))

irf_data <- overall_data %>% filter(Grouping == "IRF")
snf_data <- overall_data %>% filter(Grouping == "SNF")
hh_data <- overall_data %>% filter(Grouping == "HH")

tab3_characteristics <- data.frame(
  Variable = c(
    "Age, mean (SD)",
    "Ischemic Stroke, n (%)",
    "Hemorrhagic Stroke, n (%)",
    "Gender",
    "    Male, n (%)",
    "    Female, n (%)",
    "    Unknown, n (%)",
    "Race",
    "    American Indian or Alaska Native",
    "    Asian, Native Hawaiian, or Other Pacific Islander",
    "    Black or African American",
    "    Other race not specified",
    "    White",
    "    Missing",
    "Ethnicity",
    "    Hispanic or Latino, n (%)",
    "    Not Hispanic or Latino, n (%)",
    "    Unknown, n (%)",
    "GISRUH",
    "    Urban, n (%)",
    "    Insular, n (%)",
    "    Rural, n (%)",
    "    Highly Rural, n (%)",
    "Marital Status",
    "    Married, n (%)",
    "    Not married, n (%)",
    "    Missing, n (%)",
    "Veteran Priority Group",
    "    1, n (%)", "    2, n (%)", "    3, n (%)", "    4, n (%)",
    "    5, n (%)", "    6, n (%)", "    7, n (%)", "    8, n (%)", "    Missing, n (%)",
    "Dual Eligible (Medicare/Medicaid)",
    "    Yes, n (%)",
    "    No, n (%)",
    "Medicare Type (from Medicare Enrollment Files)",
    "    Fee-for-Service Only, n (%)",
    "    Medicare Advantage, n (%)",
    "ADI National Rank",
    "    Mean (SD)",
    "    Median [IQR]",
    "    Missing, n (%)",
    "Number of inpatient stays in prior year, median [IQR]",
    "Enrolled in VA Primary Care Prior to Hospitalization",
    "    Yes, n (%)",
    "    No, n (%)",
    "VA Primary Care Visit in Prior Year",
    "    Yes, n (%)",
    "    No, n (%)",
    "Acute hospitalization at VA hospital",
    "    Yes, n (%)",
    "    No, n (%)",
    "ICU Admission",
    "    Yes, n (%)",
    "    No, n (%)",
    "Hospitalization with primary diagnosis of stroke in prior two years",
    "    Yes, n (%)",
    "    No, n (%)",
    "Charlson Comorbidity Index, median [IQR]",
    "Kim et al. Frailty Measure, mean (SD)",
    "Stroke Severity Index, mean (SD)",
    "Prior cognition score",
    "    Score of 1, n (%)",
    "    Score of 2, n (%)",
    "    Score of 3, n (%)",
    "    Missing, n (%)",
    "Prior self-care score",
    "    Score of 1, n (%)",
    "    Score of 2, n (%)",
    "    Score of 3, n (%)",
    "    Missing, n (%)",
    "Prior mobility score",
    "    Score of 1, n (%)",
    "    Score of 2, n (%)",
    "    Score of 3, n (%)",
    "    Missing, n (%)"
  ),
  stringsAsFactors = FALSE
)

# Function to calculate stats for each group
calc_stats <- function(data) {
  n_total <- nrow(data)
  
  c(
    # Age
    paste0(round(mean(data$Age_ADMSNDT, na.rm = TRUE), 2), " (", round(sd(data$Age_ADMSNDT, na.rm = TRUE), 2), ")"),
    # Stroke type
    fmt_n_pct(sum(data$StrokeType == "Ischemic", na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$StrokeType == "Hemorrhagic", na.rm = TRUE), n_total),
    "", # Gender header
    fmt_n_pct(sum(data$Gender == "MALE", na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$Gender == "FEMALE", na.rm = TRUE), n_total),
    fmt_n_pct(sum(is.na(data$Gender) | data$Gender == "Unknown", na.rm = TRUE), n_total),
    "", # Race header
    fmt_n_pct(sum(data$race == "American_Indian_AN", na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$race == "Asian_PI", na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$race == "Black", na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$race == "Other", na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$race == "White", na.rm = TRUE), n_total),
    fmt_n_pct(sum(is.na(data$race), na.rm = TRUE), n_total),
    "", # Ethnicity header
    fmt_n_pct(sum(data$eth == "Hispanic", na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$eth == "NotHispanic", na.rm = TRUE), n_total),
    fmt_n_pct(sum(is.na(data$eth), na.rm = TRUE), n_total),
    "", # GISRUH header
    fmt_n_pct(sum(data$GISURH == "U", na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$GISURH == "I", na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$GISURH == "R", na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$GISURH == "H", na.rm = TRUE), n_total),
    "", # Marital Status header
    fmt_n_pct(sum(data$Married == 1, na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$Married == 0, na.rm = TRUE), n_total),
    fmt_n_pct(sum(is.na(data$Married), na.rm = TRUE), n_total),
    "", # Priority Group header
    fmt_n_pct(sum(data$PriorityGroupName == "Group 1", na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$PriorityGroupName == "Group 2", na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$PriorityGroupName == "Group 3", na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$PriorityGroupName == "Group 4", na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$PriorityGroupName == "Group 5", na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$PriorityGroupName == "Group 6", na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$PriorityGroupName == "Group 7", na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$PriorityGroupName == "Group 8", na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$PriorityGroupName == "Missing", na.rm = TRUE), n_total),
    "", # Dual Eligible (Medicare/Medicaid):
    fmt_n_pct(sum(data$dual == 1, na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$dual == 0, na.rm = TRUE), n_total),
    "", # Medicare Type:
    fmt_n_pct(sum(data$FFS_only == 1, na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$MA == 1, na.rm = TRUE), n_total),
    "", # ADI
    paste0(round(mean(data$Age_ADMSNDT, na.rm = TRUE), 2), " (", round(sd(data$Age_ADMSNDT, na.rm = TRUE), 2), ")"),
    paste0(round(median(data$ADI_NATRANK, na.rm = TRUE), 2), " [",
           round(quantile(data$ADI_NATRANK, 0.25, na.rm = TRUE), 2), ", ",
           round(quantile(data$ADI_NATRANK, 0.75, na.rm = TRUE), 2), "]"),
    fmt_n_pct(sum(is.na(data$ADI_NATRANK), na.rm = TRUE), n_total),
    # Inpatient stays
    paste0(round(median(data$p1_count_inpat, na.rm = TRUE), 2), " [",
           round(quantile(data$p1_count_inpat, 0.25, na.rm = TRUE), 2), ", ",
           round(quantile(data$p1_count_inpat, 0.75, na.rm = TRUE), 2), "]"),
    "", # Enrolled in VA Primary Care
    fmt_n_pct(sum(data$va_primary_care_ever == 1, na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$va_primary_care_ever == 0, na.rm = TRUE), n_total),
    "", # VA PC header
    fmt_n_pct(sum(data$p1_any_pc == 1, na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$p1_any_pc == 0, na.rm = TRUE), n_total),
    "", # VA hospital header
    fmt_n_pct(sum(data$inpatient_setting == "VA", na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$inpatient_setting != "VA", na.rm = TRUE), n_total),
    "", # ICU header
    fmt_n_pct(sum(data$Acute_Inpatient_ICU == 1, na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$Acute_Inpatient_ICU == 0, na.rm = TRUE), n_total),
    "", # Hospitalization with primary discharge diagnosis of a stroke
    fmt_n_pct(sum(data$py2_any_pdx_stroke == 1, na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$py2_any_pdx_stroke == 0, na.rm = TRUE), n_total),
    # Charlson:
    paste0(round(median(data$p2_charlson_cindex, na.rm = TRUE), 2), " [",
           round(quantile(data$p2_charlson_cindex, 0.25, na.rm = TRUE), 2), ", ",
           round(quantile(data$p2_charlson_cindex, 0.75, na.rm = TRUE), 2), "]"),
    # Frailty
    paste0(round(mean(data$p1_frailty_index, na.rm = TRUE), 2), " (", 
           round(sd(data$p1_frailty_index, na.rm = TRUE), 2), ")"),
    # Stroke Severity
    paste0(round(mean(data$acu_stroke_severity, na.rm = TRUE), 2), " (", 
           round(sd(data$acu_stroke_severity, na.rm = TRUE), 2), ")"),
    # Prior function scores
    "", #  Prior Cognition Header:
    fmt_n_pct(sum(data$prior_cognition_score == 1, na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$prior_cognition_score == 2, na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$prior_cognition_score == 3, na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$prior_cognition_score == 0, na.rm = TRUE), n_total),
    "", #  Prior Self care Header:
    fmt_n_pct(sum(data$prior_self_care_score == 1, na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$prior_self_care_score == 2, na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$prior_self_care_score == 3, na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$prior_self_care_score == 0, na.rm = TRUE), n_total),
    "", # Prior Mobility Header:
    fmt_n_pct(sum(data$prior_mobility_score == 1, na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$prior_mobility_score == 2, na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$prior_mobility_score == 3, na.rm = TRUE), n_total),
    fmt_n_pct(sum(data$prior_mobility_score == 0, na.rm = TRUE), n_total)
    
  )
}

tab3_characteristics$Overall <- calc_stats(overall_data)
tab3_characteristics$IRF <- calc_stats(irf_data)
tab3_characteristics$SNF <- calc_stats(snf_data)
tab3_characteristics$`Home Health` <- calc_stats(hh_data)

writeData(wb, "Tab3_Table1", tab3_characteristics, startRow = 1, startCol = 1)

# ==============================================================================
# TAB 4: DATA QUALITY CHECKS (QUANTILES)
# ==============================================================================

cat("  Building Tab 4: Data Quality Checks...\n")
addWorksheet(wb, "Tab4_Data_Checks")

quantile_summary <- data.frame(
  Measure = c("acute_los", "IRF_LOS", "SNF_LOS", "HH_LOS", "Age"),
  p5 = c(
    round(quantile(analysis_data$acute_los, 0.05, na.rm = TRUE), 2),
    round(quantile(IRF$los, 0.05, na.rm = TRUE), 2),
    round(quantile(SNF$los, 0.05, na.rm = TRUE), 2),
    round(quantile(HH$los, 0.05, na.rm = TRUE), 2),
    round(quantile(analysis_data$Age_ADMSNDT, 0.05, na.rm = TRUE), 2)
  ),
  p10 = c(
    round(quantile(analysis_data$acute_los, 0.10, na.rm = TRUE), 2),
    round(quantile(IRF$los, 0.10, na.rm = TRUE), 2),
    round(quantile(SNF$los, 0.10, na.rm = TRUE), 2),
    round(quantile(HH$los, 0.10, na.rm = TRUE), 2),
    round(quantile(dt_first_post_acute_elig$Age_ADMSNDT, 0.10, na.rm = TRUE), 2)
  ),
  p25 = c(
    round(quantile(analysis_data$acute_los, 0.25, na.rm = TRUE), 2),
    round(quantile(IRF$los, 0.25, na.rm = TRUE), 2),
    round(quantile(SNF$los, 0.25, na.rm = TRUE), 2),
    round(quantile(HH$los, 0.25, na.rm = TRUE), 2),
    round(quantile(dt_first_post_acute_elig$Age_ADMSNDT, 0.25, na.rm = TRUE), 2)
  ),
  p50 = c(
    round(quantile(analysis_data$acute_los, 0.50, na.rm = TRUE), 2),
    round(quantile(IRF$los, 0.50, na.rm = TRUE), 2),
    round(quantile(SNF$los, 0.50, na.rm = TRUE), 2),
    round(quantile(HH$los, 0.50, na.rm = TRUE), 2),
    round(quantile(dt_first_post_acute_elig$Age_ADMSNDT, 0.50, na.rm = TRUE), 2)
  ),
  p75 = c(
    round(quantile(analysis_data$acute_los, 0.75, na.rm = TRUE), 2),
    round(quantile(IRF$los, 0.75, na.rm = TRUE), 2),
    round(quantile(SNF$los, 0.75, na.rm = TRUE), 2),
    round(quantile(HH$los, 0.75, na.rm = TRUE), 2),
    round(quantile(dt_first_post_acute_elig$Age_ADMSNDT, 0.75, na.rm = TRUE), 2)
  ),
  p90 = c(
    round(quantile(analysis_data$acute_los, 0.90, na.rm = TRUE), 2),
    round(quantile(IRF$los, 0.90, na.rm = TRUE), 2),
    round(quantile(SNF$los, 0.90, na.rm = TRUE), 2),
    round(quantile(HH$los, 0.90, na.rm = TRUE), 2),
    round(quantile(dt_first_post_acute_elig$Age_ADMSNDT, 0.90, na.rm = TRUE), 2)
  ),
  p95 = c(
    round(quantile(analysis_data$acute_los, 0.95, na.rm = TRUE), 2),
    round(quantile(IRF$los, 0.95, na.rm = TRUE), 2),
    round(quantile(SNF$los, 0.95, na.rm = TRUE), 2),
    round(quantile(HH$los, 0.95, na.rm = TRUE), 2),
    round(quantile(dt_first_post_acute_elig$Age_ADMSNDT, 0.95, na.rm = TRUE), 2)
  )
)

writeData(wb, "Tab4_Data_Checks", quantile_summary, startRow = 1, startCol = 1)

# ==============================================================================
# TAB 5: LENGTH OF STAY BY VA/NON-VA SETTING
# ==============================================================================

cat("  Building Tab 5: LOS by VA/Non-VA...\n")
addWorksheet(wb, "Tab5_VA_NonVA")

# Calculate stats for each setting
overall <- analysis_data
va_only <- analysis_data %>% filter(inpatient_setting == "VA")
nonva <- analysis_data %>% filter(inpatient_setting == "Non-VA")
both <- analysis_data %>% filter(inpatient_setting == "Both")

irf_overall <- overall %>% filter(Grouping == "IRF")
irf_va <- va_only %>% filter(Grouping == "IRF")
irf_nonva <- nonva %>% filter(Grouping == "IRF")
irf_both <- both %>% filter(Grouping == "IRF")

snf_overall <- overall %>% filter(Grouping == "SNF")
snf_va <- va_only %>% filter(Grouping == "SNF")
snf_nonva <- nonva %>% filter(Grouping == "SNF")
snf_both <- both %>% filter(Grouping == "SNF")

hh_overall <- overall %>% filter(Grouping == "HH")
hh_va <- va_only %>% filter(Grouping == "HH")
hh_nonva <- nonva %>% filter(Grouping == "HH")
hh_both <- both %>% filter(Grouping == "HH")

tab5_los <- data.frame(
  Measure = c(
    paste0("Acute hospital length of stay (days) (n=", nrow(overall), ")"),
    "    Mean (SD)",
    "    Median [IQR]",
    paste0("IRF Length of stay (days) (n=", nrow(irf_overall), ")"),
    "    Mean (SD)",
    "    Median [IQR]",
    paste0("SNF Length of stay (days) (n=", nrow(snf_overall), ")"),
    "    Mean (SD)",
    "    Median [IQR]",
    paste0("HH duration (days) (n=", nrow(hh_overall), ")"),
    "    Mean (SD)",
    "    Median [IQR]"
  ),
  stringsAsFactors = FALSE
)

tab5_los$`Overall` <- c(
  "",
  paste0(round(mean(overall$acute_los, na.rm = TRUE), 2), " (", round(sd(overall$acute_los, na.rm = TRUE), 2), ")"),
  paste0(round(median(overall$acute_los, na.rm = TRUE), 2), " [", round(quantile(overall$acute_los, 0.25, na.rm = TRUE), 2), ", ", round(quantile(overall$acute_los, 0.75, na.rm = TRUE), 2), "]"),
  "",
  paste0(round(mean(irf_overall$los, na.rm = TRUE), 2), " (", round(sd(irf_overall$los, na.rm = TRUE), 2), ")"),
  paste0(round(median(irf_overall$los, na.rm = TRUE), 2), " [", round(quantile(irf_overall$los, 0.25, na.rm = TRUE), 2), ", ", round(quantile(irf_overall$los, 0.75, na.rm = TRUE), 2), "]"),
  "",
  paste0(round(mean(snf_overall$los, na.rm = TRUE), 2), " (", round(sd(snf_overall$los, na.rm = TRUE), 2), ")"),
  paste0(round(median(snf_overall$los, na.rm = TRUE), 2), " [", round(quantile(snf_overall$los, 0.25, na.rm = TRUE), 2), ", ", round(quantile(snf_overall$los, 0.75, na.rm = TRUE), 2), "]"),
  "",
  paste0(round(mean(hh_overall$los, na.rm = TRUE), 2), " (", round(sd(hh_overall$los, na.rm = TRUE), 2), ")"),
  paste0(round(median(hh_overall$los, na.rm = TRUE), 2), " [", round(quantile(hh_overall$los, 0.25, na.rm = TRUE), 2), ", ", round(quantile(hh_overall$los, 0.75, na.rm = TRUE), 2), "]")
)

tab5_los$`Hospitalized in VA` <- c(
  paste0("(n=", nrow(va_only), ")"),
  paste0(round(mean(va_only$acute_los, na.rm = TRUE), 2), " (", round(sd(va_only$acute_los, na.rm = TRUE), 2), ")"),
  paste0(round(median(va_only$acute_los, na.rm = TRUE), 2), " [", round(quantile(va_only$acute_los, 0.25, na.rm = TRUE), 2), ", ", round(quantile(va_only$acute_los, 0.75, na.rm = TRUE), 2), "]"),
  paste0("(n=", nrow(irf_va), ")"),
  paste0(round(mean(irf_va$los, na.rm = TRUE), 2), " (", round(sd(irf_va$los, na.rm = TRUE), 2), ")"),
  paste0(round(median(irf_va$los, na.rm = TRUE), 2), " [", round(quantile(irf_va$los, 0.25, na.rm = TRUE), 2), ", ", round(quantile(irf_va$los, 0.75, na.rm = TRUE), 2), "]"),
  paste0("(n=", nrow(snf_va), ")"),
  paste0(round(mean(snf_va$los, na.rm = TRUE), 2), " (", round(sd(snf_va$los, na.rm = TRUE), 2), ")"),
  paste0(round(median(snf_va$los, na.rm = TRUE), 2), " [", round(quantile(snf_va$los, 0.25, na.rm = TRUE), 2), ", ", round(quantile(snf_va$los, 0.75, na.rm = TRUE), 2), "]"),
  paste0("(n=", nrow(hh_va), ")"),
  paste0(round(mean(hh_va$los, na.rm = TRUE), 2), " (", round(sd(hh_va$los, na.rm = TRUE), 2), ")"),
  paste0(round(median(hh_va$los, na.rm = TRUE), 2), " [", round(quantile(hh_va$los, 0.25, na.rm = TRUE), 2), ", ", round(quantile(hh_va$los, 0.75, na.rm = TRUE), 2), "]")
)

tab5_los$`Hospitalized in Community/Medicare` <- c(
  paste0("(n=", nrow(nonva), ")"),
  paste0(round(mean(nonva$acute_los, na.rm = TRUE), 2), " (", round(sd(nonva$acute_los, na.rm = TRUE), 2), ")"),
  paste0(round(median(nonva$acute_los, na.rm = TRUE), 2), " [", round(quantile(nonva$acute_los, 0.25, na.rm = TRUE), 2), ", ", round(quantile(nonva$acute_los, 0.75, na.rm = TRUE), 2), "]"),
  paste0("(n=", nrow(irf_nonva), ")"),
  paste0(round(mean(irf_nonva$los, na.rm = TRUE), 2), " (", round(sd(irf_nonva$los, na.rm = TRUE), 2), ")"),
  paste0(round(median(irf_nonva$los, na.rm = TRUE), 2), " [", round(quantile(irf_nonva$los, 0.25, na.rm = TRUE), 2), ", ", round(quantile(irf_nonva$los, 0.75, na.rm = TRUE), 2), "]"),
  paste0("(n=", nrow(snf_nonva), ")"),
  paste0(round(mean(snf_nonva$los, na.rm = TRUE), 2), " (", round(sd(snf_nonva$los, na.rm = TRUE), 2), ")"),
  paste0(round(median(snf_nonva$los, na.rm = TRUE), 2), " [", round(quantile(snf_nonva$los, 0.25, na.rm = TRUE), 2), ", ", round(quantile(snf_nonva$los, 0.75, na.rm = TRUE), 2), "]"),
  paste0("(n=", nrow(hh_nonva), ")"),
  paste0(round(mean(hh_nonva$los, na.rm = TRUE), 2), " (", round(sd(hh_nonva$los, na.rm = TRUE), 2), ")"),
  paste0(round(median(hh_nonva$los, na.rm = TRUE), 2), " [", round(quantile(hh_nonva$los, 0.25, na.rm = TRUE), 2), ", ", round(quantile(hh_nonva$los, 0.75, na.rm = TRUE), 2), "]")
)

tab5_los$`Hospitalized in Both VA and Community` <- c(
  paste0("(n=", nrow(both), ")"),
  paste0(round(mean(both$acute_los, na.rm = TRUE), 2), " (", round(sd(both$acute_los, na.rm = TRUE), 2), ")"),
  paste0(round(median(both$acute_los, na.rm = TRUE), 2), " [", round(quantile(both$acute_los, 0.25, na.rm = TRUE), 2), ", ", round(quantile(both$acute_los, 0.75, na.rm = TRUE), 2), "]"),
  paste0("(n=", nrow(irf_both), ")"),
  paste0(round(mean(irf_both$los, na.rm = TRUE), 2), " (", round(sd(irf_both$los, na.rm = TRUE), 2), ")"),
  paste0(round(median(irf_both$los, na.rm = TRUE), 2), " [", round(quantile(irf_both$los, 0.25, na.rm = TRUE), 2), ", ", round(quantile(irf_both$los, 0.75, na.rm = TRUE), 2), "]"),
  paste0("(n=", nrow(snf_both), ")"),
  paste0(round(mean(snf_both$los, na.rm = TRUE), 2), " (", round(sd(snf_both$los, na.rm = TRUE), 2), ")"),
  paste0(round(median(snf_both$los, na.rm = TRUE), 2), " [", round(quantile(snf_both$los, 0.25, na.rm = TRUE), 2), ", ", round(quantile(snf_both$los, 0.75, na.rm = TRUE), 2), "]"),
  paste0("(n=", nrow(hh_both), ")"),
  paste0(round(mean(hh_both$los, na.rm = TRUE), 2), " (", round(sd(hh_both$los, na.rm = TRUE), 2), ")"),
  paste0(round(median(hh_both$los, na.rm = TRUE), 2), " [", round(quantile(hh_both$los, 0.25, na.rm = TRUE), 2), ", ", round(quantile(hh_both$los, 0.75, na.rm = TRUE), 2), "]")
)


writeData(wb, "Tab5_VA_NonVA", tab5_los, startRow = 1, startCol = 1)

# ==============================================================================
# TAB 6: REHABILITATION SETTING UTILIZATION
# ==============================================================================


cat("  Building Tab 6: Rehab Setting Utilization...\n")
addWorksheet(wb, "Tab6_Rehab_Setting")

tab6_data <- rehab_setting %>%
  group_by(year) %>%
  summarise(
    only_VA = sum(only_VA == "1", na.rm = TRUE),
    only_NonVA = sum(only_NonVA == "1", na.rm = TRUE),
    both = sum(any_VA == "1" & any_NonVA == "1", na.rm = TRUE),
    .groups = "drop"
  )

tab6_rehab <- data.frame(
  Category = c(
    "Received post-acute rehabilitation in VA only, n",
    "Received post-acute rehabilitation in non-VA only, n",
    "Received post-acute in both VA and non-VA, n"
  ),
  stringsAsFactors = FALSE
)

years_rehab <- sort(unique(as.character(rehab_setting$year)))
for(yr in years_rehab) {
  row_data <- tab6_data %>% filter(as.character(year) == yr)
  if(nrow(row_data) > 0) {
    tab6_rehab[[yr]] <- c(
      as.character(row_data$only_VA),
      as.character(row_data$only_NonVA),
      as.character(row_data$both)
    )
  } else {
    tab6_rehab[[yr]] <- c("0", "0", "0")
  }
}

# Add Overall column
tab6_rehab$Overall <- c(
  as.character(sum(tab6_data$only_VA, na.rm = TRUE)),
  as.character(sum(tab6_data$only_NonVA, na.rm = TRUE)),
  as.character(sum(tab6_data$both, na.rm = TRUE))
)

# Add notes
tab6_notes <- data.frame(
  Category = c("", "Notes:", "**post-acute = IRF, SNF, HH", 
               "Capturing Veterans who move in and out of VA for rehabilitation care"),
  stringsAsFactors = FALSE
)

for(col in names(tab6_rehab)[-1]) {
  tab6_notes[[col]] <- ""
}

tab6_rehab_table <- bind_rows(tab6_rehab, tab6_notes)

writeData(wb, "Tab6_Rehab_Setting", tab6_rehab_table, startRow = 1, startCol = 1)

# ==============================================================================
# TAB 7: STRATIFIED ANALYSIS BY ACUTE CARE SETTING
# ==============================================================================

cat("  Building Tab 7: Stratified Analysis...\n")
addWorksheet(wb, "Tab7_Stratified")

# Get years
years_strat <- sort(unique(year(dt_first_post_acute_elig$ADMSNDT)))

# VA hospitalized section
va_data <- analysis_data %>% 
  filter(inpatient_setting == "VA") %>%
  mutate(year = year(ADMSNDT))

tab7_va_discharge <- va_data %>%
  group_by(year, Grouping) %>%
  summarise(n = n(), .groups = "drop")

tab7_va_multi <- rhf_setting_grid %>%
  filter(inpatient_setting == "VA") %>%
  group_by(year) %>%
  summarise(
    multi = round(mean(multi, na.rm = TRUE) * 100, 2),
    IRF_SNF = round(mean(IRF_SNF, na.rm = TRUE) * 100, 2),
    IRF_HH = round(mean(IRF_HH, na.rm = TRUE) * 100, 2),
    SNF_HH = round(mean(SNF_HH, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  )

# Non-VA hospitalized section
nonva_data <- analysis_data %>% 
  filter(inpatient_setting == "Non-VA") %>%
  mutate(year = year(ADMSNDT))

tab7_nonva_discharge <- nonva_data %>%
  group_by(year, Grouping) %>%
  summarise(n = n(), .groups = "drop")

tab7_nonva_multi <- rhf_setting_grid %>%
  filter(inpatient_setting == "Non-VA") %>%
  group_by(year) %>%
  summarise(
    multi = round(mean(multi, na.rm = TRUE) * 100, 2),
    IRF_SNF = round(mean(IRF_SNF, na.rm = TRUE) * 100, 2),
    IRF_HH = round(mean(IRF_HH, na.rm = TRUE) * 100, 2),
    SNF_HH = round(mean(SNF_HH, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  )

# Both VA and Non-VA hospitalized section
both_data <- analysis_data %>% 
  filter(inpatient_setting == "Both") %>%
  mutate(year = year(ADMSNDT))

tab7_both_discharge <- both_data %>%
  group_by(year, Grouping) %>%
  summarise(n = n(), .groups = "drop")

tab7_both_multi <- rhf_setting_grid %>%
  filter(inpatient_setting == "Both") %>%
  group_by(year) %>%
  summarise(
    multi = round(mean(multi, na.rm = TRUE) * 100, 2),
    IRF_SNF = round(mean(IRF_SNF, na.rm = TRUE) * 100, 2),
    IRF_HH = round(mean(IRF_HH, na.rm = TRUE) * 100, 2),
    SNF_HH = round(mean(SNF_HH, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  )

# Build table
tab7_stratified <- data.frame(
  Category = c(
    "Veterans who were hospitalized at VA for stroke",
    "Initial Discharge Location after acute hospitalization, n (%)",
    "    Inpatient Rehabilitation Facility (IRF)",
    "    Skilled Nursing Facility (SNF)",
    "    Home Health (HH)",
    "Percent receiving more than 1 post-acute rehabilitation service",
    "    Overall",
    "    IRF + SNF",
    "    IRF + HH",
    "    SNF + HH",
    "",
    "Veterans who were hospitalized at non-VA for stroke",
    "Initial Discharge Location after acute hospitalization, n (%)",
    "    Inpatient Rehabilitation Facility (IRF)",
    "    Skilled Nursing Facility (SNF)",
    "    Home Health (HH)",
    "Percent receiving more than 1 post-acute service",
    "    Overall",
    "    IRF + SNF",
    "    IRF + HH",
    "    SNF + HH",
    "",
    "Veterans who were hospitalized at both VA and non-VA for stroke",
    "Initial Discharge Location after acute hospitalization, n (%)",
    "    Inpatient Rehabilitation Facility (IRF)",
    "    Skilled Nursing Facility (SNF)",
    "    Home Health (HH)",
    "Percent receiving more than 1 post-acute service",
    "    Overall",
    "    IRF + SNF",
    "    IRF + HH",
    "    SNF + HH"
  ),
  stringsAsFactors = FALSE
)

for(yr in years_strat) {
  va_total <- sum(tab7_va_discharge %>% filter(year == yr) %>% pull(n), na.rm = TRUE)
  va_irf <- tab7_va_discharge %>% filter(year == yr, Grouping == "IRF") %>% pull(n)
  va_snf <- tab7_va_discharge %>% filter(year == yr, Grouping == "SNF") %>% pull(n)
  va_hh <- tab7_va_discharge %>% filter(year == yr, Grouping == "HH") %>% pull(n)
  
  va_multi_row <- tab7_va_multi %>% filter(year == yr)
  
  nonva_total <- sum(tab7_nonva_discharge %>% filter(year == yr) %>% pull(n), na.rm = TRUE)
  nonva_irf <- tab7_nonva_discharge %>% filter(year == yr, Grouping == "IRF") %>% pull(n)
  nonva_snf <- tab7_nonva_discharge %>% filter(year == yr, Grouping == "SNF") %>% pull(n)
  nonva_hh <- tab7_nonva_discharge %>% filter(year == yr, Grouping == "HH") %>% pull(n)
  
  nonva_multi_row <- tab7_nonva_multi %>% filter(year == yr)
  
  both_total <- sum(tab7_both_discharge %>% filter(year == yr) %>% pull(n), na.rm = TRUE)
  both_irf <- tab7_both_discharge %>% filter(year == yr, Grouping == "IRF") %>% pull(n)
  both_snf <- tab7_both_discharge %>% filter(year == yr, Grouping == "SNF") %>% pull(n)
  both_hh <- tab7_both_discharge %>% filter(year == yr, Grouping == "HH") %>% pull(n)
  
  both_multi_row <- tab7_both_multi %>% filter(year == yr)
  
  tab7_stratified[[as.character(yr)]] <- c(
    "",
    "",
    paste0(if(length(va_irf) > 0) va_irf else 0, " (", round((if(length(va_irf) > 0) va_irf else 0)/va_total*100, 2), "%)"),
    paste0(if(length(va_snf) > 0) va_snf else 0, " (", round((if(length(va_snf) > 0) va_snf else 0)/va_total*100, 2), "%)"),
    paste0(if(length(va_hh) > 0) va_hh else 0, " (", round((if(length(va_hh) > 0) va_hh else 0)/va_total*100, 2), "%)"),
    "",
    if(nrow(va_multi_row) > 0) paste0(va_multi_row$multi, "%") else "0%",
    if(nrow(va_multi_row) > 0) paste0(va_multi_row$IRF_SNF, "%") else "0%",
    if(nrow(va_multi_row) > 0) paste0(va_multi_row$IRF_HH, "%") else "0%",
    if(nrow(va_multi_row) > 0) paste0(va_multi_row$SNF_HH, "%") else "0%",
    "",
    "",
    "",
    paste0(if(length(nonva_irf) > 0) nonva_irf else 0, " (", round((if(length(nonva_irf) > 0) nonva_irf else 0)/nonva_total*100, 2), "%)"),
    paste0(if(length(nonva_snf) > 0) nonva_snf else 0, " (", round((if(length(nonva_snf) > 0) nonva_snf else 0)/nonva_total*100, 2), "%)"),
    paste0(if(length(nonva_hh) > 0) nonva_hh else 0, " (", round((if(length(nonva_hh) > 0) nonva_hh else 0)/nonva_total*100, 2), "%)"),
    "",
    if(nrow(nonva_multi_row) > 0) paste0(nonva_multi_row$multi, "%") else "0%",
    if(nrow(nonva_multi_row) > 0) paste0(nonva_multi_row$IRF_SNF, "%") else "0%",
    if(nrow(nonva_multi_row) > 0) paste0(nonva_multi_row$IRF_HH, "%") else "0%",
    if(nrow(nonva_multi_row) > 0) paste0(nonva_multi_row$SNF_HH, "%") else "0%",
    "",
    "",
    "",
    paste0(if(length(both_irf) > 0) both_irf else 0, " (", round((if(length(both_irf) > 0) both_irf else 0)/both_total*100, 2), "%)"),
    paste0(if(length(both_snf) > 0) both_snf else 0, " (", round((if(length(both_snf) > 0) both_snf else 0)/both_total*100, 2), "%)"),
    paste0(if(length(both_hh) > 0) both_hh else 0, " (", round((if(length(both_hh) > 0) both_hh else 0)/both_total*100, 2), "%)"),
    "",
    if(nrow(both_multi_row) > 0) paste0(both_multi_row$multi, "%") else "0%",
    if(nrow(both_multi_row) > 0) paste0(both_multi_row$IRF_SNF, "%") else "0%",
    if(nrow(both_multi_row) > 0) paste0(both_multi_row$IRF_HH, "%") else "0%",
    if(nrow(both_multi_row) > 0) paste0(both_multi_row$SNF_HH, "%") else "0%"
  )
}

writeData(wb, "Tab7_Stratified", tab7_stratified, startRow = 1, startCol = 1)


# ==============================================================================
# SAVE WORKBOOK
# ==============================================================================

cat("\\nSaving workbook...\\n")
saveWorkbook(wb, "Stroke_Rehabilitation_Tables.xlsx", overwrite = TRUE)
cat("\\n=============================================================================\\n")
cat("TABLES CREATED SUCCESSFULLY\\n")
cat("Output file: Stroke_Rehabilitation_Tables.xlsx\\n")
cat("=============================================================================\\n\\n")





























