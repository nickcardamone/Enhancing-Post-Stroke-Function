# CONSORT diagram exclusions script:

cat("\n=============================================================================\n")
cat("SCRIPT 06: CONSORT diagram exclusions script")
cat("=============================================================================\n\n")

source("00_config.R")


# Load required packages
load_packages(c("dplyr", "dbplyr", "DBI", "VINCI", "arrow", "data.table",
                "table1", "stringr", "tidyr", "ggplot2", "scales", "tidytable", "kableExtra"))

# Set working directory
setwd(project_base)
con <- connect_db(database = db_project, server = db_server)

# Load parquet files

# Exclusions steps:
# Start: All veterans hospitalized with a primary discharge diagnosis of stroke Jan 1 2019- Dec 31 2022
# Exclusion 1: Limit to veterans found in the RHF 
# Exclusion 2: Limit to hospitalizations that have a matching RHF inpatient episode (+/- 1 day overlap) and are followed by IRF/SNF or HH within 3 days or 14 days of hospital discharge 
# Exclusion 3: Limit to hospitalizations that satisfy the following criteria: 
#               - not readmitted from post-acute care stay
#               - did not die during post-acute care stay or witihn one day of discharge or did not have "death" as reason for no OASIS/MDS/IRFPAI assessment followup data
#               - did not have "unplanned discharge" listed on their OASIS/MDS/IRFPAI
#               - post-acute care episodes were not all too short (3 days or fewer)
#               - patient was not younger than 65 or older than 99 at the time of hospitalization
#               - acute hospitalization was not more than 365 days.
# Exclusion 4: Limit to hospitalizations that satisfy the following criteria:
#               - no matched assessment record (GG data)
#               - Post-acute care was not VA SNF only
#               - Post-acute care was not VA Purchase Home Health only
#               - Post-acute care was not VA IRF only (those without assessment data)
# Exclusion 5: Limit to hospitalizations with at least one post-acute episode with at least one GG item at admission or discharge with a valid score (i.e. 01, 02, 03, 04, 05, 06).
# Exclusion 6: For patients with more than one eligible hospitalization (and subsequent post-acute care stay) at this point, randomly select one hospitalization.

con <- connect_db(database = db_project, server = db_server)

elig_rhf <- load_parquet_safe("dt_post_acute_elig.parquet") %>%
  mutate(setting_series = map_chr(settings, ~paste(.x, collapse = "|")))

start <- load_parquet_safe("pdx_stroke_visit_summary.parquet") 

start <- start%>% mutate(scrssn = SCRSSN, ADMSNDT = ACUTE_INPATIENT_VISIT_START)

groupings_file <- file.path(project_base, "Groupings.xlsx")
Groupings <- readxl::read_xlsx(groupings_file) %>% 
  mutate(Grouping = if_else(Grouping == "EXCL", "OTHER", Grouping))

RHFB <- tbl(con, in_schema(schema_dflt, 'RHFB_SHIP2393')) %>% collect() %>% left_join(Groupings, by = c("hee_type1" = "type1"))

exclusion1 <- start %>% inner_join(RHFB %>% transmute(scrssn) %>% distinct(),  by = "scrssn")

exclusion2 <- load_parquet_safe("rhf_cohort_included.parquet") 

exclusion2 <- exclusion2 %>% mutate(SCRSSN = scrssn)

elig_rhf_data_assessment_final <- load_parquet_safe("elig_rhf_data_assessment_final.parquet")
exclusion3 <- load_parquet_safe("elig_rhf_data_assessment_final_epis_excl.parquet")
exclusion4 <- load_parquet_safe("elig_rhf_data_assessment_final_epis_excl1.parquet")
exclusion5 <- load_parquet_safe("elig_rhf_data_assessment_final_epis_excl2.parquet")

analysis_data <- load_parquet_safe("analysis_data.parquet")
## Create Episode-Level Dataset

# Arrange by patient, admission date, and episode run
analysis_data <- analysis_data %>% 
  arrange(SCRSSN, ADMSNDT, desc(episode_run)) %>%
  group_by(SCRSSN, ADMSNDT) %>%
  mutate(total_post_acute = max(episode_run) - 1) %>%
  ungroup()

## Create Patient-Level Dataset (One Episode Per Patient)
# Take admission scores from earliest episode, discharge from latest
admsn_vars <- names(analysis_data)[grepl("_ADMSN$", names(analysis_data))]

analysis_data_final <- analysis_data %>%
  group_by(SCRSSN, ADMSNDT) %>%
  summarize(
    across(all_of(admsn_vars), ~.[episode_run == min(episode_run)][[1]]),
    across(!all_of(c(admsn_vars)), ~.[episode_run == max(episode_run)][[1]]),
    .groups = "drop"
  )

# Identify patients with multiple hospitalizations
patient_episode_counts <- analysis_data_final %>%
  group_by(SCRSSN) %>%
  summarise(n_episodes = n(), .groups = "drop")

# Summary of multiple episodes
cat("\nPatients with multiple episodes:\n")
table(patient_episode_counts$n_episodes) %>% 
  as.data.frame() %>%
  setNames(c("Number of Episodes", "Number of Patients")) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Randomly select one episode per patient
set.seed(12345)
exclusion6 <- analysis_data_final %>%
  left_join(patient_episode_counts, by = "SCRSSN") %>%
  group_by(SCRSSN) %>%
  slice_sample(n = 1) %>%
  ungroup()

cat("\nPatient-level dataset created:", nrow(exclusion6), "unique patients\n")
cat("Excluded episodes:", nrow(analysis_data_final) - nrow(exclusion6), "\n")

# Functions:

count_people_hosp <- function(df) {
  list(
    people = n_distinct(df$SCRSSN),
    hospitalizations = df %>% distinct(SCRSSN, ADMSNDT) %>% nrow()
  )
}

format_people_hosp <- function(people, hosp) {
  paste0(format_big(people), " people, ", format_big(hosp), " hospitalizations")
}

diff_counts <- function(before, after) {
  list(
    people = before$people - after$people,
    hospitalizations = before$hospitalizations - after$hospitalizations
  )
}

build_exclusion_text <- function(people, hosp, reasons = NULL) {
  main <- paste0("Excluded: ", format_people_hosp(people, hosp))
  if (!is.null(reasons) && length(reasons) > 0) {
    main <- paste0(main, " ", paste(reasons, collapse = "; "))
  }
  main
}


# Helper functions
format_big <- function(x) {
  format(x, big.mark = ",", scientific = FALSE)
}


# ===========================================================================
# Count at each step
# ==============================================================================

step_counts_total <- rbind.data.frame(
  count_people_hosp(start),
  count_people_hosp(exclusion1),
  count_people_hosp(exclusion2),
  count_people_hosp(exclusion3),
  count_people_hosp(exclusion4),
  count_people_hosp(exclusion5),
  count_people_hosp(exclusion6)
)

# Count at each step
step_counts <- list(
  A = count_people_hosp(start),
  B = count_people_hosp(exclusion1),
  C = count_people_hosp(exclusion2),
  D = count_people_hosp(exclusion3),
  E = count_people_hosp(exclusion4),
  `F` = count_people_hosp(exclusion5),
  G = count_people_hosp(exclusion6)
)

step_diffs <- list(
  AB = diff_counts(step_counts$A, step_counts$B),
  BC = diff_counts(step_counts$B, step_counts$C),
  CD = diff_counts(step_counts$C, step_counts$D),
  DE = diff_counts(step_counts$D, step_counts$E),
  EF = diff_counts(step_counts$E, step_counts$`F`),
  FG = diff_counts(step_counts$`F`, step_counts$G)
)

# Build exclusion text for each step
stepA_exclusions <- build_exclusion_text(
  step_diffs$AB$people, step_diffs$AB$hospitalizations, "Hospitalizations found in RHF data (any encounter)")

stepB_exclusions <- build_exclusion_text(step_diffs$BC$people, step_diffs$BC$hospitalizations, "Hospitalizations with IRF/SNF discharge =3 days or HH =14 days")

hosp_excl_flags <- elig_rhf_data_assessment_final %>% 
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
    excl_hosp = if_else(readmitted == 1 | Too_Short_Only == 1 | Age_OB == 1 |  Inpatient_Too_Long == 1 |  died == 1 | discharge_unpl == 1, 
                        1, 0
    )
  ) 

# Summarize flags by hospitalization:
hosp_excl_summary <- hosp_excl_flags %>% 
  dplyr::group_by(SCRSSN, ADMSNDT, INP_hee_thru) %>%
  dplyr::summarize( 
    excl_hosp = max(excl_hosp), 
    readmitted = max(readmitted), 
    Too_Short_Only= max(Too_Short_Only), 
    Age_OB = max(Age_OB), 
    Inpatient_Too_Long = max(Inpatient_Too_Long), 
    died= max(died), , 
    discharge_unpl= max(discharge_unpl)) %>%
  ungroup()

# Count each hospitalization-level exclusion reason
hosp_excl_summary <- hosp_excl_summary %>%
  dplyr::summarize(
    total_excluded = sum(excl_hosp == 1, na.rm = TRUE),
    n_readmitted = sum(readmitted == 1, na.rm = TRUE),
    n_too_short = sum(Too_Short_Only == 1, na.rm = TRUE),
    n_age_ob = sum(Age_OB == 1, na.rm = TRUE),
    n_inp_too_long = sum(Inpatient_Too_Long == 1, na.rm = TRUE),
    n_died = sum(died == 1, na.rm = TRUE),
    n_unpl_disch = sum(discharge_unpl == 1, na.rm = TRUE)
  )

stepC_exclusion_reasons <- c(
  paste0("Readmitted to acute care within 1 day: ", format_big(hosp_excl_summary$n_readmitted)),
  paste0("Died on assessment: ", format_big(hosp_excl_summary$n_died)),
  paste0("Unplanned discharge: ", format_big(hosp_excl_summary$n_unpl_disch)),
  paste0("Episode ≤3 days (too short for rehab): ", format_big(hosp_excl_summary$n_too_short)),
  paste0("Age >99 or <65: ", format_big(hosp_excl_summary$n_age_ob)),
  paste0("Acute hospitalization >365 days: ", format_big(hosp_excl_summary$n_inp_too_long))
)

stepC_exclusions <- build_exclusion_text(
  step_diffs$CD$people, step_diffs$CD$hospitalizations,
  stepC_exclusion_reasons
)

# --- Step D→E: Episode-level exclusions Level 1 (detailed) ---
# Reconstruct episode-level flags from elig_rhf_data_assessment_final_epis_excl
epis_lv1_flags <- exclusion3 %>%
  ungroup() %>%
  mutate(
    no_record = if_else(is.na(record_id_ADMSN) & is.na(record_id_DSCHRG), 1, 0),
    IRF_VA_flag2 = if_else(IRF_VA_flag == 1 & no_record == 1, 1, 0),
    excl_episode_lv1 = if_else(
      no_record == 1 | HH_VA_flag == 1 | SNF_VA_flag == 1 | IRF_VA_flag2 == 1,
      1, 0
    )
  ) %>% distinct()

epis_lv1_summary <- epis_lv1_flags %>%
  summarize(
    total_excluded = sum(excl_episode_lv1 == 1, na.rm = TRUE),
    n_no_record = sum(no_record == 1, na.rm = TRUE),
    n_va_snf = sum(SNF_VA_flag == 1, na.rm = TRUE),
    n_va_hh = sum(HH_VA_flag == 1, na.rm = TRUE),
    n_va_irf_no_data = sum(IRF_VA_flag2 == 1, na.rm = TRUE)
  )

stepD_exclusion_reasons <- c(
  paste0("No matched assessment record (no GG data): ", format_big(epis_lv1_summary$n_no_record)),
  paste0("VA SNF with no external assessment data: ", format_big(epis_lv1_summary$n_va_snf)),
  paste0("VA HH (purchased home health only): ", format_big(epis_lv1_summary$n_va_hh)),
  paste0("VA IRF with no assessment data: ", format_big(epis_lv1_summary$n_va_irf_no_data))
)

stepD_exclusions <- build_exclusion_text(
  step_diffs$DE$people, step_diffs$DE$hospitalizations,
  stepD_exclusion_reasons
)

# --- Step E→F: Episode-level exclusions Level 2 (detailed) ---
epis_lv2_flags <- exclusion4 %>%
  ungroup() %>%
  mutate(
    no_valid_items = coalesce(
      if_else(count_valid_ALL_ADMSN == 0 & count_valid_ALL_DSCHRG == 0, 1, 0), 0
    )
  )  %>% distinct()

epis_lv2_summary <- epis_lv2_flags %>%
  summarize(
    n_no_valid = sum(no_valid_items == 1, na.rm = TRUE)
  )

stepE_exclusion_reasons <- c(
  paste0("No GG items with a valid score (01-06): ", format_big(epis_lv2_summary$n_no_valid))
)

stepE_exclusions <- build_exclusion_text(
  step_diffs$EF$people, step_diffs$EF$hospitalizations,
  stepE_exclusion_reasons
)

# --- Step F→G: Patient-level selection ---
stepF_exclusions <- build_exclusion_text(
  step_diffs$FG$people, step_diffs$FG$hospitalizations,
  c(paste0("Random selection: 1 episode per patient. ",
           "Patients with multiple episodes: ",
           sum(patient_episode_counts$n_episodes > 1)))
)


#step_counts %>% flextable::flextable()
start <- data.frame(excl = "Hospitalizations with primary diagnosis of stroke")
stepA <- data.frame(excl = stepA_exclusions)
stepB <- data.frame(excl = stepB_exclusions)
stepC <- data.frame(excl = stepC_exclusions)
stepD <-  data.frame(excl = stepD_exclusions)
stepE <- data.frame(excl = stepE_exclusions)
stepF <- data.frame(excl = stepF_exclusions)


excl <- rbind.data.frame(start, stepA, stepB, stepC, stepD, stepE, stepF)

consortdf <- cbind(step_counts_total, excl) 
consortdf %>% flextable::flextable() %>% flextable::autofit()




