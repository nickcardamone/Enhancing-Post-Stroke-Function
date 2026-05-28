
# RACE
# Additional analysis to try and complete the missing race/ethnicity using MSBF files.

# Source configuration
source("00_config.R")

# Load required packages
load_packages(c("dplyr", "dbplyr", "DBI", "VINCI", "arrow", "data.table",
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


# Load eligible cohort
analysis_data <- load_parquet_safe("analysis_data.parquet") 


# Load full cohort
#cohort_full <- load_parquet_safe("rhf_post_acute_final.parquet")
#pdx_stroke_visit_summary <- load_parquet_safe("pdx_stroke_visit_summary.parquet")

# Extract key stroke admission information
#pdx_stroke_visit_summary <- pdx_stroke_visit_summary %>% 
#  transmute(
#    scrssn = SCRSSN, 
#    ADMSNDT = ACUTE_INPATIENT_VISIT_START, 
#    discharge_SOURCE
#  ) %>% 
#  distinct()

## Create cohort with one row per hospitalization
#cat("Creating cohort with lookback periods...\n")
#cohort <- cohort_full %>%
#  select(PatientICN, scrssn, PERSON_ID, ADMSNDT, DSCHRGDT, hee_from, hee_thru) %>%
#  group_by(PatientICN, scrssn, PERSON_ID, ADMSNDT) %>%
#  slice(1) %>%
#  ungroup() %>%
#  mutate(
#    hosp_start = if_else(hee_from < ADMSNDT, hee_from, ADMSNDT),
#    year_prior_to_admission = hosp_start %m-% years(1),
#    twoyears_prior_to_admission = hosp_start %m-% years(2)
#  ) %>%
#  left_join(pdx_stroke_visit_summary, by = c("scrssn", "ADMSNDT"))

#log_count(cohort, "Cohort with lookback periods")

# ==============================================================================
# DATABASE CONNECTIONS
# ==============================================================================

#cat("\nEstablishing database connections...\n")
con <- connect_db(database = db_project, server = db_server)
cdwwork <- connect_db(database = db_cdwwork, server = db_server)

# Load OMOP concept reference table
omop_concept <- tbl(con, in_schema(schema_src, 'OMOPV5_CONCEPT'))

# ==============================================================================
# CREATE CROSSWALK TABLES
# ==============================================================================

cat("Creating patient crosswalk tables...\n")

# Load OMOP crosswalk: Patient ICN to PERSON_ID
cohort <- tbl(con, in_schema(schema_src, 'OMOPV5Map_SPatient_PERSON')) %>% 
  select(PatientICN, PERSON_ID) %>%
  inner_join(analysis_data %>% select(PatientICN) %>% distinct(), by = "PatientICN", copy = T)

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
    cohort %>% select(PERSON_ID, PatientICN) %>% distinct(), 
    by = "PERSON_ID", 
    copy = TRUE
  ) %>% 
  left_join(
    omop_concept %>% transmute(CONCEPT_ID, Race = CONCEPT_NAME), 
    by = c("RACE_CONCEPT_ID" = "CONCEPT_ID")
  ) %>% 
  left_join(
    omop_concept %>% transmute(CONCEPT_ID, Ethnicity = CONCEPT_NAME), 
    by = c("ETHNICITY_CONCEPT_ID" = "CONCEPT_ID")
  ) 

omop_person_cdw <- omop_person_cdw %>% 
  transmute(
    PatientICN, 
    Unknown = if_else(Race == "No matching concept", 1L, 0L),
    White = if_else(Race == "White", 1L, 0L),
    Black = if_else(Race == "Black or African American", 1L, 0L),
    Asian_PI = if_else(Race == "Asian" | Race == "Native Hawaiian or Other Pacific Islander", 1L, 0L),
    American_Indian_AN = if_else(Race == "American Indian or Alaska Native", 1L, 0L),
    Hispanic = if_else(Ethnicity == "Hispanic or Latino", 1L, 0L),
    NotHispanic = if_else(Ethnicity == "Not Hispanic or Latino", 1L, 0L),
    UnkHispanic = if_else(is.na(Ethnicity) | Ethnicity == "No matching concept", 1L, 0L)
    ) %>%
  collect()

cdw_race <- omop_person_cdw %>% 
  group_by(PatientICN) %>%
  summarize(Unknown = sum(Unknown),
            White = sum(White),
            Black = sum(Black),
            Asian_PI = sum(Asian_PI),
            American_Indian_AN = sum(American_Indian_AN)) %>%
  pivot_longer(cols = Unknown:American_Indian_AN, values_to = "count", names_to = "race") %>% 
  mutate(pref = case_when(
                          race == "American_Indian_AN" ~ 1,
                          race == "Asian_PI" ~ 2,
                          race == "Black" ~ 3,
                          race == "White" ~ 4,
                          race ==  "Unknown" ~ 5), 
    unk = if_else(race == "Unknown", 1, 0)) %>% 
  filter(count > 0) %>%
  arrange(PatientICN, unk, desc(count), pref) %>% 
  group_by(PatientICN) %>%
  slice_head(n=1) %>%
  transmute(PatientICN, cdw_race = if_else(race == "Unknown", NA, race))

cdw_eth <- omop_person_cdw %>% 
  group_by(PatientICN) %>%
  summarize(Hispanic = sum(Hispanic),
            NotHispanic = sum(NotHispanic),
            UnkHispanic = sum(UnkHispanic)) %>%
  pivot_longer(cols = Hispanic:UnkHispanic, values_to = "count", names_to = "eth") %>% 
  mutate(unk = if_else(eth=="UnkHispanic", 1, 0)) %>% 
  filter(count > 0) %>%
  arrange(PatientICN, unk, desc(count), eth) %>% 
  group_by(PatientICN) %>%
  slice_head(n=1) %>%
  transmute(PatientICN, cdw_eth = if_else(eth == "UnkHispanic", NA, eth))
    
table1(~cdw_race, data = cdw_race %>% mutate(cdw_race = as.factor(cdw_race)))
table1(~cdw_eth, data = cdw_eth %>% mutate(cdw_eth = as.factor(cdw_eth)))

log_count(omop_person_cdw, "CDW demographics")

# Extract demographics from CMS OMOP
cat("Extracting CMS demographics...\n")
omop_person_cms <- tbl(con, in_schema(schema_src, 'OMOP_CMS_PERSON_FF291')) %>% 
  inner_join(
    cohort %>% select(PERSON_ID, PatientICN) %>% distinct(), 
    by = "PERSON_ID", 
    copy = TRUE
  ) %>% 
  left_join(
    omop_concept %>% transmute(CONCEPT_ID, Race = CONCEPT_NAME), 
    by = c("RACE_CONCEPT_ID" = "CONCEPT_ID")
  ) %>% 
  left_join(
    omop_concept %>% transmute(CONCEPT_ID, Ethnicity = CONCEPT_NAME), 
    by = c("ETHNICITY_CONCEPT_ID" = "CONCEPT_ID")
  ) %>% 
  transmute(
    PatientICN, 
    Unknown = if_else(Race == "No matching concept", 1L, 0L),
    White = if_else(Race == "White", 1L, 0L),
    Black = if_else(Race == "Black or African American", 1L, 0L),
    Asian_PI = if_else(Race == "Asian", 1L, 0L),
    American_Indian_AN = if_else(Race == "American Indian or Alaska Native", 1L, 0L),
    Hispanic = if_else(Ethnicity == "Hispanic or Latino", 1L, 0L),
    UnkHispanic = if_else(is.na(Ethnicity) | Ethnicity == "No matching concept", 1L, 0L)
    ) %>%
  collect()

cms_race <- omop_person_cms %>% 
  group_by(PatientICN) %>%
  summarize(Unknown = sum(Unknown),
            White = sum(White),
            Black = sum(Black),
            Asian_PI = sum(Asian_PI),
            American_Indian_AN = sum(American_Indian_AN)) %>%
  pivot_longer(cols = Unknown:American_Indian_AN, values_to = "count", names_to = "race") %>% 
  mutate(pref = case_when(
    race == "American_Indian_AN" ~ 1,
    race == "Asian_PI" ~ 2,
    race == "Black" ~ 3,
    race == "White" ~ 4,
    race ==  "Unknown" ~ 5), 
    unk = if_else(race == "Unknown", 1, 0)) %>% 
  filter(count > 0) %>%
  arrange(PatientICN, unk, desc(count), pref) %>% 
  group_by(PatientICN) %>%
  slice_head(n=1) %>%
  transmute(PatientICN, cms_race = if_else(race == "Unknown", NA, race))

cms_eth <- omop_person_cms %>% 
  group_by(PatientICN) %>%
  summarize(Hispanic = sum(Hispanic),
            UnkHispanic = sum(UnkHispanic)) %>%
  pivot_longer(cols = Hispanic:UnkHispanic, values_to = "count", names_to = "eth") %>% 
  mutate(unk = if_else(eth=="UnkHispanic", 1, 0)) %>% 
  filter(count > 0) %>%
  arrange(PatientICN, unk, desc(count), eth) %>% 
  group_by(PatientICN) %>%
  slice_head(n=1) %>%
  transmute(PatientICN,cms_eth = if_else(eth == "UnkHispanic", NA, eth))

table1(~cms_race, data = cms_race %>% mutate(cms_race = as.factor(cms_race)))
table1(~cms_eth, data = cms_eth %>% mutate(cms_eth = as.factor(cms_eth)))

# ==============================================================================
# MODULE 2: MEDICARE DEMOGRAPHICS (MBSF)
# ==============================================================================

# Summarize MBSF demographics (handle multiple years per patient)
cat("Summarizing MBSF demographics...\n")
# update 1/27/2026: RTI_RACE has the following categories:
# 0 "Unknown"
# 1 = "Non-Hispanic White"
# 2 = "Black or African American (non-Hispanic)"
# 3 = "Other (non-Hispanic)"
# 4 = "Asian/Pacific Islander (non-Hispanic)"
# 5 = "Hispanic"
# 6 = "American Indian / Alaska Native (non-Hispanic)"

MBSF1822_race <- tbl(con, in_schema(schema_dflt, "MBSF_1822")) %>%
  transmute(
    PatientICN, 
    Unknown = if_else(RACE == "0", 1L, 0L),
    White = if_else(RACE == "1", 1L, 0L),
    Black = if_else(RACE == "2", 1L, 0L),
    Other = if_else(RACE == "3", 1L, 0L),
    Asian_PI = if_else(RACE == "4", 1L, 0L),
    American_Indian_AN = if_else(RACE == "6", 1L, 0L),
    Hispanic = if_else(RACE == "5", 1L, 0L),
    NotHispanic = if_else(RACE %in% c("1", "2", "3", "4", "6"), 1L, 0L),
    UnkHispanic = if_else(RACE == "0", 1L, 0L),
  ) %>%
  collect() %>%
  inner_join(
    analysis_data %>% select(PatientICN) %>% distinct(), 
    by = "PatientICN"
  )

mbsf_race <- MBSF1822_race %>% 
  group_by(PatientICN) %>%
  summarize(Unknown = sum(Unknown),
            White = sum(White),
            Black = sum(Black),
            Asian_PI = sum(Asian_PI),
            Other = sum(Other),
            American_Indian_AN = sum(American_Indian_AN)) %>%
  pivot_longer(cols = Unknown:American_Indian_AN, values_to = "count", names_to = "race") %>% 
  mutate(pref = case_when(
    race == "American_Indian_AN" ~ 1,
    race == "Asian_PI" ~ 2,
    race == "Black" ~ 3,
    race == "White" ~ 4,
    race == "Other" ~ 5,
    race ==  "Unknown" ~ 6), 
    unk = if_else(race == "Unknown", 1, 0)) %>% 
  filter(count > 0) %>%
  arrange(PatientICN, unk, desc(count), pref) %>% 
  group_by(PatientICN) %>%
  slice_head(n=1) %>%
  transmute(PatientICN, mbsf_race = if_else(race == "Unknown", NA, race))

mbsf_eth <- MBSF1822_race %>% 
  group_by(PatientICN) %>%
  summarize(Hispanic = sum(Hispanic),
            NotHispanic = sum(NotHispanic),
            UnkHispanic = sum(UnkHispanic)) %>%
  pivot_longer(cols = Hispanic:UnkHispanic, values_to = "count", names_to = "eth") %>% 
  mutate(unk = if_else(eth=="UnkHispanic", 1, 0)) %>% 
  filter(count > 0) %>%
  arrange(PatientICN, unk, desc(count), eth) %>% 
  group_by(PatientICN) %>%
  slice_head(n=1) %>%
  transmute(PatientICN, mbsf_eth = if_else(eth == "UnkHispanic", NA, eth))

table1(~mbsf_race, data = mbsf_race %>% mutate(mbsf_race = as.factor(mbsf_race)))
table1(~mbsf_eth, data = mbsf_eth %>% mutate(mbsf_eth = as.factor(mbsf_eth)))

MBSF1822_RTI_RACE <- tbl(con, in_schema(schema_dflt, "MBSF_1822")) %>%
  transmute(
    PatientICN, 
    Unknown = if_else(RTI_RACE_CD == "0", 1L, 0L),
    White = if_else(RTI_RACE_CD == "1", 1L, 0L),
    Black = if_else(RTI_RACE_CD == "2", 1L, 0L),
    Other = if_else(RTI_RACE_CD == "3", 1L, 0L),
    Asian_PI = if_else(RTI_RACE_CD == "4", 1L, 0L),
    American_Indian_AN = if_else(RTI_RACE_CD == "6", 1L, 0L),
    Hispanic = if_else(RTI_RACE_CD == "5", 1L, 0L),
    NotHispanic = if_else(RTI_RACE_CD %in% c("1", "2", "3", "4", "6"), 1L, 0L),
    UnkHispanic = if_else(RTI_RACE_CD == "0", 1L, 0L),
  ) %>%
  collect() %>%
  inner_join(
    analysis_data %>% select(PatientICN) %>% distinct(), 
    by = "PatientICN"
  )

mbsf_rti_race <- MBSF1822_RTI_RACE %>% 
  group_by(PatientICN) %>%
  summarize(Unknown = sum(Unknown),
            White = sum(White),
            Black = sum(Black),
            Asian_PI = sum(Asian_PI),
            Other = sum(Other),
            American_Indian_AN = sum(American_Indian_AN)) %>%
  pivot_longer(cols = Unknown:American_Indian_AN, values_to = "count", names_to = "race") %>% 
  mutate(pref = case_when(
    race == "American_Indian_AN" ~ 1,
    race == "Asian_PI" ~ 2,
    race == "Black" ~ 3,
    race == "White" ~ 4,
    race == "Other" ~ 5,
    race ==  "Unknown" ~ 6), 
    unk = if_else(race == "Unknown", 1, 0)) %>% 
  filter(count > 0) %>%
  arrange(PatientICN, unk, desc(count), pref) %>% 
  group_by(PatientICN) %>%
  slice_head(n=1) %>%
  transmute(PatientICN, mbsf_rti_race = if_else(race == "Unknown", NA, race))

mbsf_rti_eth <- MBSF1822_RTI_RACE %>% 
  group_by(PatientICN) %>%
  summarize(Hispanic = sum(Hispanic),
            NotHispanic = sum(NotHispanic),
            UnkHispanic = sum(UnkHispanic)) %>%
  pivot_longer(cols = Hispanic:UnkHispanic, values_to = "count", names_to = "eth") %>% 
  mutate(unk = if_else(eth=="UnkHispanic", 1, 0)) %>% 
  filter(count > 0) %>%
  arrange(PatientICN, unk, desc(count), eth) %>% 
  group_by(PatientICN) %>%
  slice_head(n=1) %>%
  transmute(PatientICN, mbsf_rti_eth = if_else(eth == "UnkHispanic", NA, eth))

table1(~mbsf_rti_race, data = mbsf_rti_race %>% mutate(mbsf_rti_race = as.factor(mbsf_rti_race)))
table1(~mbsf_rti_eth, data = mbsf_rti_eth %>% mutate(mbsf_rti_eth = as.factor(mbsf_rti_eth)))

# ==============================================================================
# MODULE 3: MDS, OASIS, IRFPAI race / ethnicity
# ==============================================================================

mds1922 <- tbl(con, in_schema(schema_dflt, 'MDS_1922'))

# Race in the MDS is a "check all that apply"
## American Indian or Alaska Native
## Asian
## Black or African American
## Hispanic or Latino
## Native Hawaiian or Other Pacific Islander
## White

mds1922_raceeth <- mds1922 %>%
  select(
    SCRSSN,
    starts_with("A1000")) %>%
  collect() %>%
  mutate(source = "MDS",
         SCRSSN = as.character(sprintf("%09d", as.integer(SCRSSN)))
  ) %>% 
  inner_join(
      analysis_data %>% select(SCRSSN) %>% distinct(), 
      by = "SCRSSN"
    )

mds1922_raceethf <- mds1922_raceeth %>% 
  transmute(SCRSSN,
            source,
            Unknown = if_else(A1000F_WHT_CD == "-" | A1000C_AFRCN_AMRCN_CD == "-" | A1000B_ASN_CD == "-" | A1000D_HSPNC_CD == "-" |  A1000A_AMRCN_INDN_AK_NTV_CD == "-", 1L, 0L),
            White = if_else(A1000F_WHT_CD == "1", 1L, 0L),
            Black = if_else(A1000C_AFRCN_AMRCN_CD == "1", 1L, 0L),
            Asian_PI = if_else(A1000B_ASN_CD == "1", 1L, 0L),
            American_Indian_AN = if_else(A1000A_AMRCN_INDN_AK_NTV_CD == "1", 1L, 0L),
            Hispanic = if_else(A1000D_HSPNC_CD == "1", 1L, 0L),
            UnkHispanic = if_else(A1000D_HSPNC_CD == "-", 1L, 0L)
            
  )

mds1922_race <- mds1922_raceethf %>% 
  group_by(SCRSSN) %>%
  summarize(Unknown = sum(Unknown),
            White = sum(White),
            Black = sum(Black),
            Asian_PI = sum(Asian_PI),
            American_Indian_AN = sum(American_Indian_AN)) %>%
  pivot_longer(cols = Unknown:American_Indian_AN, values_to = "count", names_to = "race") %>% 
  mutate(pref = case_when(
    race == "American_Indian_AN" ~ 1,
    race == "Asian_PI" ~ 2,
    race == "Black" ~ 3,
    race == "White" ~ 4,
    race ==  "Unknown" ~ 5), 
    unk = if_else(race == "Unknown", 1, 0)) %>% 
  filter(count > 0) %>%
  arrange(SCRSSN, unk, desc(count), pref) %>% 
  group_by(SCRSSN) %>%
  slice_head(n=1)  %>%
  transmute(SCRSSN, race = if_else(race == "Unknown", NA, race))

mds1922_eth <- mds1922_raceethf %>% 
  group_by(SCRSSN) %>%
  summarize(Hispanic = sum(Hispanic),
            UnkHispanic = sum(UnkHispanic)) %>%
  pivot_longer(cols = Hispanic:UnkHispanic, values_to = "count", names_to = "eth") %>% 
  mutate(unk = if_else(eth=="UnkHispanic", 1, 0)) %>% 
  filter(count > 0) %>%
  arrange(SCRSSN, unk, desc(count), eth) %>% 
  group_by(SCRSSN) %>%
  slice_head(n=1) %>%
  transmute(SCRSSN, eth = if_else(eth == "UnkHispanic", NA, eth))

table1(~race, data = mds_race %>% mutate(race = as.factor(race)))
table1(~eth, data = mds_eth %>% mutate(eth = as.factor(eth)))


# OASIS:
# Also a "mark all that apply"

oasis1922_raceeth <- tbl(con, in_schema(schema_dflt, 'OASIS_1922')) %>% 
  collect() %>%
  mutate(
    source = "OASIS",
    SCRSSN = as.character(sprintf("%09d", as.integer(SCRSSN)))
  ) %>% 
  inner_join(analysis_data %>% select(SCRSSN) %>% distinct(), by = 'SCRSSN') %>% 
  select(
    SCRSSN,
    source,
    starts_with("M0140")   # Mobility codes
  )

oasis1922_raceethf <- oasis1922_raceeth %>% 
  transmute(SCRSSN,
            source,
            Unknown = if_else(is.na(M0140_ETHNIC_WHITE) &
                                is.na(M0140_ETHNIC_BLACK) &
                                is.na(M0140_ETHNIC_ASIAN) & 
                                is.na(M0140_ETHNIC_NH_PI)& is.na(M0140_ETHNIC_AI_AN), 1L, 0L),
            White = if_else(!is.na(M0140_ETHNIC_WHITE) & M0140_ETHNIC_WHITE == "1", 1L, 0L),
            Black = if_else(!is.na(M0140_ETHNIC_BLACK) & M0140_ETHNIC_BLACK == "1", 1L, 0L),
            Asian_PI = if_else((!is.na(M0140_ETHNIC_WHITE) & M0140_ETHNIC_ASIAN == "1") | (!is.na(M0140_ETHNIC_NH_PI) & M0140_ETHNIC_NH_PI == "1"), 1L, 0L),
            Hispanic = if_else(!is.na(M0140_ETHNIC_HISP) & M0140_ETHNIC_HISP == "1", 1L, 0L),
            UnkHispanic = if_else(is.na(M0140_ETHNIC_HISP), 1L, 0L),
            American_Indian_AN = if_else(!is.na(M0140_ETHNIC_AI_AN) & M0140_ETHNIC_AI_AN == "1", 1L, 0L))

oasis1922_race <- oasis1922_raceethf %>% 
  group_by(SCRSSN) %>%
  summarize(Unknown = sum(Unknown),
            White = sum(White),
            Black = sum(Black),
            Asian_PI = sum(Asian_PI),
            American_Indian_AN = sum(American_Indian_AN)) %>%
  pivot_longer(cols = Unknown:American_Indian_AN, values_to = "count", names_to = "race") %>% 
  mutate(pref = case_when(
    race == "American_Indian_AN" ~ 1,
    race == "Asian_PI" ~ 2,
    race == "Black" ~ 3,
    race == "White" ~ 4,
    race ==  "Unknown" ~ 5), 
    unk = if_else(race == "Unknown", 1, 0)) %>% 
  filter(count > 0) %>%
  arrange(SCRSSN, unk, desc(count), pref) %>% 
  group_by(SCRSSN) %>%
  slice_head(n=1) %>%
  transmute(SCRSSN, race = if_else(race == "Unknown", NA, race))

oasis1922_eth <- oasis1922_raceethf %>% 
  group_by(SCRSSN) %>%
  summarize(Hispanic = sum(Hispanic),
            UnkHispanic = sum(UnkHispanic)) %>%
  pivot_longer(cols = Hispanic:UnkHispanic, values_to = "count", names_to = "eth") %>% 
  mutate(unk = if_else(eth=="UnkHispanic", 1, 0)) %>% 
  filter(count > 0) %>%
  arrange(SCRSSN, unk, desc(count), eth) %>% 
  group_by(SCRSSN) %>%
  slice_head(n=1) %>%
  transmute(SCRSSN, eth = if_else(eth == "UnkHispanic", NA, eth))

table1(~race, data = oasis1922_race %>% mutate(race = as.factor(race)))
table1(~eth, data = oasis1922_eth %>% mutate(eth = as.factor(eth)))

# IRFPAI is also check all that apply:

irfpai1922_raceeth <- tbl(con, in_schema(schema_dflt, 'IRFPAI_1922')) %>%
  collect() %>% 
  mutate(
    source = "IRF",
    SCRSSN = as.character(sprintf("%09d", as.integer(SCRSSN))),
  ) %>%   
  inner_join(analysis_data %>% select(SCRSSN) %>% distinct(), by = 'SCRSSN') %>%  
  select(
    SCRSSN, 
    source, 
    starts_with("ETH"))  # Self-care codes


irfpai1922_raceethf <- irfpai1922_raceeth %>% 
  transmute(SCRSSN,
            source,
            ETH_WHT_SW = coalesce(ETH_WHT_SW, "-"), 
            ETH_AFRCN_AMRCN_SW = coalesce(ETH_AFRCN_AMRCN_SW, "-"), 
            ETH_ASN_SW= coalesce(ETH_ASN_SW, "-"),  
            ETH_HSPNC_LTN_SW = coalesce(ETH_HSPNC_LTN_SW, "-"),  
            ETH_AMRCN_INDN_AK_NTV_SW = coalesce(ETH_AMRCN_INDN_AK_NTV_SW, "-"), 
            Unknown = if_else(ETH_WHT_SW == "-" & ETH_AFRCN_AMRCN_SW == "-" | ETH_ASN_SW == "-" |  ETH_AMRCN_INDN_AK_NTV_SW == "-", 1L, 0L),
            White = if_else(ETH_WHT_SW == "1", 1L, 0L),
            Black = if_else(ETH_AFRCN_AMRCN_SW == "1", 1L, 0L),
            Asian_PI = if_else(ETH_ASN_SW == "1", 1L, 0L),
            American_Indian_AN = if_else(ETH_AMRCN_INDN_AK_NTV_SW == "1", 1L, 0L),
            Hispanic = if_else(ETH_HSPNC_LTN_SW == "1", 1L, 0L),
            UnkHispanic = if_else(ETH_HSPNC_LTN_SW == "-", 1L, 0L))

irfpai1922_race <- irfpai1922_raceethf %>% 
  group_by(SCRSSN) %>%
  summarize(Unknown = sum(Unknown),
            White = sum(White),
            Black = sum(Black),
            Asian_PI = sum(Asian_PI),
            American_Indian_AN = sum(American_Indian_AN)) %>%
  pivot_longer(cols = Unknown:American_Indian_AN, values_to = "count", names_to = "race") %>% 
  mutate(pref = case_when(
    race == "American_Indian_AN" ~ 1,
    race == "Asian_PI" ~ 2,
    race == "Black" ~ 3,
    race == "White" ~ 4,
    race ==  "Unknown" ~ 5), 
    unk = if_else(race == "Unknown", 1, 0)) %>% 
  filter(count > 0) %>%
  arrange(SCRSSN, unk, desc(count), pref) %>% 
  group_by(SCRSSN) %>%
  slice_head(n=1) %>%
  transmute(SCRSSN, race = if_else(race == "Unknown", NA, race))

irfpai1922_eth <- irfpai1922_raceethf %>% 
  group_by(SCRSSN) %>%
  summarize(Hispanic = sum(Hispanic),
            UnkHispanic = sum(UnkHispanic)) %>%
  pivot_longer(cols = Hispanic:UnkHispanic, values_to = "count", names_to = "eth") %>% 
  mutate(unk = if_else(eth=="UnkHispanic", 1, 0)) %>% 
  filter(count > 0) %>%
  arrange(SCRSSN, unk, desc(count), eth) %>% 
  group_by(SCRSSN) %>%
  slice_head(n=1) %>%
  transmute(SCRSSN, eth = if_else(eth == "UnkHispanic", NA, eth))

table1(~race, data = irfpai1922_race %>% mutate(race = as.factor(irfpai_race)))
table1(~eth, data = irfpai1922_eth %>% mutate(eth = as.factor(irfpai_eth)))


full_race <- bind_rows(mds1922_race, oasis1922_race, irfpai1922_race)

full_race <- full_race %>% 
  dplyr::mutate(pref = dplyr::case_when(
    race == "American_Indian_AN" ~ 1,
    race == "Asian_PI" ~ 2,
    race == "Black" ~ 3,
    race == "White" ~ 4,
    is.na(race) ~ 5,
    TRUE ~ 6))

full_race <- full_race %>% 
  group_by(SCRSSN) %>%
  arrange(pref) %>%
  slice_head(n=1)

full_race <- analysis_data %>% 
  select(SCRSSN) %>%
  distinct() %>%
  left_join(full_race, by = "SCRSSN") %>%
  transmute(SCRSSN, race = coalesce(race, race, race))

table1(~race, data = full_race %>% mutate(race = as.factor(race)))

full_eth <- analysis_data %>% 
  select(SCRSSN) %>%
  distinct() %>%
  left_join(mds1922_eth, by = "SCRSSN") %>%
  left_join(oasis1922_eth, by = "SCRSSN") %>%
  left_join(irfpai1922_eth, by = "SCRSSN") %>%
  transmute(SCRSSN, eth = coalesce(eth, eth, eth))

table1(~eth, data = full_eth %>% mutate(eth = as.factor(eth)))

# ==============================================================================
# MODULE 4: Combine Race / Ethnicity datasets:
# ==============================================================================

full_dat_race <- analysis_data %>% 
  select(PatientICN, SCRSSN) %>%
  distinct() %>%
  left_join(full_race %>% select(SCRSSN, race), by = "SCRSSN") %>%
  left_join(cdw_race %>% select(PatientICN, cdw_race), by = "PatientICN") %>%
  left_join(cms_race %>% select(PatientICN, cms_race), by = "PatientICN") %>%
  left_join(mbsf_race %>% select(PatientICN, mbsf_race), by = "PatientICN") %>%
  left_join(mbsf_rti_race %>% select(PatientICN, mbsf_rti_race), by = "PatientICN") %>%
  mutate(race = coalesce(race, mbsf_rti_race, cdw_race, cms_race, mbsf_race))

table1(~race, data = full_dat_race %>% mutate(race = as.factor(race)))

full_dat_eth <- analysis_data %>% 
  select(PatientICN, SCRSSN) %>%
  distinct() %>%
  left_join(full_eth %>% select(SCRSSN, eth), by = "SCRSSN") %>%
  left_join(cdw_eth %>% select(PatientICN, cdw_eth), by = "PatientICN") %>%
  left_join(cms_eth %>% select(PatientICN, cms_eth), by = "PatientICN") %>%
  left_join(mbsf_eth %>% select(PatientICN, mbsf_eth), by = "PatientICN") %>%
  left_join(mbsf_rti_eth %>% select(PatientICN, mbsf_rti_eth), by = "PatientICN") %>%
  mutate(eth = coalesce(eth, mbsf_rti_eth, cdw_eth, cms_eth, mbsf_eth))

table1(~eth, data = full_dat_eth %>% mutate(eth = as.factor(eth)))


full_dat_race_eth <- analysis_data %>% 
  select(PatientICN, SCRSSN) %>%
  distinct() %>%
  left_join(full_dat_race %>% select(SCRSSN, race), by = "SCRSSN") %>%
  left_join(full_dat_eth %>% select(SCRSSN, eth), by = "SCRSSN")

full_dat_race_eth %>% 
  mutate(eth = if_else(is.na(eth), "Missing", eth),
         race = if_else(is.na(race), "Missing", race)) %>% 
  group_by(race, eth) %>% 
  summarize(n = n())

save_parquet_safe(full_dat_race_eth, "full_dat_race_eth.parquet")






