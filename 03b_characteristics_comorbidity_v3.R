################################################################################
# SCRIPT 03B: COMORBIDITY AND STROKE SEVERITY CHARACTERISTICS
# Stroke Rehabilitation Research - CDA Study
# ORD Waddell 202408036D
################################################################################
#
# Purpose:
#   Calculate comorbidity measures and stroke severity indicators for stroke
#   cohort using diagnosis and procedure codes from CDW and CMS OMOP tables.
#   Implements three key measures:
#     1. Claims Frailty Index (CFI) - 1-year lookback
#     2. Charlson Comorbidity Index - 2-year lookback
#     3. Stroke Severity Index (SSI) - acute hospitalization only
#
# Author: Nick Cardamone (CFI model by Dae Hyun Kim, versioned by Doug Bedell)
# Created: 2021-06-17
# Last Modified: 2025-12-11
# Version: 3.0
#
# Processing Steps:
#   1. Define temporal windows (2-year, 1-year lookback, acute period)
#   2. Extract diagnosis codes from CDW and CMS OMOP
#   3. Extract procedure codes from CDW and CMS OMOP
#   4. Calculate Charlson Comorbidity Index (2-year lookback)
#   5. Calculate Claims Frailty Index (1-year lookback)
#   6. Calculate Stroke Severity Index (acute period)
#   7. Combine all measures into final dataset
#
# Inputs:
#   - rhf_post_acute_final.parquet (cohort with admission dates)
#   - OMOPV5_VISIT_OCCURRENCE (CDW visits)
#   - OMOP_CMS_VISIT_OCCURRENCE_FF291 (CMS visits)
#   - OMOPV5_CONDITION_OCCURRENCE (CDW diagnoses)
#   - OMOP_CMS_CONDITION_OCCURRENCE_FF291 (CMS diagnoses)
#   - OMOPV5_PROCEDURE_OCCURRENCE (CDW procedures)
#   - OMOP_CMS_PROCEDURE_OCCURRENCE_FF291 (CMS procedures)
#   - CFI lookup tables: CFI_ICD10CM_V2020.csv, pxlookup.txt, disease_weight.txt
#
# Outputs:
#   - cohort_comorb_frailty.parquet: Final dataset with all comorbidity measures
#   - Intermediate parquet files for diagnosis/procedure codes by time window
#
# Expected Results:
#   - ~74K stroke hospitalizations with comorbidity scores
#   - Default CFI = 0.10288 for patients with no claims
#   - Default Charlson = 0 for patients with no diagnoses
#
# Key Variables Created:
#   - p1_frailty_index: Claims Frailty Index (1-year lookback)
#   - p2_charlson_cindex: Charlson Comorbidity Index (2-year lookback)
#   - acu_stroke_severity: Stroke Severity Index (0-56 scale)
#   - nutrition, trach_vent, aphasia, coma, dysarthria, hemiplegia, neglect
#
################################################################################

# ==============================================================================
# SETUP
# ==============================================================================

# Source configuration
source("00_config.R")

# Load required packages
load_packages(c("dplyr", "dbplyr", "DBI", "VINCI", "arrow", "data.table", 
                "table1", "tidyr", "lubridate", "stringr", "comorbidity", 
                "bit64", "sqldf", "comorbidity"))

cat("\n=============================================================================\n")
cat("SCRIPT 03B: COMORBIDITY AND STROKE SEVERITY CHARACTERISTICS\n")
cat("=============================================================================\n\n")

# Set working directory
setwd(project_base)

# CFI model directory
cfi_dir <- file.path(project_base, "CFI")
if (!dir.exists(cfi_dir)) {
  stop("CFI directory not found. Required CFI lookup files must be in: ", cfi_dir)
}

# ==============================================================================
# MODULE 1: LOAD COHORT AND DEFINE TEMPORAL WINDOWS
# ==============================================================================

cat("\n--- MODULE 1: Load Cohort and Define Temporal Windows ---\n")

# Load post-acute care cohort
cat("Loading stroke cohort...\n")
cohort_full <- load_parquet_safe("rhf_post_acute_final.parquet")

cohort <- cohort_full %>%
  select(PatientICN, scrssn, PERSON_ID, ADMSNDT, DSCHRGDT) %>% 
  distinct()

log_count(cohort, "Unique stroke hospitalizations")
cat("Unique patients:", format(length(unique(cohort$scrssn)), big.mark = ","), "\n\n")

# Define temporal windows
cat("Defining temporal windows for comorbidity lookback...\n")
cat("  2-year lookback: For Charlson Comorbidity Index\n")
cat("  1-year lookback: For Claims Frailty Index\n")
cat("  Acute period: ADMSNDT to DSCHRGDT for Stroke Severity\n\n")

cohort <- cohort %>% 
  transmute(
    PERSON_ID,
    ADMSNDT,
    DSCHRGDT,
    # Handle leap year edge case (Feb 29 admission)
    WINDOW_START2 = if_else(
      ADMSNDT == "2020-02-29", 
      as.Date("2018-02-28"), 
      ADMSNDT - lubridate::years(2)
    ), 
    WINDOW_START1 = if_else(
      ADMSNDT == "2020-02-29", 
      as.Date("2019-02-28"), 
      ADMSNDT - lubridate::years(1)
    )
  )

cat("Temporal window examples:\n")
print(head(cohort %>% select(PERSON_ID, ADMSNDT, WINDOW_START2, WINDOW_START1)))

# ==============================================================================
# DATABASE CONNECTION
# ==============================================================================

cat("\nEstablishing database connections...\n")
con <- connect_db(database = db_project, server = db_server)
cdwwork <- connect_db(database = "CDWWork", server = db_server)

# Load procedure concept table for code mapping
cat("Loading ICD10 procedure concept mapping...\n")
omop_proc_concept <- tbl(con, in_schema(schema_src, 'OMOPV5Dim_ICD10Procedure_CONCEPT')) %>% 
  select(CONCEPT_ID, ICD10ProcedureCode)

# ==============================================================================
# MODULE 2: EXTRACT VISITS FOR TEMPORAL WINDOWS
# ==============================================================================

cat("\n--- MODULE 2: Extract Visits for Temporal Windows ---\n")

# Extract CMS visits (2-year lookback window)
cat("Extracting CMS visits (2-year lookback)...\n")
omop_visits_cms_widest <- tbl(con, in_schema(schema_src, 'OMOP_CMS_VISIT_OCCURRENCE_FF291')) %>% 
  select(PERSON_ID, VISIT_START_DATE, VISIT_END_DATE, VISIT_OCCURRENCE_ID) %>% 
  inner_join(cohort, by = "PERSON_ID", copy = TRUE) %>%
  filter(VISIT_START_DATE >= WINDOW_START2 & VISIT_END_DATE <= DSCHRGDT) %>% 
  distinct()

# Extract CDW visits (2-year lookback window)
cat("Extracting CDW visits (2-year lookback)...\n")
omop_visits_cdw_widest <- tbl(con, in_schema(schema_src, 'OMOPV5_VISIT_OCCURRENCE')) %>% 
  select(PERSON_ID, VISIT_START_DATE, VISIT_END_DATE, VISIT_OCCURRENCE_ID) %>% 
  inner_join(cohort, by = "PERSON_ID", copy = TRUE) %>%
  filter(VISIT_START_DATE >= WINDOW_START2 & VISIT_END_DATE <= DSCHRGDT) %>% 
  distinct()

cat("Visit extraction complete.\n")

# ==============================================================================
# MODULE 3: EXTRACT DIAGNOSIS AND PROCEDURE CODES - 2 YEAR LOOKBACK
# ==============================================================================

cat("\n--- MODULE 3: Extract Diagnosis and Procedure Codes (2-Year Lookback) ---\n")

cat("Extracting diagnosis codes from CMS (2-year lookback)...\n")
omop_cond_cms_widest <- tbl(con, in_schema(schema_src, 'OMOP_CMS_CONDITION_OCCURRENCE_FF291')) %>% 
  select(CONDITION_SOURCE_VALUE, VISIT_OCCURRENCE_ID) %>% 
  inner_join(omop_visits_cms_widest, by = "VISIT_OCCURRENCE_ID") %>%
  distinct()

cat("Extracting diagnosis codes from CDW (2-year lookback)...\n")
omop_cond_cdw_widest <- tbl(con, in_schema(schema_src, 'OMOPV5_CONDITION_OCCURRENCE')) %>% 
  select(CONDITION_SOURCE_VALUE, VISIT_OCCURRENCE_ID) %>% 
  inner_join(omop_visits_cdw_widest, by = "VISIT_OCCURRENCE_ID") %>%
  distinct()

cat("Extracting procedure codes from CMS (2-year lookback)...\n")
omop_proc_cms_widest <- tbl(con, in_schema(schema_src, 'OMOP_CMS_PROCEDURE_OCCURRENCE_FF291')) %>%
  select(PROCEDURE_SOURCE_VALUE, PROCEDURE_CONCEPT_ID, VISIT_OCCURRENCE_ID) %>% 
  inner_join(omop_proc_concept, by = c("PROCEDURE_CONCEPT_ID" = "CONCEPT_ID")) %>% 
  inner_join(omop_visits_cms_widest, by = "VISIT_OCCURRENCE_ID") %>%
  distinct()

cat("Extracting procedure codes from CDW (2-year lookback)...\n")
omop_proc_cdw_widest <- tbl(con, in_schema(schema_src, 'OMOPV5_PROCEDURE_OCCURRENCE')) %>%
  select(PROCEDURE_SOURCE_VALUE, PROCEDURE_CONCEPT_ID, VISIT_OCCURRENCE_ID) %>% 
  inner_join(omop_proc_concept, by = c("PROCEDURE_CONCEPT_ID" = "CONCEPT_ID")) %>% 
  inner_join(omop_visits_cdw_widest, by = "VISIT_OCCURRENCE_ID") %>%
  distinct()

# Filter to 2-year lookback period (before admission)
cat("\nFiltering to 2-year lookback period (prior to admission)...\n")

omop_cond_cdw_p2 <- omop_cond_cdw_widest %>% 
  filter(VISIT_START_DATE >= WINDOW_START2 & 
         VISIT_START_DATE < ADMSNDT & 
         VISIT_END_DATE < "2023-01-01") %>% 
  transmute(PERSON_ID, ADMSNDT, DX = CONDITION_SOURCE_VALUE) %>% 
  distinct() %>% 
  collect()

omop_cond_cms_p2 <- omop_cond_cms_widest %>% 
  filter(VISIT_START_DATE >= WINDOW_START2 & 
         VISIT_START_DATE < ADMSNDT & 
         VISIT_END_DATE < "2023-01-01") %>% 
  transmute(PERSON_ID, ADMSNDT, DX = CONDITION_SOURCE_VALUE) %>% 
  distinct() %>% 
  collect()

omop_proc_cdw_p2 <- omop_proc_cdw_widest %>% 
  filter(VISIT_START_DATE >= WINDOW_START2 & 
         VISIT_START_DATE < ADMSNDT & 
         VISIT_END_DATE < "2023-01-01") %>% 
  transmute(PERSON_ID, ADMSNDT, PX = ICD10ProcedureCode) %>% 
  distinct() %>% 
  collect()

omop_proc_cms_p2 <- omop_proc_cms_widest %>% 
  filter(VISIT_START_DATE >= WINDOW_START2 & 
         VISIT_START_DATE < ADMSNDT & 
         VISIT_END_DATE < "2023-01-01") %>% 
  transmute(PERSON_ID, ADMSNDT, PX = ICD10ProcedureCode) %>% 
  distinct() %>% 
  collect()

# Parse source and code from combined fields
cat("Parsing diagnosis and procedure codes...\n")
setDT(omop_cond_cdw_p2)
setDT(omop_cond_cms_p2)
setDT(omop_proc_cdw_p2)
setDT(omop_proc_cms_p2)

omop_cond_cdw_p2_clean <- omop_cond_cdw_p2[, 
  c("source", "code") := tstrsplit(DX, "|", fixed = TRUE)
] %>% 
  transmute(PERSON_ID, ADMSNDT, source, code) %>% 
  na.omit()

omop_cond_cms_p2_clean <- omop_cond_cms_p2[, 
  c("source", "code") := tstrsplit(DX, "-", fixed = TRUE)
] %>% 
  transmute(PERSON_ID, ADMSNDT, source, code) %>% 
  na.omit()

omop_proc_cdw_p2_clean <- omop_proc_cdw_p2 %>% 
  transmute(PERSON_ID, ADMSNDT, source = "ICD10", code = PX) %>% 
  na.omit()

omop_proc_cms_p2_clean <- omop_proc_cms_p2 %>% 
  transmute(PERSON_ID, ADMSNDT, source = "ICD10", code = PX) %>% 
  na.omit()

# Save intermediate files
save_parquet_safe(omop_cond_cdw_p2_clean, "omop_cond_cdw_p2_clean.parquet")
save_parquet_safe(omop_cond_cms_p2_clean, "omop_cond_cms_p2_clean.parquet")
save_parquet_safe(omop_proc_cdw_p2_clean, "omop_proc_cdw_p2_clean.parquet")
save_parquet_safe(omop_proc_cms_p2_clean, "omop_proc_cms_p2_clean.parquet")

# Combine all sources
cat("Combining diagnosis and procedure codes from all sources...\n")
omop_proc_cond_p2 <- rbind(
  omop_cond_cdw_p2_clean, 
  omop_cond_cms_p2_clean, 
  omop_proc_cdw_p2_clean, 
  omop_proc_cms_p2_clean
) %>% distinct()

log_count(omop_proc_cond_p2, "Diagnosis/procedure codes (2-year lookback)")

# ==============================================================================
# MODULE 4: EXTRACT DIAGNOSIS AND PROCEDURE CODES - 1 YEAR LOOKBACK
# ==============================================================================

cat("\n--- MODULE 4: Extract Diagnosis and Procedure Codes (1-Year Lookback) ---\n")

cat("Filtering to 1-year lookback period (prior to admission)...\n")

omop_cond_cdw_p1 <- omop_cond_cdw_widest %>% 
  filter(VISIT_START_DATE >= WINDOW_START1 & 
         VISIT_START_DATE < ADMSNDT & 
         VISIT_END_DATE < "2023-01-01") %>% 
  transmute(PERSON_ID, ADMSNDT, DX = CONDITION_SOURCE_VALUE) %>% 
  distinct() %>% 
  collect()

omop_cond_cms_p1 <- omop_cond_cms_widest %>% 
  filter(VISIT_START_DATE >= WINDOW_START1 & 
         VISIT_START_DATE < ADMSNDT & 
         VISIT_END_DATE < "2023-01-01") %>% 
  transmute(PERSON_ID, ADMSNDT, DX = CONDITION_SOURCE_VALUE) %>% 
  distinct() %>% 
  collect()

omop_proc_cdw_p1 <- omop_proc_cdw_widest %>% 
  filter(VISIT_START_DATE >= WINDOW_START1 & 
         VISIT_START_DATE < ADMSNDT & 
         VISIT_END_DATE < "2023-01-01") %>% 
  transmute(PERSON_ID, ADMSNDT, PX = PROCEDURE_SOURCE_VALUE) %>% 
  distinct() %>% 
  collect()

omop_proc_cms_p1 <- omop_proc_cms_widest %>% 
  filter(VISIT_START_DATE >= WINDOW_START1 & 
         VISIT_START_DATE < ADMSNDT & 
         VISIT_END_DATE < "2023-01-01") %>% 
  transmute(PERSON_ID, ADMSNDT, PX = PROCEDURE_SOURCE_VALUE) %>% 
  distinct() %>% 
  collect()

# Parse source and code from combined fields
cat("Parsing diagnosis and procedure codes...\n")
setDT(omop_cond_cdw_p1)
setDT(omop_cond_cms_p1)
setDT(omop_proc_cdw_p1)
setDT(omop_proc_cms_p1)

omop_cond_cdw_p1_clean <- omop_cond_cdw_p1[, 
  c("source", "code") := tstrsplit(DX, "|", fixed = TRUE)
] %>% 
  transmute(PERSON_ID, ADMSNDT, source, code) %>% 
  na.omit()

omop_cond_cms_p1_clean <- omop_cond_cms_p1[, 
  c("source", "code") := tstrsplit(DX, "-", fixed = TRUE)
] %>% 
  transmute(PERSON_ID, ADMSNDT, source, code) %>% 
  na.omit()

omop_proc_cdw_p1_clean <- omop_proc_cdw_p1[, 
  c("source", "code") := tstrsplit(PX, "|", fixed = TRUE)
] %>% 
  transmute(PERSON_ID, ADMSNDT, source, code) %>% 
  na.omit()

omop_proc_cms_p1_clean <- omop_proc_cms_p1[, 
  c("source", "code") := tstrsplit(PX, "-", fixed = TRUE)
] %>% 
  transmute(PERSON_ID, ADMSNDT, source, code) %>% 
  na.omit()

# Save intermediate files
save_parquet_safe(omop_cond_cdw_p1_clean, "omop_cond_cdw_p1_clean.parquet")
save_parquet_safe(omop_cond_cms_p1_clean, "omop_cond_cms_p1_clean.parquet")
save_parquet_safe(omop_proc_cdw_p1_clean, "omop_proc_cdw_p1_clean.parquet")
save_parquet_safe(omop_proc_cms_p1_clean, "omop_proc_cms_p1_clean.parquet")

# Combine all sources
cat("Combining diagnosis and procedure codes from all sources...\n")
omop_proc_cond_p1 <- rbind(
  omop_cond_cdw_p1_clean, 
  omop_cond_cms_p1_clean, 
  omop_proc_cdw_p1_clean, 
  omop_proc_cms_p1_clean
) %>% distinct()

log_count(omop_proc_cond_p1, "Diagnosis/procedure codes (1-year lookback)")

# ==============================================================================
# MODULE 5: EXTRACT DIAGNOSIS AND PROCEDURE CODES - ACUTE PERIOD
# ==============================================================================

cat("\n--- MODULE 5: Extract Diagnosis and Procedure Codes (Acute Period) ---\n")

cat("Filtering to acute hospitalization period (ADMSNDT to DSCHRGDT)...\n")

omop_cond_cdw_acu <- omop_cond_cdw_widest %>% 
  filter(VISIT_START_DATE >= ADMSNDT & VISIT_END_DATE <= DSCHRGDT) %>% 
  transmute(PERSON_ID, ADMSNDT, DX = CONDITION_SOURCE_VALUE) %>% 
  distinct() %>% 
  collect()

omop_cond_cms_acu <- omop_cond_cms_widest %>% 
  filter(VISIT_START_DATE >= ADMSNDT & VISIT_END_DATE <= DSCHRGDT) %>% 
  transmute(PERSON_ID, ADMSNDT, DX = CONDITION_SOURCE_VALUE) %>% 
  distinct() %>% 
  collect()

omop_proc_cdw_acu <- omop_proc_cdw_widest %>% 
  filter(VISIT_START_DATE >= ADMSNDT & VISIT_END_DATE <= DSCHRGDT) %>% 
  transmute(PERSON_ID, ADMSNDT, PX = ICD10ProcedureCode) %>% 
  distinct() %>% 
  collect()

omop_proc_cms_acu <- omop_proc_cms_widest %>% 
  filter(VISIT_START_DATE >= ADMSNDT & VISIT_END_DATE <= DSCHRGDT) %>% 
  transmute(PERSON_ID, ADMSNDT, PX = ICD10ProcedureCode) %>% 
  distinct() %>% 
  collect()

# Parse source and code from combined fields
cat("Parsing diagnosis and procedure codes...\n")
setDT(omop_cond_cdw_acu)
setDT(omop_cond_cms_acu)

omop_cond_cdw_acu_clean <- omop_cond_cdw_acu[, 
  c("source", "code") := tstrsplit(DX, "|", fixed = TRUE)
] %>% 
  transmute(PERSON_ID, ADMSNDT, source, code) %>% 
  na.omit()

omop_cond_cms_acu_clean <- omop_cond_cms_acu[, 
  c("source", "code") := tstrsplit(DX, "-", fixed = TRUE)
] %>% 
  transmute(PERSON_ID, ADMSNDT, source, code) %>% 
  na.omit()

omop_proc_cdw_acu_clean <- omop_proc_cdw_acu %>% 
  transmute(PERSON_ID, ADMSNDT, source = "ICD10", code = PX) %>% 
  na.omit()

omop_proc_cms_acu_clean <- omop_proc_cms_acu %>% 
  transmute(PERSON_ID, ADMSNDT, source = "ICD10", code = PX) %>% 
  na.omit()

# Save intermediate files
save_parquet_safe(omop_cond_cdw_acu_clean, "omop_cond_cdw_acu_clean.parquet")
save_parquet_safe(omop_cond_cms_acu_clean, "omop_cond_cms_acu_clean.parquet")
save_parquet_safe(omop_proc_cdw_acu_clean, "omop_proc_cdw_acu_clean.parquet")
save_parquet_safe(omop_proc_cms_acu_clean, "omop_proc_cms_acu_clean.parquet")

# Combine all sources
cat("Combining diagnosis and procedure codes from all sources...\n")
omop_proc_cond_acu <- rbind(
  omop_cond_cdw_acu_clean, 
  omop_cond_cms_acu_clean, 
  omop_proc_cdw_acu_clean, 
  omop_proc_cms_acu_clean
) %>% distinct()

log_count(omop_proc_cond_acu, "Diagnosis/procedure codes (acute period)")

# ==============================================================================
# MODULE 6: CALCULATE CHARLSON COMORBIDITY INDEX (2-YEAR LOOKBACK)
# ==============================================================================

cat("\n--- MODULE 6: Calculate Charlson Comorbidity Index ---\n")

cat("Using 2-year lookback period for Charlson calculation...\n")

# Filter to ICD10 diagnosis codes only
omop_cond_icd10 <- omop_proc_cond_p2 %>% 
  filter(source %in% c("ICD10", "ICD10Proc", "ICD10Procedure")) %>% 
  transmute(
    id = paste0(PERSON_ID, "_", ADMSNDT), 
    dx = code
  ) %>% 
  distinct()

log_count(omop_cond_icd10, "ICD10 diagnosis codes for Charlson")

# Calculate Charlson index using comorbidity package
cat("Calculating Charlson Comorbidity Index...\n")
charlson_dx10 <- comorbidity(
  x = omop_cond_icd10, 
  id = "id", 
  code = "dx", 
  map = "charlson_icd10_quan", 
  tidy.codes = FALSE, 
  assign0 = FALSE
)

# Calculate weighted score
charlson_score_dx10 <- tibble::enframe(
  score(charlson_dx10, weights = "quan", assign0 = FALSE)
)

charlson_score_dx10 <- charlson_score_dx10 %>% 
  select(-name)

charlson_score_dx10 <- bind_cols(charlson_dx10, charlson_score_dx10) %>% 
  transmute(id, charlson_cindex = value)

# Parse ID back to PERSON_ID and ADMSNDT
charlson_score_dx10 <- charlson_score_dx10 %>% 
  separate(col = "id", into = c("PERSON_ID", "ADMSNDT"), sep = "_") %>% 
  mutate(ADMSNDT = as.Date(ADMSNDT))

cat("Charlson Index calculated for", 
    format(nrow(charlson_score_dx10), big.mark = ","), 
    "hospitalizations\n")
cat("Mean Charlson score:", 
    round(mean(charlson_score_dx10$charlson_cindex, na.rm = TRUE), 2), "\n")

# ==============================================================================
# MODULE 7: CALCULATE CLAIMS FRAILTY INDEX (1-YEAR LOOKBACK)
# ==============================================================================

cat("\n--- MODULE 7: Calculate Claims Frailty Index (CFI) ---\n")

cat("Using 1-year lookback period for CFI calculation...\n")
cat("CFI implementation based on Kim et al. model\n\n")

# Set CFI working directory
setwd(cfi_dir)

# Model intercept (default score for patients with no claims)
ModelIntercept <- 0.10288
cat("Model intercept (default CFI):", ModelIntercept, "\n\n")

# Helper function for HCPCS/CPT disease number lookup
lookup_pxdisease <- function(data, lookup) {
  sqldf("SELECT A.*, B.disease_number FROM
         data A LEFT JOIN lookup B 
         ON (A.px >= B.start AND A.px <= B.stop)")
}

# Create patient IDs
cat("Creating patient identifiers...\n")
iddata <- cohort %>% 
  transmute(id = paste0(PERSON_ID, "_", ADMSNDT)) %>% 
  distinct()

# Load CFI lookup tables
cat("Loading CFI lookup tables...\n")

dx10lookup_file <- file.path(cfi_dir, "CFI_ICD10CM_V2020.csv")
pxlookup_file <- file.path(cfi_dir, "pxlookup.txt")
weightlookup_file <- file.path(cfi_dir, "disease_weight.txt")

if (!file.exists(dx10lookup_file)) {
  stop("CFI_ICD10CM_V2020.csv not found in CFI directory")
}
if (!file.exists(pxlookup_file)) {
  stop("pxlookup.txt not found in CFI directory")
}
if (!file.exists(weightlookup_file)) {
  stop("disease_weight.txt not found in CFI directory")
}

dx10lookup <- read.delim(dx10lookup_file, sep = ',', header = TRUE)
pxlookup <- read.delim(pxlookup_file, sep = '\t', header = TRUE)
weightlookup <- read.delim(weightlookup_file, sep = '\t', header = TRUE)

cat("Lookup tables loaded successfully\n")

# Prepare diagnosis data (ICD10)
cat("\nPreparing ICD10 diagnosis data...\n")
dx10data <- omop_proc_cond_p1 %>% 
  filter(source %in% c("ICD10", "ICD10Proc", "ICD10Procedure")) %>% 
  transmute(PERSON_ID, ADMSNDT, dx = code) %>% 
  distinct()

log_count(dx10data, "ICD10 diagnoses for CFI")

# Prepare procedure data (CPT/HCPCS)
cat("Preparing CPT/HCPCS procedure data...\n")
pxdata <- omop_proc_cond_p1 %>% 
  filter(source %in% c("CPT", "CPT4", "HCPCS")) %>% 
  transmute(PERSON_ID, ADMSNDT, px = code) %>% 
  distinct()

log_count(pxdata, "CPT/HCPCS procedures for CFI")

# Map diagnosis codes to disease numbers
cat("\nMapping ICD10 diagnoses to CFI disease categories...\n")
dx10data <- unique(dx10data)
dx10data <- merge(dx10data, dx10lookup, all.x = TRUE)
dx10data[is.na(dx10data)] <- 0
dx10data <- dx10data[order(dx10data$PERSON_ID, dx10data$ADMSNDT, dx10data$dx), ]

# Map procedure codes to disease numbers
cat("Mapping CPT/HCPCS procedures to CFI disease categories...\n")
pxdata <- unique(pxdata)
pxdata <- lookup_pxdisease(pxdata, lookup = pxlookup)
pxdata[is.na(pxdata)] <- 0

# Validate procedure codes (must be 5 characters, last character numeric)
pxdata <- within(pxdata, 
  disease_number[
    nchar(px) != 5 | 
    grepl("[0-9]", substr(px, nchar(px), nchar(px))) == FALSE
  ] <- 0
)

# Assign dummy disease_number = 0 for all study IDs
# This ensures ModelIntercept is assigned to patients with no claims
iddata <- unique(iddata)
setDT(iddata)
iddata$disease_number <- 0

# Combine all disease data
cat("Combining diagnosis and procedure disease categories...\n")
pxdata <- pxdata %>% 
  transmute(PERSON_ID, ADMSNDT, disease_number) %>% 
  distinct()

dx10data <- dx10data %>% 
  transmute(PERSON_ID, ADMSNDT, disease_number) %>% 
  distinct()

diseasedata <- rbind(pxdata, dx10data) %>% 
  transmute(id = paste0(PERSON_ID, "_", ADMSNDT), disease_number) %>% 
  distinct()

# Remove duplicates (each DX/PX should only be weighted once)
diseasedatasort <- diseasedata[order(diseasedata$id, diseasedata$disease_number), ]

log_count(diseasedatasort, "Unique disease categories")

# Assign disease weights
cat("Assigning CFI disease weights...\n")
diseasedatasort <- merge(diseasedatasort, weightlookup, all.x = TRUE)
diseasedatasort[is.na(diseasedatasort)] <- 0
diseasedatasort <- diseasedatasort[
  order(diseasedatasort$id, diseasedatasort$disease_number), 
]

# Calculate frailty scores
cat("Calculating frailty scores...\n")
scores <- aggregate(
  diseasedatasort$weight, 
  by = list(id = diseasedatasort$id), 
  FUN = sum
)
scores$x <- scores$x + ModelIntercept
colnames(scores) <- c('id', 'frailty_index')

# Parse ID back to PERSON_ID and ADMSNDT
scores <- scores %>% 
  separate(col = "id", into = c("PERSON_ID", "ADMSNDT"), sep = "_") %>% 
  mutate(ADMSNDT = as.Date(ADMSNDT))

cat("CFI calculated for", 
    format(nrow(scores), big.mark = ","), 
    "hospitalizations\n")
cat("Mean CFI:", 
    round(mean(scores$frailty_index, na.rm = TRUE), 4), "\n")

# Return to project directory
setwd(project_base)

# ==============================================================================
# MODULE 8: CALCULATE STROKE SEVERITY INDEX (ACUTE PERIOD)
# ==============================================================================

cat("\n--- MODULE 8: Calculate Stroke Severity Index ---\n")

cat("Using acute hospitalization period for stroke severity...\n")
cat("SSI components: Aphasia, Coma, Dysarthria, Hemiplegia, Neglect,\n")
cat("                Nutritional support, Tracheostomy/Ventilation\n\n")

# Define ICD10 code sets for stroke severity components
cat("Defining stroke severity ICD10 code patterns...\n")

# Aphasia codes
aphasia_codes <- c("R4701", "I69920", "I6902", "I69028", "I69120", 
                   "I69220", "I69320", "I69820")

# Coma codes
coma_codes <- c("R4020", "R403")

# Dysarthria codes
dysarthria_codes <- c("I69922", "I69991", "I69021", "I69022", "I69091", 
                      "I69121", "I69122", "I69191", "I69221", "I69222", 
                      "I69291", "I69321", "I69322", "I69391", "I69821", 
                      "I69822", "R471", "R1310")

# Hemiplegia codes (complex pattern with ranges)
g81_codes <- paste0("G81", sprintf("%02d", 0:99))
i69_03x <- paste0("I6903", 0:9)
i69_04x <- paste0("I6904", 0:9)
i69_05x <- paste0("I6905", 0:9)
i69_13x <- paste0("I6913", 0:9)
i69_14x <- paste0("I6914", 0:9)
i69_15x <- paste0("I6915", 0:9)
i69_23x <- paste0("I6923", 0:9)
i69_24x <- paste0("I6924", 0:9)
i69_25x <- paste0("I6925", 0:9)
i69_33x <- paste0("I6933", 0:9)
i69_34x <- paste0("I6934", 0:9)
i69_35x <- paste0("I6935", 0:9)

hemiplegia_codes <- c(g81_codes, i69_03x, i69_04x, i69_05x, i69_13x, 
                      i69_14x, i69_15x, i69_23x, i69_24x, i69_25x, 
                      i69_33x, i69_34x, i69_35x, "G8190", "G832", 
                      "I69959", "I6984")

# Neglect codes
neglect_codes <- c("R414", "I69012", "I69112", "I69212", "I69312")

# Nutritional infusion codes (ICD10-PCS)
nutritional_codes <- c("3E0G36Z", "3E0336Z", "3E0436Z", "3E0536Z", "3E0636Z")

# Tracheostomy/ventilation codes (ICD10-PCS)
trach_ventilation_codes <- c("0B110F4", "0B110Z4", "0B113F4", "0B113Z4", 
                             "0B114F4", "0B114Z4", "5A09357", "5A09457", 
                             "5A09557", "0BH17EZ", "0BH18EZ", "5A1935Z", 
                             "5A1945Z", "5A1955Z")

# Create stroke severity indicators
cat("Creating stroke severity indicators...\n")
ss_codes <- omop_proc_cond_acu %>% 
  transmute(PERSON_ID, ADMSNDT, code) %>%
  distinct() %>% 
  transmute(
    PERSON_ID,
    ADMSNDT,
    code,
    CODE = str_remove_all(code, "\\."),
    aphasia = if_else(CODE %in% aphasia_codes, 1, 0),
    coma = if_else(CODE %in% coma_codes, 1, 0),
    dysarthria = if_else(CODE %in% dysarthria_codes, 1, 0),
    hemiplegia = if_else(CODE %in% hemiplegia_codes, 1, 0),
    neglect = if_else(CODE %in% neglect_codes, 1, 0),
    nutrition = if_else(CODE %in% nutritional_codes, 1, 0),
    trach_vent = if_else(CODE %in% trach_ventilation_codes, 1, 0)
  )

# Aggregate to patient-hospitalization level
cat("Aggregating stroke severity indicators to hospitalization level...\n")
omop_ss <- ss_codes %>% 
  group_by(PERSON_ID, ADMSNDT) %>% 
  summarise(
    nutrition = max(nutrition),
    trach_vent = max(trach_vent),
    aphasia = max(aphasia),
    coma = max(coma),
    dysarthria = max(dysarthria),
    hemiplegia = max(hemiplegia),
    neglect = max(neglect),
    .groups = 'drop'
  )

# Calculate Stroke Severity Index (weighted sum)
# Weights from Kim et al. algorithm
cat("Calculating weighted Stroke Severity Index...\n")
cat("  Weights: Nutrition=5, Trach/Vent=10, Aphasia=4, Coma=23,\n")
cat("           Dysarthria=2, Hemiplegia=6, Neglect=6\n\n")

omop_ss <- omop_ss %>% 
  mutate(
    PERSON_ID = as.character(PERSON_ID), 
    ssi = 5 * nutrition + 
          10 * trach_vent + 
          4 * aphasia + 
          23 * coma + 
          2 * dysarthria + 
          6 * hemiplegia + 
          6 * neglect
  )

cat("SSI calculated for", 
    format(nrow(omop_ss), big.mark = ","), 
    "hospitalizations\n")
cat("Mean SSI:", 
    round(mean(omop_ss$ssi, na.rm = TRUE), 2), "\n")
cat("Range SSI:", 
    min(omop_ss$ssi, na.rm = TRUE), "-", 
    max(omop_ss$ssi, na.rm = TRUE), "\n")

# ==============================================================================
# MODULE 9: COMBINE ALL COMORBIDITY MEASURES
# ==============================================================================

cat("\n--- MODULE 9: Combine All Comorbidity Measures ---\n")

cat("Joining all comorbidity measures to cohort...\n")

cohort_comorb_frailty <- cohort_full %>% 
  mutate(PERSON_ID = as.character(PERSON_ID)) %>%
  left_join(scores, by = c("PERSON_ID", "ADMSNDT")) %>%
  left_join(charlson_score_dx10, by = c("PERSON_ID", "ADMSNDT")) %>%
  left_join(omop_ss, by = c("PERSON_ID", "ADMSNDT"))

# Create final dataset with default values for missing
cat("Applying default values for missing comorbidity scores...\n")
cat("  Default CFI: 0.10288 (model intercept)\n")
cat("  Default Charlson: 0 (no comorbidities)\n")
cat("  Default SSI components: 0 (not present)\n\n")

cohort_comorb_frailty <- cohort_comorb_frailty %>% 
  transmute(
    PatientICN, 
    ADMSNDT,
    # Frailty Index (1-year lookback)
    p1_frailty_index = replace_na(frailty_index, 0.10288),
    # Charlson Index (2-year lookback)
    p2_charlson_cindex = replace_na(charlson_cindex, 0),
    # Stroke Severity components (acute)
    nutrition = replace_na(nutrition, 0),
    trach_vent = replace_na(trach_vent, 0),
    aphasia = replace_na(aphasia, 0),
    coma = replace_na(coma, 0),
    dysarthria = replace_na(dysarthria, 0),
    hemiplegia = replace_na(hemiplegia, 0),
    neglect = replace_na(neglect, 0),
    # Stroke Severity Index (weighted sum)
    acu_stroke_severity = replace_na(ssi, 0)
  )

log_count(cohort_comorb_frailty, "Final comorbidity dataset")

# Save final dataset
save_parquet_safe(cohort_comorb_frailty, "cohort_comorb_frailty.parquet")

# ==============================================================================
# DATA VALIDATION AND SUMMARY STATISTICS
# ==============================================================================

cat("\n--- Data Validation and Summary Statistics ---\n")

# Summary statistics
cat("\nComorbidity Measure Summary Statistics:\n")
cat("----------------------------------------\n")

summary_stats <- cohort_comorb_frailty %>%
  summarise(
    n = n(),
    # Frailty Index
    cfi_mean = mean(p1_frailty_index, na.rm = TRUE),
    cfi_median = median(p1_frailty_index, na.rm = TRUE),
    cfi_min = min(p1_frailty_index, na.rm = TRUE),
    cfi_max = max(p1_frailty_index, na.rm = TRUE),
    # Charlson
    charlson_mean = mean(p2_charlson_cindex, na.rm = TRUE),
    charlson_median = median(p2_charlson_cindex, na.rm = TRUE),
    charlson_min = min(p2_charlson_cindex, na.rm = TRUE),
    charlson_max = max(p2_charlson_cindex, na.rm = TRUE),
    # SSI
    ssi_mean = mean(acu_stroke_severity, na.rm = TRUE),
    ssi_median = median(acu_stroke_severity, na.rm = TRUE),
    ssi_min = min(acu_stroke_severity, na.rm = TRUE),
    ssi_max = max(acu_stroke_severity, na.rm = TRUE)
  )

print(summary_stats)

# Stroke severity component prevalence
cat("\n\nStroke Severity Component Prevalence:\n")
cat("--------------------------------------\n")
severity_prev <- cohort_comorb_frailty %>%
  summarise(
    nutrition = sum(nutrition),
    trach_vent = sum(trach_vent),
    aphasia = sum(aphasia),
    coma = sum(coma),
    dysarthria = sum(dysarthria),
    hemiplegia = sum(hemiplegia),
    neglect = sum(neglect)
  )

print(severity_prev)

# Correlations between measures
cat("\n\nCorrelations Between Comorbidity Measures:\n")
cat("------------------------------------------\n")
cat("CFI vs SSI:", 
    round(cor(cohort_comorb_frailty$p1_frailty_index, 
              cohort_comorb_frailty$acu_stroke_severity), 3), "\n")
cat("CFI vs Charlson:", 
    round(cor(cohort_comorb_frailty$p1_frailty_index, 
              cohort_comorb_frailty$p2_charlson_cindex), 3), "\n")
cat("Charlson vs SSI:", 
    round(cor(cohort_comorb_frailty$p2_charlson_cindex, 
              cohort_comorb_frailty$acu_stroke_severity), 3), "\n")

# ==============================================================================
# SESSION INFO
# ==============================================================================

cat("\n=============================================================================\n")
cat("SCRIPT 03B COMPLETED SUCCESSFULLY\n")
cat("=============================================================================\n")
cat("\nKey Output File:\n")
cat("  - cohort_comorb_frailty.parquet: Final comorbidity dataset\n\n")

cat("Summary Statistics:\n")
cat("  Hospitalizations with comorbidity data:", 
    format(nrow(cohort_comorb_frailty), big.mark = ","), "\n")
cat("  Mean Claims Frailty Index:", 
    round(mean(cohort_comorb_frailty$p1_frailty_index), 4), "\n")
cat("  Mean Charlson Index:", 
    round(mean(cohort_comorb_frailty$p2_charlson_cindex), 2), "\n")
cat("  Mean Stroke Severity Index:", 
    round(mean(cohort_comorb_frailty$acu_stroke_severity), 2), "\n")
cat("=============================================================================\n\n")

# Clean up
dbDisconnect(con)
dbDisconnect(cdwwork)

################################################################################
# END OF SCRIPT
################################################################################
