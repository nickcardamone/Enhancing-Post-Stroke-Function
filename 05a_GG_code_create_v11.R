################################################################################
# SCRIPT 05A: EXTRACT AND PROCESS GG FUNCTIONAL ASSESSMENT CODES
# Stroke Rehabilitation Research - CDA Study
# ORD Waddell 202408036D
################################################################################
#
# Purpose:
#   Extract and process GG functional assessment codes from three data sources:
#   MDS (Minimum Data Set), OASIS (home health), and IRF-PAI (inpatient rehab).
#   Clean and standardize codes across sources, apply scoring algorithms, and
#   create composite self-care and mobility scores for matching to post-acute
#   care episodes.
#
# Author: Nick Cardamone
# Created: 2025-04-07
# Last Modified: 2025-12-30
# Version: 12.0
#
# Processing Steps:
#   1. Extract GG codes from MDS 2019-2022
#   2. Extract GG codes from OASIS 2019-2022
#   3. Extract GG codes from IRF-PAI 2019-2022
#   4. Standardize assessment timing indicators across sources
#   5. Convert wide format to long format (item-level)
#   6. Parse GG code components (ability, timing)
#   7. Apply scoring rules and missing data codes
#   8. Handle wheelchair vs walking determination
#   9. Calculate composite scores (self-care, mobility, total)
#   10. Create scale-level and item-level datasets
#
# Inputs:
#   - dt_first_post_acute_elig.parquet (cohort)
#   - MDS_1922 (Minimum Data Set 2019-2022)
#   - OASIS_1922 (Home Health OASIS 2019-2022)
#   - IRFPAI_1922 (Inpatient Rehab Facility PAI 2019-2022)
#
# Outputs:
#   - gg_complete_base.parquet: Raw GG codes from all sources
#   - gg_complete_base2.parquet: Processed GG codes with scoring
#   - gg_meta.parquet: Assessment metadata
#   - gg_complete_scale.parquet: Scale-level scores (self-care, mobility)
#   - gg_complete_item.parquet: Item-level scores with labels
#
# GG Code Structure:
#   - GG0130: Self-care items (A-H: eating, oral hygiene, toileting, etc.)
#   - GG0170: Mobility items (A-S: rolling, transfers, walking, stairs, etc.)
#   - Timing: ADMSN (admission), DSCHRG (discharge), GOAL (goal)
#   - Scoring: 01-06 (dependent to independent), 07-10/88 (not attempted/applicable)
#
# Scoring Rules:
#   - Valid scores: 01 (dependent) through 06 (independent)
#   - Missing codes: 07 (refused), 09 (not applicable), 10 (not attempted), 88 (not assessed)
#   - Missing codes recoded to 1 for scoring
#   - Self-care total: Sum of 3 items (eating, oral hygiene, toileting) - min 3 required
#   - Mobility total: Sum of 7 items (roll, lying-sitting, sit-stand, transfers, walking) - min 7 required
#   - Walking vs wheelchair: If walk not attempted, use wheelchair scores
#
################################################################################

# ==============================================================================
# SETUP
# ==============================================================================

# Source configuration
source("00_config.R")

# Load required packages
load_packages(c("dplyr", "dbplyr", "DBI", "arrow", "data.table",
                "table1", "stringr", "tidyr"))

cat("\n=============================================================================\n")
cat("SCRIPT 05A: EXTRACT AND PROCESS GG FUNCTIONAL ASSESSMENT CODES\n")
cat("=============================================================================\n\n")

# Set working directory
setwd(project_base)

# ==============================================================================
# DATABASE CONNECTION
# ==============================================================================

cat("Establishing database connection...\n")
con <- connect_db(database = db_project, server = db_server)

# ==============================================================================
# MODULE 1: LOAD ELIGIBLE COHORT
# ==============================================================================

cat("\n--- MODULE 1: Load Eligible Cohort ---\n")

cat("Loading eligible post-acute care cohort...\n")
dt_elig_index <- load_parquet_safe("dt_first_post_acute_elig.parquet") %>% 
  transmute(PatientICN, SCRSSN = scrssn) %>% 
  distinct()

log_count(dt_elig_index, "Eligible patients for GG code extraction")

# ==============================================================================
# MODULE 2: EXTRACT MDS GG CODES (SKILLED NURSING FACILITIES)
# ==============================================================================

cat("\n--- MODULE 2: Extract MDS GG Codes ---\n")

cat("Loading MDS 2019-2022 data...\n")
cat("Note: This is a large dataset and may take several minutes...\n\n")

mds1922 <- tbl(con, in_schema(schema_dflt, 'MDS_1922'))

cat("Extracting GG codes and assessment metadata from MDS...\n")
cat("GG items:\n")
cat("  GG0130: Self-care (eating, oral hygiene, toileting, bathing, dressing)\n")
cat("  GG0170: Mobility (rolling, transfers, walking, stairs)\n\n")

mds1922_b <- mds1922 %>%
  transmute(
    SCRSSN,
    # Dates
    admit_date = A1900_ADMSN_DT,
    exit_date = A2000_DSCHRG_DT,
    assessment_date = TRGT_DT,
    # Assessment type indicators
    assessment_occur = MDS_ITM_SBST_CD,
    admit_rec = if_else(
      A0310C_PPS_OMRA_CD %in% c("01", "03") | A0310A_FED_OBRA_CD == "01", 
      1, 0
    ),
    reentry_rec = if_else(A1700_ENTRY_TYPE_CD == 2, 1, 0),
    followup_rec = if_else(
      A0310A_FED_OBRA_CD %in% c("02", "03") | 
        A0310B_PPS_CD %in% c("01", "02", "03", "04", "05"), 
      1, 0
    ),
    exit_rec = if_else(
      A0310C_PPS_OMRA_CD %in% c("02", "03") | 
        A0310F_ENTRY_DSCHRG_CD %in% c(10, 11, 12), 
      1, 0
    ),
    discharge_unpl = if_else(
      !is.na(A0310G_PLND_DSCHRG_CD) & A0310G_PLND_DSCHRG_CD == 2, 
      1, 0
    ),
    died = if_else(A0310F_ENTRY_DSCHRG_CD == 12, 1, 0),
    # GG Self-care items (admission, goal, discharge),
    prior_self_care = GG0100A_PRIOR_SELF_CARE_IND,
    prior_mobility = GG0100B_PRIOR_INDR_MBLTY_IND,
    prior_cognition = GG0100D_PRIOR_FNCTNL_CGNTN_IND,
  #  bims = C0500_BIMS_SCRE_NUM,
    GG0130A1_EATG_SELF_ADMSN_CD, GG0130A2_EATG_SELF_GOAL_CD, 
    GG0130A3_EATG_SELF_DSCHRG_CD,
    GG0130B1_ORAL_HYGNE_ADMSN_CD, GG0130B2_ORAL_HYGNE_GOAL_CD, 
    GG0130B3_ORAL_HYGNE_DSCHRG_CD,
    GG0130C1_TOILT_HYGNE_ADMSN_CD, GG0130C2_TOILT_HYGNE_GOAL_CD, 
    GG0130C3_TOILT_HYGNE_DSCHRG_CD,
    GG0130E1_BTHE_SELF_STRT_CD, GG0130E2_BTHE_SELF_GOAL_CD, 
    GG0130E3_BTHE_SELF_END_CD,
    GG0130F1_UPR_DRSNG_STRT_CD, GG0130F2_UPR_DRSNG_GOAL_CD, 
    GG0130F3_UPR_DRSNG_END_CD,
    GG0130G1_LWR_DRSNG_STRT_CD, GG0130G2_LWR_DRSNG_GOAL_CD, 
    GG0130G3_LWR_DRSNG_END_CD,
    GG0130H1_ON_OFF_FTWR_STRT_CD, GG0130H2_ON_OFF_FTWR_GOAL_CD, 
    GG0130H3_ON_OFF_FTWR_END_CD,
    # GG Mobility items (admission, goal, discharge)
    GG0170A1_ROLL_STRT_CD, GG0170A2_ROLL_GOAL_CD, GG0170A3_ROLL_END_CD,
    GG0170B1_SIT_ADMSN_CD, GG0170B2_SIT_GOAL_CD, GG0170B3_SIT_DSCHRG_CD,
    GG0170C1_LYNG_ADMSN_CD, GG0170C2_LYNG_GOAL_CD, GG0170C3_LYNG_DSCHRG_CD,
    GG0170D1_STAND_ADMSN_CD, GG0170D2_STAND_GOAL_CD, GG0170D3_STAND_DSCHRG_CD,
    GG0170E1_CHR_TRNSF_ADMSN_CD, GG0170E2_CHR_TRNSF_GOAL_CD, 
    GG0170E3_CHR_TRNSF_DSCHRG_CD,
    GG0170F1_TOILT_TRNSF_ADMSN_CD, GG0170F2_TOILT_TRNSF_GOAL_CD, 
    GG0170F3_TOILT_TRNSF_DSCHRG_CD,
    GG0170G1_CAR_TRNSFR_ADMSN_CD, GG0170G2_CAR_TRNSFR_GOAL_CD, 
    GG0170G3_CAR_TRNSFR_DSCHRG_CD,
    GG0170I1_WLK_10_FEET_ADMSN_CD, GG0170I2_WLK_10_FEET_GOAL_CD, 
    GG0170I3_WLK_10_FEET_DSCHRG_CD,
    GG0170J1_WLK_50_ADMSN_CD, GG0170J2_WLK_50_GOAL_CD, GG0170J3_WLK_50_DSCHRG_CD,
    GG0170K1_WLK_150_ADMSN_CD, GG0170K2_WLK_150_GOAL_CD, 
    GG0170K3_WLK_150_DSCHRG_CD,
    GG0170L1_WLK_10_UNEVEN_ADMSN_CD, GG0170L2_WLK_10_UNEVEN_GOAL_CD, 
    GG0170L3_WLK_10_UNEVEN_DSCHRG_CD,
    GG0170M1_STP_1_ADMSN_CD, GG0170M2_STP_1_GOAL_CD, GG0170M3_STP_1_DSCHRG_CD,
    GG0170N1_STP_4_ADMSN_CD, GG0170N2_STP_4_GOAL_CD, GG0170N3_STP_4_DSCHRG_CD,
    GG0170O1_STP_12_ADMSN_CD, GG0170O2_STP_12_GOAL_CD, GG0170O3_STP_12_DSCHRG_CD,
    GG0170P1_PCKP_OBJ_ADMSN_CD, GG0170P2_PCKP_OBJ_GOAL_CD, 
    GG0170P3_PCKP_OBJ_DSCHRG_CD,
    GG0170Q1_WLCHR_ADMSN_CD, GG0170Q3_WLCHR_DSCHRG_CD,
    GG0170R1_WHL_50_ADMSN_CD, GG0170R2_WHL_50_GOAL_CD, GG0170R3_WHL_50_DSCHRG_CD,
    GG0170RR1_WHLCHR_50_ADMSN_CD, GG0170RR3_WHLCHR_50_DSCHRG_CD,
    GG0170S1_WHL_150_ADMSN_CD, GG0170S2_WHL_150_GOAL_CD, 
    GG0170S3_WHL_150_DSCHRG_CD,
    GG0170SS1_WHLCHR_150_ADMSN_CD, GG0170SS3_WHLCHR_150_DSCHRG_CD,
    # Source identifier
    source = "MDS"
  ) %>% 
  collect()

log_count(mds1922_b, "MDS assessments (raw)")

# Filter to cohort patients and create record ID
cat("Filtering to cohort patients...\n")
mds1922_c <- mds1922_b %>%
  mutate(
    record_id = paste0(row_number(), "_", source),
    SCRSSN = as.character(sprintf("%09d", as.integer(SCRSSN)))
  ) %>%
  inner_join(dt_elig_index, by = 'SCRSSN') %>% 
  select(
    SCRSSN, 
    record_id,
    source, 
    admit_date,
    admit_rec,
    reentry_rec,
    followup_rec,
    exit_rec,
    discharge_unpl,
    died,
    assessment_date,
    exit_date,
    assessment_occur,
    prior_self_care,
    prior_mobility,
    prior_cognition,
    starts_with("GG0130"),  # Self-care codes
    starts_with("GG0170"),  # Mobility codes
    -matches("[A-Z]5")      # Remove interim performance codes
  )

# Fill discharge date forward within admission episodes
cat("Filling discharge dates within admission episodes...\n")
mds1922_d <- mds1922_c %>% 
  arrange(SCRSSN, admit_date, assessment_date) %>% 
  group_by(SCRSSN, admit_date) %>% 
  tidyr::fill(exit_date, .direction = "up") %>%
  ungroup()

log_count(mds1922_d, "MDS assessments for cohort")

# ==============================================================================
# MODULE 3: EXTRACT OASIS GG CODES (HOME HEALTH)
# ==============================================================================

cat("\n--- MODULE 3: Extract OASIS GG Codes ---\n")

cat("Loading OASIS 2019-2022 data...\n")

oasis1922 <- tbl(con, in_schema(schema_dflt, 'OASIS_1922')) %>% 
  collect() %>%
  mutate(
    # Dates
    admit_date = M0030_SOC_DT,
    assessment_date = ASMT_EFF_DATE,
    exit_date = M0906_DC_TR_DTH_DT,
    # Assessment type indicators
    # M0100_ASSMT_REASON: 01/02=Start of Care, 03=Resumption, 04/05=Follow-up, 
    # 06/07=Transfer, 08=Death, 09/10=Discharge
    admit_rec = if_else(M0100_ASSMT_REASON %in% c("01", "02"), 1, 0),
    followup_rec = if_else(M0100_ASSMT_REASON %in% c("04", "05"), 1, 0),
    reentry_rec = if_else(M0100_ASSMT_REASON == "03", 1, 0),
    exit_rec = if_else(M0100_ASSMT_REASON %in% c("06", "07", "09", "10"), 1, 0),
    discharge_unpl = NA_real_,  # Not available in OASIS
    died = if_else(M0100_ASSMT_REASON == "08", 1, 0),
    assessment_occur = M0100_ASSMT_REASON,
    prior_self_care = GG0100A_PRIOR_SELF_CARE_IND,
    prior_mobility = GG0100B_PRIOR_INDR_MBLTY_IND,
    prior_cognition = GG0100D_PRIOR_FNCTNL_CGNTN_IND,
   # bims = C0500_BIMS_SCRE_NUM, we don't have it for the OASIS...
    source = "OASIS"
  ) %>%
  mutate(
    record_id = paste0(row_number(), "_", source),
    SCRSSN = as.character(sprintf("%09d", as.integer(SCRSSN)))
  ) %>% 
  inner_join(dt_elig_index, by = 'SCRSSN') %>% 
  select(
    SCRSSN,
    record_id,
    source,
    admit_date,
    assessment_date,
    exit_date,
    assessment_occur,
    admit_rec,
    reentry_rec,
    followup_rec,
    exit_rec,
    discharge_unpl,
    died,
    prior_self_care,
    prior_mobility,
    prior_cognition,
    starts_with("GG0130"),  # Self-care codes
    starts_with("GG0170")   # Mobility codes
  )

log_count(oasis1922, "OASIS assessments (raw)")

# Fill discharge date forward within admission episodes
cat("Filling discharge dates within admission episodes...\n")
oasis1922 <- oasis1922 %>% 
  arrange(SCRSSN, admit_date, assessment_date) %>% 
  group_by(SCRSSN, admit_date) %>% 
  tidyr::fill(exit_date, .direction = "up") %>%
  ungroup()

log_count(oasis1922, "OASIS assessments for cohort")

# ==============================================================================
# MODULE 4: EXTRACT IRF-PAI GG CODES (INPATIENT REHAB FACILITIES)
# ==============================================================================

cat("\n--- MODULE 4: Extract IRF-PAI GG Codes ---\n")

cat("Loading IRF-PAI 2019-2022 data...\n")
cat("Note: IRF-PAI uses same form for admission and discharge assessments\n\n")

irfpai1922 <- tbl(con, in_schema(schema_dflt, 'IRFPAI_1922')) %>%
  collect() %>% 
  mutate(
    # Dates
    admit_date = ADMSN_DT,
    assessment_date = TRGT_DT,
    exit_date = DSCHRG_DT,
    # Assessment type indicators
    # ADMSN_CLS_CD: 01=Admission, 02=Evaluation, 03=Readmission, 
    # 04=Unplanned discharge, 05=Continuing rehab
    assessment_occur = ADMSN_CLS_CD,
    SCRSSN = as.character(sprintf("%09d", as.integer(SCRSSN))),
    admit_rec = if_else(ADMSN_CLS_CD == "01", 1, 0),
    reentry_rec = if_else(ADMSN_CLS_CD %in% c("03", "05"), 1, 0),
    followup_rec = 0,
    exit_rec = 1,  # All IRF-PAI records are discharge records
    discharge_unpl = if_else(ADMSN_CLS_CD == "04", 1, 0),
    died = if_else(DSCHRG_ALIVE_IND == 0, 1, 0),
    source = "IRFPAI",
    prior_self_care = GG0100A_PRIOR_SELF_CARE_IND,
    prior_mobility = GG0100B_PRIOR_INDR_MBLTY_IND,
    prior_cognition = GG0100D_PRIOR_FNCTNL_CGNTN_IND,
    record_id = paste0(row_number(), "_", source)
  ) %>%   
  inner_join(dt_elig_index, by = 'SCRSSN') %>%  
  select(
    record_id,
    SCRSSN, 
    source, 
    admit_date,
    assessment_date,
    exit_date,
    assessment_occur,
    admit_rec,
    reentry_rec,
    followup_rec,
    exit_rec,
    discharge_unpl,
    died,
    prior_self_care,
    prior_mobility,
    prior_cognition,
    starts_with("GG0130"),  # Self-care codes
    starts_with("GG0170")   # Mobility codes
  )

log_count(irfpai1922, "IRF-PAI assessments for cohort")

# ==============================================================================
# MODULE 5: CONVERT TO LONG FORMAT AND STANDARDIZE
# ==============================================================================

cat("\n--- MODULE 5: Convert to Long Format ---\n")

cat("Pivoting GG codes from wide to long format...\n")
cat("Removing GOAL codes (not used in analysis)...\n\n")

# Pivot MDS to long
mds1922_long <- mds1922_d %>% 
  tidyr::pivot_longer(
    cols = starts_with("GG"), 
    names_to = "gg_var", 
    values_to = "gg_val"
  ) %>%
  filter(!grepl("GOAL_CD$", gg_var))

# Pivot OASIS to long
oasis_long <- oasis1922 %>% 
  tidyr::pivot_longer(
    cols = starts_with("GG"), 
    names_to = "gg_var", 
    values_to = "gg_val"
  ) %>%
  filter(!grepl("GOAL_CD$", gg_var))

# Pivot IRF-PAI to long (remove wheelchair type and extra codes)
irfpai_long <- irfpai1922 %>% 
  tidyr::pivot_longer(
    cols = starts_with("GG"), 
    names_to = "gg_var", 
    values_to = "gg_val"
  ) %>%
  filter(!grepl("GOAL_CD$", gg_var)) %>%
  filter(!grepl("STRT_C$", gg_var)) %>%  # Scooter type indicator
  filter(!grepl("150_ST$", gg_var))

# Standardize SCRSSN format
mds1922_long$SCRSSN <- as.character(mds1922_long$SCRSSN)
oasis_long$SCRSSN <- as.character(oasis_long$SCRSSN)
irfpai_long$SCRSSN <- as.character(irfpai_long$SCRSSN)

# Combine all sources
cat("Combining MDS, OASIS, and IRF-PAI datasets...\n")
gg_complete_base <- rbind(mds1922_long, oasis_long, irfpai_long)

log_count(gg_complete_base, "Combined GG codes (long format)")

# Save base dataset
save_parquet_safe(gg_complete_base, "gg_complete_base.parquet")

# ==============================================================================
# MODULE 6: PARSE GG CODE COMPONENTS
# ==============================================================================

cat("\n--- MODULE 6: Parse GG Code Components ---\n")

gg_complete_base <- load_parquet_safe("gg_complete_base.parquet")

cat("Parsing GG variable names into ability and timing components...\n")

# Parse ability and timing from variable names
setDT(gg_complete_base)

gg_complete_base[, `:=`(
  ability = sub("_.*", "", gg_var),
  time1 = sub(".*?_", "", gg_var)
)]

gg_complete_base[!grepl("_", gg_var), `:=`(
  ability = gg_var,
  time1 = ""
)]

# Clean timing indicators
cat("Cleaning timing indicators...\n")
gg_complete_base$time1 <- sub("_CD$", "", gg_complete_base$time1)
gg_complete_base$time1 <- sub("_IND$", "", gg_complete_base$time1)
gg_complete_base$time1 <- gsub("CD", "", gg_complete_base$time1)
gg_complete_base$time1 <- sub("_C$", "", gg_complete_base$time1)

# Extract ability label and timing
gg_complete_base <- gg_complete_base %>% 
  mutate(
    ability_label = str_replace(time1, "_[^_]+$", ""),
    time2 = str_extract(time1, "[^_]+$")
  )

# Remove wheelchair type indicators
cat("Removing wheelchair type and other irrelevant codes...\n")
gg_complete_base <- gg_complete_base %>% 
  filter(time2 %!in% c("ST", "C"))

# Standardize timing codes
cat("Standardizing timing codes to ADMSN, DSCHRG, FLWP...\n")
gg_complete_base <- gg_complete_base %>% 
  mutate(
    time2 = if_else(gg_var == "GG0170C_MBLTY_PRFMNC_CD", "ADMSN", time2),
    time2 = if_else(gg_var == "GG0170C_MBLTY_DSCHRG_GOAL_CD", "GOAL", time2)
  )

# Create standardized time variable
setDT(gg_complete_base)
gg_complete_base <- gg_complete_base[, `:=`(
  time = case_when(
    time2 %in% c("ADMSN", "STRT") ~ "ADMSN",
    time2 %in% c("GOAL", "BYGOAL") | grepl("GOAL", ability_label) ~ "GOAL",
    time2 %in% c("END", "DSCHRG") ~ "DSCHRG",
    TRUE ~ "FLWP"
  )
)]

# Filter to relevant timing points and abilities
cat("Filtering to admission, discharge, and follow-up assessments...\n")
gg_complete_base <- gg_complete_base %>% 
  # Don't need these variables at all - wheelchair motorized or manual indicator.
  filter(
    ability %!in% c("GG0170SS1", "GG0170RR1", "GG0170SS3", "GG0170Q1", "GG0170Q3",
                    "GG0170RR3")
  ) %>% 
  filter(time %in% c("ADMSN", "DSCHRG", "FLWP")) %>%
  mutate(ability2 = substr(ability, 1, 7)) %>% 
  distinct()

gg_complete_base <- gg_complete_base %>% filter(ability2 != "GG0170Q")

log_count(gg_complete_base, "GG codes after parsing")

# ==============================================================================
# MODULE 7: APPLY SCORING RULES
# ==============================================================================

cat("\n--- MODULE 7: Apply Scoring Rules ---\n")

cat("Converting missing codes to NA and creating raw value backup...\n")
gg_complete_base2 <- gg_complete_base %>% 
  mutate(
    gg_val = if_else(gg_val == "-", NA, gg_val),
    gg_val_raw = gg_val
  )

# Save metadata separately
cat("Extracting assessment metadata...\n")
gg_meta <- gg_complete_base2 %>% 
  select(SCRSSN, record_id, source, exit_date, assessment_occur, 
         admit_rec, reentry_rec, followup_rec, exit_rec, 
         discharge_unpl, died, prior_cognition, prior_mobility, prior_self_care) %>% 
  distinct()

save_parquet_safe(gg_meta, "gg_meta.parquet")
log_count(gg_meta, "Unique assessments")

# Simplify dataset for scoring
cat("Simplifying dataset for scoring...\n")
gg_complete_base2 <- gg_complete_base2 %>% 
  select(-source, -exit_date, -assessment_occur, -admit_rec, -reentry_rec, 
         -followup_rec, -exit_rec, -discharge_unpl, -died, -prior_cognition, -prior_mobility, -prior_self_care) %>% 
  distinct()

gg_complete_base2 <- gg_complete_base2 %>% 
  select(-time1, -time2, -ability_label, -gg_var, -ability) %>% 
  distinct()

# Filter to core functional items used in composite scores
cat("Filtering to core functional assessment items...\n")
cat("  Self-care: GG0130A (eating), GG0130B (oral hygiene), GG0130C (toileting)\n")
cat("  Mobility: GG0170A (roll), GG0170C (lying-sitting), GG0170D (sit-stand),\n")
cat("            GG0170E (chair/bed transfer), GG0170F (toilet transfer),\n")
cat("            GG0170I (walk 10), GG0170J (walk 50), GG0170R (wheel 50),\n")
cat("            GG0170S (wheel 150)\n\n")

#gg_complete_base2 <- gg_complete_base2 %>% 
#  filter(ability2 %in% c("GG0130A", "GG0130B", "GG0130C", "GG0170A", 
#                         "GG0170C", "GG0170D", "GG0170E", "GG0170F", 
#                         "GG0170I", "GG0170J", "GG0170R", "GG0170S"))

# Convert to factors for efficiency
gg_complete_base2 <- gg_complete_base2 %>% 
  ungroup() %>% 
  mutate(
    SCRSSN = factor(SCRSSN),
    record_id = factor(record_id),
    gg_val = factor(gg_val),
    time = factor(time),
    ability2 = factor(ability2)
  )

log_count(gg_complete_base2, "Core functional items")

# Save intermediate dataset
save_parquet_safe(gg_complete_base2, "gg_complete_base2.parquet")

# ==============================================================================
# MODULE 8: SCORE GG CODES
# ==============================================================================

cat("\n--- MODULE 8: Score GG Codes ---\n")

cat("Applying GG scoring algorithm...\n")
cat("Valid scores: 01-06 (01=dependent, 06=independent)\n")
cat("Missing codes recoded to 1: 07 (refused), 09 (not applicable),\n")
cat("                             10 (not attempted), 88 (not assessed)\n\n")

setDT(gg_complete_base2)
gg_complete_base2 <- gg_complete_base2[, `:=`(
  # Score: convert missing codes to 1, keep valid scores as numeric
  gg_score = if_else(
    gg_val %in% c("07", "09", "10", "88", "99"), 
    1, 
    as.numeric(gg_val)
  ),
  # Missing code tracker
  missing_code = if_else(
    gg_val %in% c("-", "07", "09", "10", "88"), 
    gg_val, 
    NA_character_
  ),
  # Individual missing code indicators
  scored_07 = if_else(gg_val == "07", 1, 0),  # Refused
  scored_09 = if_else(gg_val == "09", 1, 0),  # Not applicable
  scored_10 = if_else(gg_val == "10", 1, 0),  # Not attempted
  scored_88 = if_else(gg_val == "88", 1, 0)   # Not assessed
)]

log_count(gg_complete_base2, "Scored GG items")

# Save scored dataset
save_parquet_safe(gg_complete_base2, "gg_complete_base2.parquet")

# ==============================================================================
# MODULE 9: CALCULATE COMPOSITE SCORES
# ==============================================================================

cat("\n--- MODULE 9: Calculate Composite Scores ---\n")

gg_complete_base2 <- load_parquet_safe("gg_complete_base2.parquet")

cat("Converting to wide format for composite score calculation...\n")

# Widen dataset for composite scores
gg_complete_wide <- gg_complete_base2 %>%
  select(
    record_id,
    SCRSSN, 
    time, 
    ability2,
    admit_date, 
    gg_val,
    gg_score,
    assessment_date
  ) %>% 
  tidyr::pivot_wider(
    names_from = "ability2", 
    values_from = c("gg_score", "gg_val"), 
    names_sep = "_"
  ) %>% 
  arrange(SCRSSN, admit_date, assessment_date)

log_count(gg_complete_wide, "Assessments in wide format")

# Calculate composite scores
cat("\nCalculating composite scores...\n")
cat("  Walk Score: Walk 10 (GG0170I) + Walk 50 (GG0170J)\n")
cat("  Wheel Score: Wheel 50 (GG0170R) + Wheel 150 (GG0170S)\n")
cat("  Walking or Wheeling Score:\n")
cat("    - If any walk item (I or J) has valid score 01-06: use Walk Score\n")
cat("    - Otherwise: use Wheel Score\n")
cat("    - All missing/invalid values already imputed to 1 in scoring step\n\n")

gg_complete_wide <- gg_complete_wide %>% 
  mutate(
    gg_score_GG0170I = if_else(is.na(gg_score_GG0170I), 1, gg_score_GG0170I),
    gg_score_GG0170J = if_else(is.na(gg_score_GG0170J), 1, gg_score_GG0170J),
    gg_score_GG0170R = if_else(is.na(gg_score_GG0170R), 1, gg_score_GG0170R),
    gg_score_GG0170S = if_else(is.na(gg_score_GG0170S), 1, gg_score_GG0170S),
    
    # Create Walk Score (Walk 10 + Walk 50)
    Walk_Score = gg_score_GG0170I + gg_score_GG0170J,
    # Create Wheel Score (Wheel 50 + Wheel 150)
    Wheel_Score = gg_score_GG0170R + gg_score_GG0170S,
    # Check if any walk item has valid score (01-06)
    valid_walk = if_else((gg_val_GG0170I %in% c("01", "02", "03", "04", "05", "06")) | 
      (gg_val_GG0170J %in% c("01", "02", "03", "04", "05", "06")), 1L, 0L),
    valid_wheel = if_else((gg_val_GG0170R %in% c("01", "02", "03", "04", "05", "06")) | 
      (gg_val_GG0170S %in% c("01", "02", "03", "04", "05", "06")), 1L, 0L),
    # Use Walk Score if any walk item is valid, otherwise use Wheel Score
    WALK_OR_WHEEL_SCORE = if_else(is.na(gg_val_GG0170I) & 
                                  is.na(gg_val_GG0170J) & 
                                  is.na(gg_val_GG0170R) &
                                  is.na(gg_val_GG0170S), NA,
                                  if_else(valid_walk == 1L, Walk_Score, Wheel_Score))
  ) %>%
  rowwise() %>%
  mutate(
    # Self-care total: eating + oral hygiene + toileting
    SELFCARE_total = sum(
      gg_score_GG0130A, gg_score_GG0130B, gg_score_GG0130C, 
      na.rm = TRUE
    ),
    # Mobility total: roll + lying-sitting + sit-stand + transfers + walking/wheeling
    MOBILITY_total = sum(
      gg_score_GG0170A, gg_score_GG0170C, gg_score_GG0170D, 
      gg_score_GG0170E, gg_score_GG0170F, WALK_OR_WHEEL_SCORE, 
      na.rm = TRUE
    ),
    any_walk_wheel_valid = max(valid_walk, valid_wheel)
  ) %>%
  ungroup()

# Apply minimum item requirements
cat("Applying minimum item requirements:\n")
cat("  Self-care: Requires at least 3 items (all 3 core items)\n")
cat("  Mobility: Requires at least 7 items (all 7 core items)\n")
cat("  Total GG: Requires at least 10 items combined\n\n")

setDT(gg_complete_wide)
gg_complete_wide <- gg_complete_wide[, `:=`(
  SELFCARE_total = if_else(SELFCARE_total < 3, NA_real_, SELFCARE_total), 
  MOBILITY_total = if_else(MOBILITY_total < 7, NA_real_, MOBILITY_total),
  # Check if any walking/wheeling item present
  any_IJRS = if_else(!is.na(gg_val_GG0170I) | !is.na(gg_val_GG0170J) | !is.na(gg_val_GG0170R) | !is.na(gg_val_GG0170S), 1, 0
  )
)]

# Calculate total GG score
gg_complete_wide <- gg_complete_wide %>% 
  rowwise() %>%
  mutate(
    GG_total = sum(SELFCARE_total, MOBILITY_total, na.rm = TRUE)
  ) %>%
  ungroup()

setDT(gg_complete_wide)
gg_complete_wide <- gg_complete_wide[, `:=`(
  GG_total = if_else(GG_total < 10, NA_real_, GG_total)
)]

# Count valid items
cat("Counting valid items per assessment...\n")
setDT(gg_complete_wide)
gg_complete_wide <- gg_complete_wide[, `:=`(
  # Count non-NA items in core abilities
  count_not_na = rowSums(!is.na(.SD))
), .SDcols = patterns("^gg_val_(GG0130A|GG0130B|GG0130C|GG0170A|GG0170C|GG0170D|GG0170E|GG0170F)$")]

# Add walking/wheeling item to count
gg_complete_wide <- gg_complete_wide[, `:=`(
  count_not_na = count_not_na + any_IJRS
)]

# Count valid scores (01-06)
gg_complete_wide <- gg_complete_wide[, `:=`(
  count_valid_ALL = Reduce(
    `+`, 
    lapply(.SD, function(x) x %in% c("01", "02", "03", "04", "05", "06")) # RR and SS are == 1 or 2.
  )
), .SDcols = patterns("^gg_val_(GG0130|GG0170)")]

# Count valid scores (01-06)
gg_complete_wide <- gg_complete_wide[, `:=`(
  count_valid = Reduce(
    `+`, 
    lapply(.SD, function(x) x %in% c("01", "02", "03", "04", "05", "06"))
  )
), .SDcols = patterns("^gg_val_(GG0130A|GG0130B|GG0130C|GG0170A|GG0170C|GG0170D|GG0170E|GG0170F)$")]

# Add walking/wheeling item to count
gg_complete_wide <- gg_complete_wide[, `:=`(
  count_valid = count_valid + any_walk_wheel_valid
)]

gg_complete_wide <- gg_complete_wide %>% 
  mutate(source = case_when(
    str_ends(record_id, "MDS") ~ "MDS",
    str_ends(record_id, "OASIS") ~ "OASIS",
    str_ends(record_id, "IRFPAI") ~ "IRFPAI",
    TRUE ~ NA_character_
  ))

save_parquet_safe(gg_complete_wide, "gg_complete_wide.parquet")
log_count(gg_complete_wide, "Scale-level assessments (wide)")

# ==============================================================================
# MODULE 10: CREATE FINAL DATASETS
# ==============================================================================

cat("\n--- MODULE 10: Create Final Datasets ---\n")

# Load metadata
gg_meta <- load_parquet_safe("gg_meta.parquet")

# Create scale-level dataset
cat("Creating scale-level dataset...\n")
gg_complete_scale <- gg_complete_wide %>% select(-source)%>% 
  distinct() %>%
  left_join(gg_meta, by = c("SCRSSN", "record_id")) %>%
  mutate(prior_cognition_val = prior_cognition,
         prior_self_care_val = prior_self_care,
         prior_mobility_val = prior_mobility,
         prior_cognition_score = as.numeric(if_else(prior_cognition_val %in% c("8", "9"), "1", prior_cognition_val)),
         prior_self_care_score = as.numeric(if_else(prior_self_care_val %in% c("8", "9"), "1", prior_self_care_val)),
         prior_mobility_score = as.numeric(if_else(prior_mobility_val %in% c("8", "9"), "1", prior_mobility))) %>%
  select(
    SCRSSN, 
    record_id,
    source, 
    time, 
    admit_date, 
    assessment_date, 
    count_not_na,
    count_valid,
    count_valid_ALL,
    exit_date,
    assessment_occur,
    admit_rec,
    reentry_rec,
    followup_rec,
    exit_rec,
    discharge_unpl,
    died, 
    prior_cognition_val,
    prior_self_care_val,
    prior_mobility_val,
    prior_cognition_score,
    prior_self_care_score,
    prior_mobility_score,
    GG_total,
    SELFCARE_total,
    MOBILITY_total,
    gg_val_GG0130A:gg_val_GG0170S,
    Walk_Score,
    Wheel_Score,
    valid_walk,
    valid_wheel,
    any_walk_wheel_valid,
    WALK_OR_WHEEL_SCORE
  )

save_parquet_safe(gg_complete_scale, "gg_complete_scale.parquet")
log_count(gg_complete_scale, "Scale-level assessments")

# Create item-level dataset with labels
cat("\nCreating item-level dataset with ability labels...\n")

gg_complete_base2 <- load_parquet_safe("gg_complete_base2.parquet")

gg_complete_item <- gg_complete_base2 %>%
  left_join(gg_meta, by = c("SCRSSN", "record_id")) %>%
  mutate(
    gg_val = if_else(is.na(gg_val), "NA", as.character(gg_val)),
    ability = case_when(
      # Self-care items
      grepl("GG0130A", ability2) ~ "1-A. EATING",
      grepl("GG0130B", ability2) ~ "1-B. ORAL HYGIENE",
      grepl("GG0130C", ability2) ~ "1-C. TOILETING",
      grepl("GG0130D", ability2) ~ "1-D. WASH UPPER",
      grepl("GG0130E", ability2) ~ "1-E. SHOWER BATHE",
      grepl("GG0130F", ability2) ~ "1-F. UPPER BODY DRESS",
      grepl("GG0130G", ability2) ~ "1-G. LOWER BODY DRESS",
      grepl("GG0130H", ability2) ~ "1-H. FOOTWEAR",
      # Mobility items
      grepl("GG0170A", ability2) ~ "2-A. ROLL LEFT/RIGHT",
      grepl("GG0170B", ability2) ~ "2-B. SIT TO LYING",
      grepl("GG0170C", ability2) ~ "2-C. LYING TO SIT",
      grepl("GG0170D", ability2) ~ "2-D. SIT TO STAND",
      grepl("GG0170E", ability2) ~ "2-E. CHAIR/BED TRANSF",
      grepl("GG0170F", ability2) ~ "2-F. TOILET TRANSFER",
      grepl("GG0170G", ability2) ~ "2-G. CAR TRANSFER",
      grepl("GG0170I", ability2) ~ "2-I. WALK 10",
      grepl("GG0170J", ability2) ~ "2-J. WALK 50 2 TURNS",
      grepl("GG0170K", ability2) ~ "2-K. WALK 150",
      grepl("GG0170L", ability2) ~ "2-L. WALK 10 UNEVEN",
      grepl("GG0170M", ability2) ~ "2-M. 1 STEP CURB",
      grepl("GG0170N", ability2) ~ "2-N. 4 STEPS",
      grepl("GG0170O", ability2) ~ "2-O. 12 STEPS",
      grepl("GG0170P", ability2) ~ "2-P. PICKING UP OBJ",
      grepl("GG0170R", ability2) ~ "2-R. WHEEL 50 FEET",
      grepl("GG0170S", ability2) ~ "2-S. WHEEL 150 FEET",
      TRUE ~ NA_character_
    )
  )

save_parquet_safe(gg_complete_item, "gg_complete_item.parquet")
log_count(gg_complete_item, "Item-level records")

# ==============================================================================
# DATA VALIDATION AND SUMMARY STATISTICS
# ==============================================================================

cat("\n--- Data Validation and Summary Statistics ---\n")

# Summary by source
cat("\nAssessments by data source:\n")
source_summary <- gg_complete_scale %>%
  group_by(source) %>%
  summarize(
    n_assessments = n(),
    n_patients = n_distinct(SCRSSN),
    mean_gg_total = mean(GG_total, na.rm = TRUE),
    mean_selfcare = mean(SELFCARE_total, na.rm = TRUE),
    mean_mobility = mean(MOBILITY_total, na.rm = TRUE),
    pct_missing_total = sum(is.na(GG_total)) / n() * 100,
    .groups = 'drop'
  )

print(source_summary)

# Summary by timing
cat("\nAssessments by timing:\n")
timing_summary <- gg_complete_scale %>%
  group_by(time, source) %>%
  summarize(
    n = n(),
    mean_gg_total = mean(GG_total, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_wider(names_from = source, values_from = c(n, mean_gg_total))

print(timing_summary)

# Walking vs wheeling summary
cat("\nWalking vs wheeling distribution:\n")
walk_wheel_summary <- gg_complete_scale %>%
  filter(count_not_na > 0) %>%
  group_by(time, source) %>%
  summarize(
    n = n(),
    valid_walk_count = sum(valid_walk == TRUE, na.rm = TRUE),
    valid_wheel_count = sum(valid_wheel == TRUE, na.rm = TRUE),
    invalid_count = sum(valid_wheel == F & valid_walk == F, na.rm = TRUE),
    pct_valid_walk = sum(valid_walk == TRUE, na.rm = TRUE) / n() * 100,
    pct_valid_wheel = sum(valid_wheel == TRUE, na.rm = TRUE) / n() * 100,
    pct_invalid = sum(valid_wheel == F & valid_walk == F, na.rm = TRUE) / n() * 100,
    mean_walk_score = mean(Walk_Score[valid_walk == TRUE], na.rm = TRUE),
    mean_wheel_score = mean(Wheel_Score[valid_wheel == TRUE], na.rm = TRUE),
    mean_walk_or_wheel_WALK = mean(WALK_OR_WHEEL_SCORE[valid_walk == TRUE], na.rm = TRUE),
    mean_walk_or_wheel_WHEEL = mean(WALK_OR_WHEEL_SCORE[valid_wheel == TRUE], na.rm = TRUE),
  )

print(walk_wheel_summary)

# ==============================================================================
# SESSION INFO
# ==============================================================================

cat("\n=============================================================================\n")
cat("SCRIPT 05A COMPLETED SUCCESSFULLY\n")
cat("=============================================================================\n")
cat("\nKey Output Files:\n")
cat("  - gg_complete_base.parquet: Raw GG codes from all sources\n")
cat("  - gg_complete_base2.parquet: Processed GG codes with scoring\n")
cat("  - gg_meta.parquet: Assessment metadata\n")
cat("  - gg_complete_scale.parquet: Scale-level scores (self-care, mobility)\n")
cat("  - gg_complete_item.parquet: Item-level scores with labels\n\n")

cat("Summary Statistics:\n")
cat("  Total assessments:", 
    format(nrow(gg_complete_scale), big.mark = ","), "\n")
cat("  Unique patients:", 
    format(n_distinct(gg_complete_scale$SCRSSN), big.mark = ","), "\n")
cat("  MDS assessments:", 
    sum(gg_complete_scale$source == "MDS"), "\n")
cat("  OASIS assessments:", 
    sum(gg_complete_scale$source == "OASIS"), "\n")
cat("  IRF-PAI assessments:", 
    sum(gg_complete_scale$source == "IRFPAI"), "\n")
cat("=============================================================================\n\n")

# Clean up
dbDisconnect(con)

################################################################################
# END OF SCRIPT
################################################################################
