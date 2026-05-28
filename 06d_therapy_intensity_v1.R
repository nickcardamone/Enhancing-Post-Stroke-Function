
cat("SCRIPT 06d: EXTRACT THERAPY MINUTES / DAYS")

# 5/27/2026: We ended up not implementing these variables 

# Source configuration
source("00_config.R")

# Load required packages
load_packages(
  c(
    "dplyr",
    "dbplyr",
    "DBI",
    "VINCI",
    "arrow",
    "data.table",
    "table1",
    "stringr",
    "tidyr",
    "openxlsx"
  )
)


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
# MODULE 2: LOAD MDS, OASIS, and IRFPAI DATA 2019-2022
# ==============================================================================

cat("\n--- MODULE 2: Extract MDS GG Codes ---\n")

cat("Loading MDS 2019-2022 data...\n")
cat("Note: This is a large dataset and may take several minutes...\n\n")

mds1922 <- tbl(con, in_schema(schema_dflt, 'MDS_1922')) %>%
  
  mds1922_1000 <- mds1922 %>%
  head(1000) %>%
  collect()

oasis1922 <- tbl(con, in_schema(schema_dflt, 'OASIS_1922')) %>%
  head(1000) %>%
  collect()

irfpai1922 <- tbl(con, in_schema(schema_dflt, 'IRFPAI_1922')) %>%
  head(1000) %>%
  collect()

wb <- createWorkbook()

addWorksheet(wb, "mds1922")
writeData(wb, "mds1922", mds1922_1000)

addWorksheet(wb, "oasis1922")
writeData(wb, "oasis1922", oasis1922)

addWorksheet(wb, "irfpai1922")
writeData(wb, "irfpai1922", irfpai1922)

saveWorkbook(wb, "mds_oasis_irfpai.xlsx", overwrite = T)


mds1922 <- tbl(con, in_schema(schema_dflt, 'MDS_1922'))

oasis1922 <- tbl(con, in_schema(schema_dflt, 'OASIS_1922')) %>%
  collect()

irfpai1922 <- tbl(con, in_schema(schema_dflt, 'IRFPAI_1922'))


# MDS variables
mds_selected <- mds1922 %>%
  mutate(
    SCRSSN,
    source = "MDS",
    # Dates
    admit_date = A1900_ADMSN_DT,
    exit_date = A2000_DSCHRG_DT,
    assessment_date = TRGT_DT,
    # Assessment type indicators
    assessment_occur = MDS_ITM_SBST_CD,
    admit_rec = if_else(
      A0310C_PPS_OMRA_CD %in% c("01", "03") | A0310A_FED_OBRA_CD == "01",
      1,
      0
    ),
    reentry_rec = if_else(A1700_ENTRY_TYPE_CD == 2, 1, 0),
    followup_rec = if_else(
      A0310A_FED_OBRA_CD %in% c("02", "03") |
        A0310B_PPS_CD %in% c("01", "02", "03", "04", "05"),
      1,
      0
    ),
    exit_rec = if_else(
      A0310C_PPS_OMRA_CD %in% c("02", "03") |
        A0310F_ENTRY_DSCHRG_CD %in% c(10, 11, 12),
      1,
      0
    ),
    discharge_unpl = if_else(!is.na(A0310G_PLND_DSCHRG_CD) &
                               A0310G_PLND_DSCHRG_CD == 2, 1, 0),
    died = if_else(A0310F_ENTRY_DSCHRG_CD == 12, 1, 0),
    THRPY_SPCH_DAYS = as.numeric(O0400A4_SPCH_THRPY_DAY_NUM),
    THRPY_OT_DAYS = as.numeric(O0400B4_OT_DAY_NUM),
    THRPY_PT_DAYS = as.numeric(O0400C4_PT_DAY_NUM),
    
    THRPY_SPCH_DAYS = if_else(THRPY_SPCH_DAYS == 0, NA, THRPY_SPCH_DAYS),
    THRPY_OT_DAYS = if_else(THRPY_OT_DAYS == 0, NA, THRPY_OT_DAYS),
    THRPY_PT_DAYS = if_else(THRPY_PT_DAYS == 0, NA, THRPY_PT_DAYS),
    
    THRPY_SPCH_IND_MINS = round(
      as.numeric(O0400A1_SPCH_THRPY_IND_MIN_NUM) / THRPY_SPCH_DAYS,
      2
    ),
    THRPY_SPCH_CNC_MINS = round(
      as.numeric(O0400A2_SPCH_THRPY_CNC_MIN_NUM) / THRPY_SPCH_DAYS,
      2
    ),
    THRPY_SPCH_GRP_MINS = round(
      as.numeric(O0400A3_SPCH_THRPY_GRP_MIN_NUM) / THRPY_SPCH_DAYS,
      2
    ),
    THRPY_SPCH_CO_MINS = round(as.numeric(O0400A3A_ST_CO_TRMT_MIN_NUM) /
                                 THRPY_SPCH_DAYS, 2),
    THRPY_OT_IND_MINS = round(as.numeric(O0400B1_OT_INDVDL_MIN_NUM) /
                                THRPY_OT_DAYS, 2),
    THRPY_OT_CNC_MINS = round(as.numeric(O0400B2_OT_CNCRNT_MIN_NUM) /
                                THRPY_OT_DAYS, 2),
    THRPY_OT_GRP_MINS = round(as.numeric(O0400B3_OT_GRP_MIN_NUM) /
                                THRPY_OT_DAYS, 2),
    THRPY_OT_CO_MINS = round(as.numeric(O0400B3A_OT_CO_TRMT_MIN_NUM) /
                               THRPY_OT_DAYS, 2),
    THRPY_PT_IND_MINS = round(as.numeric(O0400C1_PT_INDVDL_MIN_NUM) /
                                THRPY_PT_DAYS, 2),
    THRPY_PT_CNC_MINS = round(as.numeric(O0400C2_PT_CNCRNT_MIN_NUM) /
                                THRPY_PT_DAYS, 2),
    THRPY_PT_GRP_MINS = round(as.numeric(O0400C3_PT_GRP_MIN_NUM) /
                                THRPY_PT_DAYS, 2),
    THRPY_PT_CO_MINS = round(as.numeric(O0400C3A_PT_CO_TRMT_MIN_NUM) /
                               THRPY_PT_DAYS, 2)
  ) %>%
  collect() %>%
  mutate(record_id = paste0(row_number(), "_", source),
         SCRSSN = as.character(sprintf("%09d", as.integer(SCRSSN)))) %>%
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
    starts_with("THRPY")  # Therapy information
  )


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
    exit_rec = 1,
    # All IRF-PAI records are discharge records
    discharge_unpl = if_else(ADMSN_CLS_CD == "04", 1, 0),
    died = if_else(DSCHRG_ALIVE_IND == 0, 1, 0),
    source = "IRFPAI",
    record_id = paste0(row_number(), "_", source),
    THRPY_SPCH_IND_MINS = round((
      as.numeric(INDVDL_SPCH_THRPY_WK_1_NUM) + as.numeric(INDVDL_SPCH_THRPY_WK_2_NUM)
    ) / 14, 2),
    THRPY_SPCH_CNC_MINS  = round((
      as.numeric(CNCRNT_SPCH_THRPY_WK_1_NUM) + as.numeric(CNCRNT_SPCH_THRPY_WK_1_NUM)
    ) / 14, 2),
    THRPY_SPCH_GRP_MINS  = round((
      as.numeric(GRP_SPCH_THRPY_WK_1_NUM) + as.numeric(GRP_SPCH_THRPY_WK_1_NUM)
    ) / 14, 2),
    THRPY_SPCH_CO_MINS  = round((
      as.numeric(CO_TRTMT_SPCH_THRPY_WK_1_NUM) + as.numeric(CO_TRTMT_SPCH_THRPY_WK_1_NUM)
    ) / 14, 2),
    THRPY_OT_IND_MINS = round((
      as.numeric(INDVDL_OT_WK_1_NUM) + as.numeric(INDVDL_OT_WK_2_NUM)
    ) / 14, 2),
    THRPY_OT_CNC_MINS = round((
      as.numeric(CNCRNT_OT_WK_1_NUM) + as.numeric(CNCRNT_OT_WK_2_NUM)
    ) / 14, 2),
    THRPY_OT_GRP_MINS = round((
      as.numeric(GRP_OT_WK_1_NUM) + as.numeric(GRP_OT_WK_2_NUM)
    ) / 14, 2),
    THRPY_OT_CO_MINS = round((
      as.numeric(CO_TRTMT_OT_WK_1_NUM) + as.numeric(CO_TRTMT_OT_WK_2_NUM)
    ) / 14, 2),
    THRPY_PT_IND_MINS = round((
      as.numeric(INDVDL_PT_WK_1_NUM) + as.numeric(INDVDL_PT_WK_2_NUM)
    ) / 14, 2),
    THRPY_PT_CNC_MINS = round((
      as.numeric(CNCRNT_PT_WK_1_NUM) + as.numeric(CNCRNT_PT_WK_2_NUM)
    ) / 14, 2),
    THRPY_PT_GRP_MINS = round((
      as.numeric(GRP_PT_WK_1_NUM) + as.numeric(GRP_PT_WK_2_NUM)
    ) / 14, 2),
    THRPY_PT_CO_MINS = round((
      as.numeric(CO_TRTMT_PT_WK_1_NUM) + as.numeric(CO_TRTMT_PT_WK_2_NUM)
    ) / 14, 2)
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
    starts_with("THRPY")   # Mobility codes
  )


chk_density <- rbind.data.frame(
  irfpai1922 %>% slice_sample(n = 10000) %>% select(
    source,
    SCRSSN,
    THRPY_SPCH_IND_MINS,
    THRPY_OT_IND_MINS,
    THRPY_PT_IND_MINS,
    THRPY_SPCH_CNC_MINS,
    THRPY_OT_CNC_MINS,
    THRPY_PT_CNC_MINS,
    THRPY_SPCH_GRP_MINS,
    THRPY_OT_GRP_MINS,
    THRPY_PT_GRP_MINS,
    THRPY_SPCH_CO_MINS,
    THRPY_OT_CO_MINS,
    THRPY_PT_CO_MINS
  ),
  mds_selected %>% slice_sample(n = 10000) %>% select(
    source,
    SCRSSN,
    THRPY_SPCH_IND_MINS,
    THRPY_OT_IND_MINS,
    THRPY_PT_IND_MINS,
    THRPY_SPCH_CNC_MINS,
    THRPY_OT_CNC_MINS,
    THRPY_PT_CNC_MINS,
    THRPY_SPCH_GRP_MINS,
    THRPY_OT_GRP_MINS,
    THRPY_PT_GRP_MINS,
    THRPY_SPCH_CO_MINS,
    THRPY_OT_CO_MINS,
    THRPY_PT_CO_MINS
  )
)


chk_density <- chk_density %>% pivot_longer(cols = contains("MINS"),
                                            names_to = "type",
                                            values_to = "mins") %>% filter(mins < 100)

chk_density <- chk_density %>%
  separate(type,
           into = c("thrpy", "type", "setting", "null"),
           sep = "_") %>%
  select(source, SCRSSN, type, setting, mins)

ggplot(data = chk_density %>% filter(setting == "IND"), aes(x = mins, fill = type)) + geom_density() + facet_grid(cols = vars(source)) + ggtitle(label =  "INDIVIDUAL")

ggplot(data = chk_density %>% filter(setting == "IND"), aes(x = mins, fill = type)) + geom_histogram(bins = 101) + facet_grid(cols = vars(source)) + ggtitle(label =  "INDIVIDUAL")

ggplot(data = chk_density %>% filter(setting == "CNC", mins <= 45),
       aes(x = mins , fill = type)) + geom_histogram(bins = 46) + facet_grid(cols = vars(source)) + ggtitle(label =  "CONCURRENT")
ggplot(data = chk_density %>% filter(setting == "CNC", mins > 0, mins <= 45),
       aes(x = mins , fill = type)) + geom_histogram(bins = 45) + facet_grid(cols = vars(source)) +
  ggtitle(label =  "CONCURRENT")

ggplot(data = chk_density %>% filter(setting == "GRP", mins <= 45),
       aes(x = mins , fill = type)) + geom_histogram(bins = 46) + facet_grid(cols = vars(source)) + ggtitle(label =  "GROUP")
ggplot(data = chk_density %>% filter(setting == "GRP", mins > 0, mins <= 45),
       aes(x = mins , fill = type)) + geom_histogram(bins = 45) + facet_grid(cols = vars(source)) +
  ggtitle(label = "GROUP")

ggplot(data = chk_density %>% filter(setting == "CO", mins <= 45),
       aes(x = mins , fill = type)) + geom_histogram(bins = 46) + facet_grid(cols = vars(source)) + ggtitle(label = "COTREATMENT")
ggplot(data = chk_density %>% filter(setting == "CO", mins > 0, mins <= 45),
       aes(x = mins , fill = type)) + geom_histogram(bins = 45) + facet_grid(cols = vars(source)) + ggtitle(label =  "COTREATMENT")
ggplot(data = chk_density %>% filter(setting == "CO", mins > 0, mins <= 90),
       aes(x = mins , fill = type)) + geom_histogram(bins = 90) + facet_grid(cols = vars(source)) + ggtitle(label = "COTREATMENT")
