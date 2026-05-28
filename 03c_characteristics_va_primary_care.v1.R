################################################################################
# SCRIPT 02C: EXTRACT PACT ASSIGNMENT PRIOR TO HOSPITALIZATION
# Stroke Rehabilitation Research - CDA Study
# ORD Waddell 202408036D
################################################################################
#
# Purpose:
#   Identify the primary care (PACT) assignment prior to stroke hospitalization.
#   Pulls the most recent primary care provider assignment at/before admission.
#
# Author: Nick Cardamone
# Created: 2026-01-16
# Version: 1.0
#
# Inputs:
#   - rhf_post_acute_final.parquet (from Script 2a)
#   - pdx_stroke_visit_summary.parquet (from Script 1)
#   - Src.RPCMM_CurrentPatientTeamMembership
#   - CDWWork.NDIM.RPCMMTeam
#   - Src.RPCMM_CurrentProviderTeamMembership
#   - Src.SStaff_SStaff
#   - CDWWork.NDIM.RPCMMTeamRole
#   - CDWWork.NDIM.RPCMMTeamCareType
#   - CDWWork.Dim.Institution
#
# Outputs:
#   - cohort_pact_assignment.parquet: PACT assignment prior to hospitalization
#
################################################################################

# ==============================================================================
# SETUP
# ==============================================================================

# Source configuration
source("00_config.R")

# Load required packages
load_packages(c("dplyr", "dbplyr", "DBI", "VINCI", "arrow", "data.table",
                "table1", "lubridate", "stringr"))

cat("\n=============================================================================\n")
cat("SCRIPT 02C: EXTRACT PACT ASSIGNMENT PRIOR TO HOSPITALIZATION\n")
cat("=============================================================================\n\n")

# Set working directory
setwd(project_base)

# ==============================================================================
# LOAD INPUT DATASETS
# ==============================================================================

cat("Loading input datasets...\n")

# Load cohort data
cohort_full <- load_parquet_safe("rhf_post_acute_final.parquet")
pdx_stroke_visit_summary <- load_parquet_safe("pdx_stroke_visit_summary.parquet")

# Extract stroke admission dates and discharge source
pdx_stroke_visit_summary <- pdx_stroke_visit_summary %>%
  transmute(
    scrssn = SCRSSN,
    ADMSNDT = ACUTE_INPATIENT_VISIT_START,
    discharge_SOURCE
  ) %>%
  distinct()

# Create cohort with lookback periods
cat("Creating lookback date windows...\n")
cohort <- cohort_full %>%
  select(PatientICN, scrssn, PERSON_ID, ADMSNDT, DSCHRGDT, hee_from, hee_thru) %>%
  group_by(PatientICN, scrssn, PERSON_ID, ADMSNDT) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    # Use earliest date (RHF or admission) as hospitalization start
    hosp_start = if_else(hee_from < ADMSNDT, hee_from, ADMSNDT),
    # Calculate lookback periods
    year_prior_to_admission = hosp_start %m-% years(1),
    twoyears_prior_to_admission = hosp_start %m-% years(2)
  ) %>%
  left_join(pdx_stroke_visit_summary, by = c("scrssn", "ADMSNDT"))

log_count(cohort, "Cohort with lookback periods")

# ==============================================================================
# DATABASE CONNECTION
# ==============================================================================

cat("\nEstablishing database connections...\n")
con <- connect_db(database = db_project, server = db_server)
cdwwork <- connect_db(database = db_cdwwork, server = db_server)

# ==============================================================================
# CREATE PATIENT CROSSWALK (ICN TO SID)
# ==============================================================================

cat("Creating PatientICN to PatientSID crosswalk...\n")
xw_icn_sid <- tbl(con, in_schema(schema_src, "SPatient_Spatient")) %>%
  inner_join(cohort, by = "PatientICN", copy = TRUE) %>%
  select(PatientICN, ADMSNDT, hosp_start, PatientSID) %>%
  distinct()

# ==============================================================================
# EXTRACT PACT ASSIGNMENT PRIOR TO HOSPITALIZATION
# ==============================================================================

cat("\n--- Extracting PACT assignment prior to hospitalization ---\n")

rpcmm <- tbl(con, in_schema(schema_src, "RPCMM_CurrentPatientTeamMembership")) %>% transmute(Sta3n, rpcmm_StartDateTime = StartDateTime, rpcmm_EndDateTime = EndDateTime, PatientSID, RPCMMTeamSID)
cptm <- tbl(con, in_schema(schema_src, "RPCMM_CurrentProviderTeamMembership")) %>% transmute(RPCMMTeamSID, ProviderSID, RPCMMTeamRoleSID, cptm_EndDateTime = EndDateTime)

rt <- tbl(cdwwork, in_schema("NDim", "RPCMMTeam")) %>% 
  select(RPCMMTeamSID, RPCMMTeamCareTypeSID) %>% distinct() %>% collect()

#staff <- tbl(cdwwork, in_schema("SStaff", "SStaff"))
tr <- tbl(cdwwork, in_schema("NDim",  "RPCMMTeamRole")) %>% select(RPCMMTeamRoleSID, PrimaryCarePositionIndicator) %>% distinct() %>% collect() 
tct <- tbl(cdwwork, in_schema("NDim", "RPCMMTeamCareType")) %>% select(RPCMMTeamCareTypeSID, RPCMMTeamCareTypeCode) %>% distinct() %>% collect()
#inst <- tbl(cdwwork, in_schema(schema_dim, "Institution"))

pact_prior <- xw_icn_sid %>%
  inner_join(rpcmm, by = "PatientSID") %>% 
  inner_join(rt, by = "RPCMMTeamSID", copy = T) %>% # saved as df so need to copy 
  inner_join(cptm, by = c("RPCMMTeamSID" = "RPCMMTeamSID")) %>%
  #inner_join(staff, by = c("ProviderSID" = "StaffSID")) %>%
  inner_join(tr, by = c("RPCMMTeamRoleSID" = "RPCMMTeamRoleSID"), copy = T) %>% # saved as df so need to copy 
  inner_join(tct, by = c("RPCMMTeamCareTypeSID" = "RPCMMTeamCareTypeSID"), copy = T) # %>% saved as df so need to copy 
  #inner_join(inst, by = c("InstitutionSID" = "InstitutionSID"))
  
pact_filter <- pact_prior %>% 
  filter(
    rpcmm_StartDateTime <= hosp_start,
    is.na(rpcmm_EndDateTime) | rpcmm_EndDateTime >= hosp_start,
    RPCMMTeamCareTypeCode %in% c("7", "13"),
    PrimaryCarePositionIndicator == "Y",
    is.na(cptm_EndDateTime)
  ) %>%
  transmute(
    PatientICN,
    ADMSNDT,
   # site = Sta3n,
   # PCP_LastName = staff$LastName,
   # PCP_FirstName = staff$FirstName,
   # PCP_PositionTitle = staff$PositionTitle,
   # PCP_SignatureBlockTitle = staff$SignatureBlockTitle,
    PCP_RelationshipStartDateTime = rpcmm_StartDateTime,
    PCP_RelationshipStartDate = as.Date(rpcmm_StartDateTime),
   
   # PCP_FacilityCode = inst$InstitutionCode,
   # PCP_Facility = inst$OfficialVAName
  ) %>% distinct() %>% collect() 

pact_filter <- pact_filter %>%
  group_by(PatientICN, ADMSNDT) %>%
  mutate(
    DRank = dense_rank(desc(PCP_RelationshipStartDateTime))
  ) %>%
  filter(DRank == 1) %>%
  select(-DRank, -PCP_RelationshipStartDateTime) %>%
  distinct() 

log_count(pact_filter, "PACT assignment prior to hospitalization")

cohort_with_pact <- cohort %>% 
  left_join(pact_filter, by = c("PatientICN", "ADMSNDT"))

cohort_with_pact <- cohort_with_pact %>% 
  mutate(va_primary_care_ever = if_else(!is.na(PCP_RelationshipStartDate), 1, 0),
         va_primary_care_p2 = if_else(!is.na(PCP_RelationshipStartDate) & PCP_RelationshipStartDate >= twoyears_prior_to_admission, 1, 0),
         va_primary_care_p1 = if_else(!is.na(PCP_RelationshipStartDate) & PCP_RelationshipStartDate >= year_prior_to_admission, 1, 0))

cohort_with_pact <- cohort_with_pact %>% filter(ADMSNDT > PCP_RelationshipStartDate | is.na(PCP_RelationshipStartDate))

cohort_with_pact <- cohort_with_pact %>% select(PatientICN, ADMSNDT, va_primary_care_p1, va_primary_care_p2, va_primary_care_ever)
  
# ==============================================================================
# SAVE OUTPUT
# ==============================================================================

cat("\n--- Saving Output ---\n")
save_parquet_safe(cohort_with_pact, "cohort_with_pact.parquet")

# ==============================================================================
# SESSION INFO
# ==============================================================================

cat("\n=============================================================================\n")
cat("SCRIPT 02C COMPLETED SUCCESSFULLY\n")
cat("=============================================================================\n")
cat("Output: cohort_pact_assignment.parquet\n")
cat("Rows:", format(nrow(pact_prior), big.mark = ","), "\n")
cat("=============================================================================\n\n")

# Clean up
dbDisconnect(con)
dbDisconnect(cdwwork)

################################################################################
# END OF SCRIPT
################################################################################
