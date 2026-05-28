################################################################################
# PROJECT CONFIGURATION FILE
# Stroke Rehabilitation Research - CDA Study
# ORD Waddell 202408036D
################################################################################
#
# Purpose: Central configuration for all analysis scripts
# Author: Nick Cardamone
# Created: 2025-12-11
# Last Modified: 2025-12-11
#
# Description:
#   This file contains all configurable parameters, file paths, database
#   connections, and constants used across the project. Source this file
#   at the beginning of each analysis script to ensure consistency.
#
# Usage:
#   source("00_config.R")
#
################################################################################

# ==============================================================================
# ENVIRONMENT INFORMATION
# ==============================================================================

# R version requirement
required_r_version <- "4.0.0"
if (getRversion() < required_r_version) {
  stop(sprintf("This project requires R version %s or higher. Current version: %s", 
               required_r_version, getRversion()))
}

# Record session info for reproducibility
cat("\n=== R Session Information ===\n")
cat("R version:", R.version.string, "\n")
cat("Platform:", R.version$platform, "\n")
cat("Date:", Sys.Date(), "\n\n")

# ==============================================================================
# PROJECT DIRECTORIES
# ==============================================================================

# Base project directory
# Note: Modify this path if project is moved to different location
project_base <- "P:/ORD_Waddell_202408036D/nick/R/Stroke_Rehabilitation"

# Subdirectories
dir_parquet <- file.path(project_base, "parquet")
dir_output <- file.path(project_base, "output")
dir_tables <- file.path(project_base, "tables")
dir_figures <- file.path(project_base, "figures")
dir_data <- file.path(project_base, "data")

# Create directories if they don't exist
for (dir in c(dir_parquet, dir_output, dir_tables, dir_figures, dir_data)) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat("Created directory:", dir, "\n")
  }
}

# ==============================================================================
# DATABASE CONNECTION PARAMETERS
# ==============================================================================

# Database servers and names
db_server <- "VHACDWRB03_17"
db_project <- "ORD_Waddell_202408036D"
db_cdwwork <- "CDWWork"

# Schema names
schema_dflt <- "Dflt"
schema_src <- "Src"
schema_dim <- "Dim"
schema_ndim <- "NDim"

# ==============================================================================
# STUDY PARAMETERS
# ==============================================================================

# Study period
study_start_date <- as.Date("2019-01-01")
study_end_date <- as.Date("2023-01-01")
study_years <- 2019:2022

# ICD-10 stroke diagnosis codes
# Source: Clinical classification based on stroke diagnosis criteria
icd10_stroke_codes <- c("G46", "I60", "I61", "I62", "I63", "I66", 
                        "I67.89", "I97.81", "I97.82")

# Regular expressions for pattern matching
# CDW format: ICD10|XXX.XX
pattern_stroke_cdw <- "\\b(G46|I60|I61|I62|I63|I66|I67\\.89|I97\\.81|I97\\.82)(\\.\\d+)*\\b"
# CMS format: ICD10-XXXXX (no decimal)
pattern_stroke_cms <- "^(G46|I60|I61|I62|I63|I66|I6789|I9781|I9782)\\d*"

# OMOP visit concept IDs
# 9201: Inpatient Visit
# 800000001: Inpatient Observation Visit
# 581385: Emergency Room and Inpatient Visit
# 80000002: Outpatient Visit Within Inpatient Visit
# 262: Emergency Room and Inpatient Visit
# 9203: Emergency Room Visit
visit_concept_ids <- c(9201, 800000001, 581385, 80000002, 262, 9203)

# Post-acute care settings
pac_settings <- c("IRF", "SNF", "HH")

# ==============================================================================
# POST-ACUTE CARE DISCHARGE TIMING RULES
# ==============================================================================

# Maximum days between acute discharge and post-acute admission
max_days_to_irf <- 3
max_days_to_snf <- 3
max_days_to_hh <- 14

# Minimum episode length of stay (days)
min_episode_los <- 4

# Gap tolerance for episode rollup (days)
max_gap_days <- 3

# ==============================================================================
# GG CODE DEFINITIONS
# ==============================================================================

# Self-care GG codes (7 items, 7-42 points)
gg_selfcare_codes <- c("GG0130A", "GG0130B", "GG0130C", "GG0130E", 
                       "GG0130F", "GG0130G", "GG0130H")

# Mobility GG codes (15 items, 15-90 points)
gg_mobility_codes <- c("GG0170A", "GG0170B", "GG0170C", "GG0170D", 
                       "GG0170E", "GG0170F", "GG0170G", "GG0170H", 
                       "GG0170I", "GG0170J", "GG0170K", "GG0170L", 
                       "GG0170M", "GG0170N", "GG0170P")

# All GG codes (22 items, 22-132 points)
gg_all_codes <- c(gg_selfcare_codes, gg_mobility_codes)

# GG score valid range
gg_min_score <- 1
gg_max_score <- 6
gg_missing_codes <- c(7, 9, 10, 88)

# ==============================================================================
# PRIMARY CARE STOP CODES
# ==============================================================================

# Primary care primary stop codes
pc_primary_stopcodes <- c(322, 323, 348, 350)

# Primary care allowed secondary stop codes
pc_secondary_stopcodes <- c(0, 117, 179, 185, 186, 187, 188, 318, 322, 323, 
                            348, 350, 531, 533, 690, 692, 693, 999)

# Special rule: Primary 350 + Secondary 117 is valid
pc_special_combinations <- list(
  list(primary = 350, secondary = 117)
)

# ==============================================================================
# REQUIRED R PACKAGES
# ==============================================================================

required_packages <- c(
  "DBI",           # Database interface
  "dbplyr",        # Database backend for dplyr
  "dplyr",         # Data manipulation
  "tidyverse",     # Suite of tidy packages
  "data.table",    # Fast data manipulation
  "arrow",         # Parquet file handling
  "stringr",       # String manipulation
  "lubridate",     # Date handling
  "VINCI",         # VA database connections
  "table1",        # Table generation
  "comorbidity",   # Comorbidity indices
  "gt",            # Table formatting
  "ggplot2",       # Visualization
  "MASS",          # Statistical functions
  "broom",         # Tidy model output
  "car",           # Regression diagnostics
  "tidyr",         # Data cleaning functions
  "table1"         # Tabling automatically
)

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

# Custom 'not in' operator
'%!in%' <- function(x,y)!('%in%'(x,y))

# Function to check and install required packages
check_packages <- function(packages) {
  missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]
  if (length(missing_packages) > 0) {
    cat("\nMissing packages:", paste(missing_packages, collapse = ", "), "\n")
    cat("Install them with: install.packages(c(", 
        paste0("'", missing_packages, "'", collapse = ", "), "))\n")
    stop("Required packages are missing.")
  }
  invisible(TRUE)
}

# Function to load all required packages
load_packages <- function(packages) {
  for (pkg in packages) {
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE, warn.conflicts = FALSE)
    )
  }
  cat("Loaded", length(packages), "packages successfully.\n")
}

# Function to establish database connection with error handling
connect_db <- function(database, server = db_server) {
  tryCatch({
    con <-  dbConnect(odbc::odbc(), dsn = db_server, timeout = 10, 
                      database = database)
    cat("Connected to database:", database, "on server:", server, "\n")
    return(con)
  }, error = function(e) {
    stop(sprintf("Failed to connect to database '%s' on server '%s': %s", 
                 database, server, e$message))
  })
}

# Function to validate date ranges
validate_dates <- function(start_date, end_date, data_date_col) {
  if (any(data_date_col < start_date | data_date_col >= end_date, na.rm = TRUE)) {
    warning("Some dates fall outside the expected study period")
  }
}

# Function to log data counts
log_count <- function(data, description) {
  n_rows <- nrow(data)
  n_unique_patients <- data %>% 
    select(matches("PatientICN|PERSON_ID|scrssn|SCRSSN")) %>% 
    distinct() %>% 
    nrow()
  
  cat(sprintf("\n[DATA COUNT] %s\n", description))
  cat(sprintf("  Rows: %s\n", format(n_rows, big.mark = ",")))
  if (n_unique_patients > 0) {
    cat(sprintf("  Unique patients: %s\n", format(n_unique_patients, big.mark = ",")))
  }
}

# Function to validate required columns
validate_columns <- function(data, required_cols, dataset_name = "dataset") {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns in %s: %s", 
                 dataset_name, paste(missing_cols, collapse = ", ")))
  }
  invisible(TRUE)
}

# Function to save parquet with logging
save_parquet_safe <- function(data, filename, dir = dir_parquet) {
  filepath <- file.path(dir, filename)
  tryCatch({
    arrow::write_parquet(data, filepath)
    cat(sprintf("Saved: %s (%s rows)\n", filename, format(nrow(data), big.mark = ",")))
  }, error = function(e) {
    stop(sprintf("Failed to save %s: %s", filename, e$message))
  })
}

# Function to load parquet with logging
load_parquet_safe <- function(filename, dir = dir_parquet) {
  filepath <- file.path(dir, filename)
  if (!file.exists(filepath)) {
    stop(sprintf("File not found: %s", filepath))
  }
  tryCatch({
    data <- arrow::open_dataset(filepath) %>% collect()
    cat(sprintf("Loaded: %s (%s rows)\n", filename, format(nrow(data), big.mark = ",")))
    return(data)
  }, error = function(e) {
    stop(sprintf("Failed to load %s: %s", filename, e$message))
  })
}

# ==============================================================================
# DATA DICTIONARIES
# ==============================================================================

# Key variable definitions for documentation
var_definitions <- list(
  PatientICN = "Patient Integration Control Number - unique patient identifier",
  PERSON_ID = "OMOP person identifier",
  SCRSSN = "Scrambled Social Security Number",
  VISIT_OCCURRENCE_ID = "OMOP visit occurrence identifier",
  VISIT_START_DATE = "Visit start date",
  VISIT_END_DATE = "Visit end date",
  ADMSNDT = "Admission date (acute inpatient)",
  DSCHRGDT = "Discharge date (acute inpatient)",
  hee_from = "Episode start date (RHF)",
  hee_thru = "Episode end date (RHF)",
  pdx_stroke = "Primary diagnosis of stroke indicator (1=Yes, 0=No)",
  Grouping = "Post-acute care setting (IRF/SNF/HH/INP/GAP/OTHER/HOSPICE/LTCH)",
  total_gg_admission = "Total GG score at admission (22-132)",
  total_gg_discharge = "Total GG score at discharge (22-132)"
)

# ==============================================================================
# PRINT CONFIGURATION SUMMARY
# ==============================================================================

cat("\n=============================================================================\n")
cat("STROKE REHABILITATION PROJECT - CONFIGURATION LOADED\n")
cat("=============================================================================\n")
cat("Project Base:", project_base, "\n")
cat("Study Period:", study_start_date, "to", study_end_date, "\n")
cat("Database Server:", db_server, "\n")
cat("Project Database:", db_project, "\n")
cat("=============================================================================\n\n")

# ==============================================================================
# END OF CONFIGURATION FILE
# ==============================================================================
