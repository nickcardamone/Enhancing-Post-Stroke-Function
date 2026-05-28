# Stroke Rehabilitation Research Project
## ORD Waddell 202408036D - Clinical Data Analytics Study

**Principal Investigator:** Dr. Waddell  
**Data Analyst:** Nick Cardamone  
**Institution:** Department of Veterans Affairs

---

## Project Overview

This project analyzes functional outcomes in Veterans who experienced stroke and received post-acute rehabilitation care (Inpatient Rehabilitation Facility, Skilled Nursing Facility, or Home Health) between 2019-2022. The analysis uses VA Clinical Data Warehouse (CDW) and Medicare claims data (CMS) to identify stroke hospitalizations and link them to post-acute care episodes with functional assessment scores.

### Key Research Questions
1. What are the functional outcomes (GG scores) at discharge from post-acute rehabilitation?
2. How do outcomes vary by rehabilitation setting (IRF vs SNF vs HH)?
3. What patient and clinical factors predict better functional outcomes?

---

## Data Sources

### Primary Data Sources
- **VA CDW OMOP**: Inpatient visits, diagnoses, procedures
- **CMS OMOP**: Medicare claims for non-VA care
- **RHF (Residential History File)**: Post-acute care episodes
- **MDS (Minimum Data Set)**: SNF functional assessments
- **OASIS**: Home health functional assessments
- **IRF-PAI**: Inpatient rehabilitation facility assessments
- **MBSF**: Medicare Master Beneficiary Summary Files (race/ethnicity supplementation)
- **PSSG / RPCMM**: VA primary care team (PACT) assignment data

### Assessment Instruments
- **GG Codes**: Section GG functional items (22 items, scored 1-6)
  - Self-care: 7 items — GG0130A–H (eating, oral hygiene, toileting, showering, upper/lower body dressing, etc.)
  - Mobility: 15 items — GG0170A–P (rolling, sit-to-stand, bed/chair/toilet transfers, walking, stairs, wheelchair use)
  - Valid scores: 01 (dependent) through 06 (independent); codes 07, 09, 10, 88 treated as missing

---

## Script Execution Order

### Prerequisites
1. Install required R packages (see `00_config.R`)
2. Ensure access to VINCI database (`VHACDWRB03_17`)
3. Create project directory structure (auto-created by `00_config.R`)

### Execution Sequence

```
00_config.R                                    # Source first - loads all configurations
    ↓
01_primary_dx_stroke_create_cohort_v8.R        # Creates stroke hospitalization cohort (CDW + CMS)
    ↓
01b_multiple_dx_stroke_create_cohort_v7.R      # Finds prior strokes for final cohort
    ↓
02a_process_rhf_join_stroke_cohort_v11.R       # Links post-acute care episodes to strokes
    ↓
02b_process_rhf_prior_service_use_v2.R         # Extracts prior healthcare utilization
    ↓
03a_characteristics_demographics_v5.R          # Pulls patient demographics & enrollment
    ↓
03b_characteristics_comorbidity_v3.R           # Calculates CFI, Charlson, Stroke Severity Index
    ↓
03c_characteristics_va_primary_care.v1.R       # Extracts PACT assignment prior to hospitalization
    ↓
04a_clean_v4.R                                 # Joins demographics, applies exclusion criteria
    ↓
05a_GG_code_create_v11.R                       # Extracts and processes GG scores (MDS/OASIS/IRF-PAI)
    ↓
05b_GG_code_join_to_RHF_v11.R                  # Matches GG assessments to post-acute episodes
    ↓
05c_complete_race_ethnicity_v1.R               # Supplements missing race/ethnicity from MBSF
    ↓
06_consort_exclusions_v1.R                     # Generates CONSORT diagram exclusion counts
    ↓
06a_analytical_dataset_v4.R                    # Creates analysis-ready dataset; imputation scaffolding
    ↓
06b_tables_v4.R                                # Generates descriptive statistics tables (Excel)
    ↓
06c_missingness_tables_v1.R                    # Creates GG item missingness tables and figures
```

> **Note:** `06d_therapy_intensity_v1.R` (therapy minutes/days) was drafted but **not implemented** — those variables were excluded from the final analysis.

---

## Key Datasets Created

### Cohort Development

| File | Description | Key Variables | Approx. Rows |
|------|-------------|---------------|--------------|
| `pdx_stroke_complete.parquet` | All stroke hospitalizations (raw, pre-rollup) | PatientICN, ADMSNDT, ICD10, SOURCE | ~1M |
| `pdx_stroke_visit_summary.parquet` | Rolled-up stroke episodes | PatientICN, SCRSSN, ADMSNDT, DSCHRGDT, CDW_pdx, CMS_pdx | ~192K |
| `rhf_no_small_gap.parquet` | RHF with small gaps removed | scrssn, hee_from, hee_thru, Grouping | — |
| `rhf_no_gap.parquet` | RHF fully cleaned | scrssn, hee_from, hee_thru, Grouping | ~3.5M (raw) |
| `rhf_cohort_included.parquet` | Stroke admissions matched to RHF inpatient episodes | scrssn, ADMSNDT, Grouping | ~74K hosp. |
| `rhf_post_acute_final.parquet` | All eligible post-acute episodes | Episode-level variables | ~98K |
| `dt_post_acute_elig.parquet` | Eligible IRF/SNF/HH episodes (pre-GG matching) | scrssn, ADMSNDT, Grouping, episode_run | ~95K |
| `dt_first_post_acute_elig.parquet` | First post-acute episode per stroke hospitalization | scrssn, ADMSNDT, Grouping, setting_sequence | ~74K |

### Characteristics

| File | Description | Key Variables |
|------|-------------|---------------|
| `cohort_prevservice_use.parquet` | Prior year healthcare utilization | p1_count_pc, p1_any_pc, p1_count_inpat, p1_los_inpat, p1_any_inpat |
| `demo_cohort.parquet` | Patient demographics and enrollment | Age_ADMSNDT, Gender, Race, Ethnicity, MaritalStatus, Rural, ADI_NATRANK, dual, FFS_only, MA, PriorityGroupName |
| `cohort_comorb_frailty.parquet` | Comorbidity scores | p1_frailty_index, p2_charlson_cindex, acu_stroke_severity, aphasia, hemiplegia, etc. |
| `cohort_pact_assignment.parquet` | PACT primary care assignment prior to hospitalization | PatientICN, ADMSNDT, PACT team variables |

### Functional Outcomes

| File | Description | Key Variables |
|------|-------------|---------------|
| `gg_complete_base.parquet` | Raw GG codes from all sources | scrssn, gg_code, time, value, source |
| `gg_complete_base2.parquet` | Processed GG codes with scoring applied | — |
| `gg_meta.parquet` | Assessment metadata | record_id, assessment_date, source, Grouping |
| `gg_complete_scale.parquet` | Scale-level scores (self-care, mobility, total) | total_gg_admission, total_gg_discharge, selfcare_*, mobility_* |
| `gg_complete_item.parquet` | Item-level scores with labels | gg_item, gg_val, time, Grouping |

### Final Analytical Datasets

| File | Description | Approx. Rows |
|------|-------------|--------------|
| `elig_rhf_data_assessment_2.parquet` | All episode–assessment matches | ~98K |
| `elig_rhf_data_assessment_final.parquet` | Best admission/discharge per episode | — |
| `elig_rhf_data_assessment_final_epis_excl.parquet` | After hospitalization-level exclusions | — |
| `elig_rhf_data_assessment_final_epis_excl1.parquet` | After episode-level exclusion 1 (no assessment/VA-only) | — |
| `elig_rhf_data_assessment_final_epis_excl2.parquet` | After episode-level exclusion 2 (no valid GG scores) | ~43K |
| `analysis_data.parquet` | Final analysis-ready dataset (one row per episode) | — |

---

## Data Processing Rules

### Stroke Cohort Inclusion
- **Primary diagnosis** of stroke (ICD-10: G46, I60-I63, I66, I67.89, I97.81, I97.82)
- Inpatient admission date: 2019-01-01 to 2022-12-31
- Discharge date before 2023-01-01
- At least one primary diagnosis code in **first position** or **primary position**

### Episode Rollup Logic
Consecutive inpatient stays are combined into a single hospitalization if:
- Transfer: Admission date = prior discharge date (same day)
- Continuous care: Admission date immediately follows prior discharge with no gap

### Post-Acute Care Matching
Episode linked to stroke hospitalization if:
- **IRF or SNF**: Admission within **3 days** of acute discharge
- **Home Health**: Admission within **14 days** of acute discharge
- Matching on scrambled SSN (SCRSSN)

### Episode Length Requirements
- Minimum **4 days** length of stay
- Episodes <4 days are consolidated into adjacent episodes

### Gap Removal
- Gaps ≤3 days between same setting type are removed
- "OTHER" category stays ≤3 days are removed
- Re-group consecutive same-setting episodes after gap removal

### CONSORT Exclusion Steps (Script 06)
1. All Veterans hospitalized with a primary stroke diagnosis 2019–2022
2. Limit to Veterans found in the RHF
3. Limit to hospitalizations with a matching RHF inpatient episode (±1 day) followed by IRF/SNF (≤3 days) or HH (≤14 days) after discharge
4. Exclude if: readmitted from post-acute, died during/within 1 day of post-acute, unplanned discharge, all episodes ≤3 days, age <65 or >99, acute stay >365 days
5. Exclude if: no matched GG assessment, VA SNF only, VA purchased HH only, VA IRF only (no assessment data)
6. Exclude if: no post-acute episode has at least one GG item with a valid score (01–06)

### GG Score Matching Rules

**Admission Assessment (if multiple candidates):**
1. Within 2 weeks of episode start
2. Has non-missing GG scores
3. Most non-missing items
4. Most valid scores (01–06)
5. Closest to admission date

**Discharge Assessment (if multiple candidates):**
1. Within 7 days before discharge (preferred)
2. If none, within 2 weeks after discharge
3. Has non-missing GG scores
4. Most non-missing items
5. Most valid scores (01–06)
6. Closest to discharge date

### Imputation Strategy (Script 06a)
- **Primary**: Setting-aware predictive mean matching (PMM) using 2-level `mice` (`miceadds`)
- **CMS sensitivity**: Simple replacement of error codes with `01`
- **Hybrid sensitivity**: Deterministic replacement for MNAR codes (e.g., 88), then PMM

---

## Variable Naming Conventions

### Prefixes
- `p1_` = Prior 1 year (e.g., `p1_count_inpat`, `p1_frailty_index`)
- `p2_` = Prior 2 years (e.g., `p2_charlson_cindex`)
- `acu_` = Acute hospitalization period (e.g., `acu_stroke_severity`)
- `gg_` = GG functional score item (e.g., `gg_item`, `gg_val`)

### Common Variables
- `PatientICN` = Patient Integration Control Number (CDW)
- `PERSON_ID` = OMOP person identifier
- `SCRSSN` = Scrambled SSN (linked across datasets)
- `ADMSNDT` = Admission date (stroke hospitalization)
- `DSCHRGDT` = Discharge date (stroke hospitalization)
- `hee_from` = Episode start date (RHF)
- `hee_thru` = Episode end date (RHF)
- `episode_run` = Episode sequence number per hospitalization
- `StrokeType` = Ischemic / Hemorrhagic / Other (derived from ICD-10)

### Setting Codes
- `IRF` = Inpatient Rehabilitation Facility
- `SNF` = Skilled Nursing Facility
- `HH` = Home Health
- `INP` = Acute Inpatient
- `GAP` = Gap in care (no claims)
- `OTHER` = Other healthcare utilization
- `HOSPICE` = Hospice care
- `LTCH` = Long-term care hospital

---

## Quality Checks

### Expected Row Counts (Approximate)
1. **Script 01**: ~1M raw rows → ~192K rolled-up stroke hospitalizations (~174K unique patients)
2. **Script 02a**: ~3.5M raw RHF records → ~74K stroke hospitalizations with post-acute care → ~98K post-acute episodes
3. **Script 04a**: ~95K eligible IRF/SNF/HH episodes
4. **Script 05b**: ~98K episode–assessment matches → ~64K after all exclusions
5. **Script 06a**: ~43K rows in `elig_rhf_data_assessment_final_epis_excl2.parquet`

### Data Validation
Each script includes:
- Row count logging (`log_count()` function)
- Missing data summaries
- Duplicate checks
- Date range validation
- Cross-tabulations of key variables

---

## Output Files

### Analysis Tables (in `derived_data/analysis/`)
- `Stroke_Rehabilitation_Tables.xlsx` — Excel workbook with tabs:
  - Tab 1: By Year Summary (cohort counts and discharge patterns)
  - Tab 2: CONSORT Detail (stepwise exclusions)
  - Tab 3: Overall Characteristics (demographics, comorbidities)
  - Tab 4: Data Quality Checks (quantiles for key measures)
  - Tab 5: Length of Stay by VA/Non-VA Setting
  - Tab 6: Rehabilitation Setting Utilization
  - Tab 7: Stratified Analysis by Acute Care Setting
  - Tab 8a: Demographic Summary (table1)
  - Tab 8b: Acute Care & LOS Summary (table1)
  - Tab 8c: Functional Scores Summary (table1)

### Figures (in `figures/`)
- GG item missingness bar charts by setting (HH, IRF, SNF) from `06c`
- Baseline and discharge GG score trends by year and setting

---

## Known Issues & Limitations

1. **Missing GG Scores**: Not all post-acute episodes have matched assessments
   - MDS: Best coverage for SNF
   - OASIS: Home health may have delayed assessments
   - IRF-PAI: Smallest sample size
   - VA-only facilities (SNF, HH, IRF) excluded if no external assessment data

2. **Data Sources**:
   - CDW and CMS data have different coding formats (CDW uses decimal ICD-10; CMS does not)
   - Some patients have care in both VA and non-VA settings

3. **Race/Ethnicity**:
   - Missing values supplemented from MBSF files (`05c_complete_race_ethnicity_v1.R`)
   - Residual missingness addressed in imputation step

4. **Episode Matching**:
   - RHF dates may not exactly match assessment dates
   - Complex transfer patterns require clinical judgment

5. **Therapy Intensity**:
   - `06d_therapy_intensity_v1.R` was drafted but not implemented; therapy minutes/days variables were excluded from the final analysis

---

## Contact & Support

**Project Team:**
- Nick Cardamone (Data Analyst): [email]
- Dr. Waddell (PI): [email]

**Technical Support:**
- VINCI Support: vinci.vaco@va.gov
- ORD Research Analytics: [contact]

---

## Version History

- **v8.0** (2026-05-27): Updated README to reflect current script pipeline; added PACT, race/ethnicity supplementation, CONSORT, imputation scripts; corrected output file names and row counts
- **v7.0** (2026-01-16): Added prior stroke lookup (01b), PACT assignment (03c), race/ethnicity completion (05c), CONSORT diagram (06), imputation scaffolding (06a); updated analytical dataset structure
- **v6.0** (2025-12-11): Finalized cohort with OMOP-derived hospitalizations; added Stroke Severity Index; standardized GG matching rules
- **v5.0** (2025-08-20): Added prior service use variables
- **v4.0** (2025-06-10): Linked functional assessments
- **v3.0** (2025-05-12): Added demographics and comorbidity
- **v2.0** (2025-04-07): Created GG score dataset
- **v1.0** (2025-01-15): Initial stroke cohort

---

## References

### Clinical Classifications
- ICD-10-CM codes for stroke: CDC/NCHS classification
- GG Functional Items: CMS Section GG specifications

### Data Sources
- OMOP Common Data Model v5
- VA CDW: Corporate Data Warehouse
- CMS: Centers for Medicare & Medicaid Services

---

*Last Updated: May 27, 2026*
