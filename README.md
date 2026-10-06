# Climate resilience coverage of Fishery Improvement Project performance indicators

Data and code for:

> Zhao, L. Z., Eurich, J. G., Wapman, E. & Finkbeiner, E. The instrumental value of social responsibility for a climate-resilient seafood sector (to be submitted to *Marine Policy*).

We scored the 49 performance indicators (PIs) in two FIP assessments, the Environmental Rapid Assessment (ERA, version 2.1, 25 PIs) and the Social Responsibility Assessment (SRA, 2021 version, 24 PIs), against 27 attributes of climate-resilient fishery systems (Mason et al., 2022). Scores are 0 (missing), 1 (minimal), 2 (moderate) or 3 (comprehensive).

## How to run

Open `Zhao_etal_FIP_climate_resilience.Rproj` in RStudio and run `source("R/run_all.R")`. This rebuilds `data/processed/` and `output/` from `data/raw/` in a few seconds. It needs R 4.1 or later and the packages tidyverse, here, patchwork, systemfonts and ragg.

## Folders

| Folder | Contents |
|---|---|
| `data/raw/` | Scores, rationale and definitions entered by the authors |
| `data/processed/` | Scores and figure data derived from the raw data by `R/01_prepare_data.R` |
| `R/` | `01_prepare_data.R` builds the processed data. `02` to `04` print the numbers in each Results section. Each `figure_*.R` draws one figure. `run_all.R` runs them all in order, and `setup.R` holds shared settings. |
| `output/figures/` | Figures 1 to 3, A.1 and A.2, as PNG (600 dpi) |
| `output/tables/` | Supporting tables for the Results |

## Raw data (`data/raw/`)

| File | Rows | Columns |
|---|---|---|
| `performance_indicators.csv` | 49 PIs (Table 1) | `assessment` (ERA or SRA), `principle` (ES P1 to P3, SR P1 to P3), `principle_name`, `pi_code` (e.g. ERA 1.1.1), `pi_name`, `pi_label` (short name used in Fig. 1), `sra_core` (TRUE for the 12 core SRA PIs) |
| `resilience_attributes.csv` | 27 attributes (Table 2) | `dimension` (Ecological, Governance or Socio-economic), `attribute`, `n_components` (1 or 2), `definition` |
| `attribute_components.csv` | 40 components | `dimension`, `attribute`, `component` (C1 or C2), `component_definition` |
| `pi_component_scores.csv` | 90 scores (appendix) | `assessment`, `pi_code`, `pi_name`, `attribute`, `component`, `score` (1 to 3), `rationale` (written justification). Unlisted PI and component pairs scored 0. |
| `collective_coverage_adjustments.csv` | 5 scores | `assessment`, `attribute`, `best_pi_score` (best single PI), `assessment_score` (the assessment's PIs considered together), `pis_reviewed` |

## Processed data (`data/processed/`)

| File | Rows | Columns |
|---|---|---|
| `pi_attribute_scores.csv` | 1,323 (49 PIs x 27 attributes) | `assessment`, `principle`, `pi_code`, `pi_name`, `pi_label`, `sra_core`, `dimension`, `attribute`, `score` (0 to 3) |
| `assessment_attribute_scores.csv` | 27 attributes | `dimension`, `attribute`, `era_best_pi_score`, `era_score`, `sra_best_pi_score`, `sra_score`, `era_sra_score` |
| `figure_1_data.csv` | 1,323 | `assessment`, `principle`, `pi_code`, `pi_label` (asterisk marks core SRA PIs), `highly_relevant` (TRUE if the PI covers any attribute comprehensively), `dimension`, `attribute`, `score` |
| `figure_2_data.csv` | 27 | `dimension`, `attribute`, `era_score`, `sra_score`, `coverage_score` (ERA+SRA), `coverage_level`, `instrumental` (ERA, SRA, ERA and SRA, or Neither) |
| `figure_3_data.csv` | 12 | `dimension` (Overall or a dimension), `approach` (ERA, SRA or ERA+SRA), `attributes`, `points` (sum of scores), `max_points` (3 x `attributes`), `coverage` (`points` / `max_points`) |
| `figure_A1_data.csv` | 6 | `assessment`, `score`, `n` (instances), `strength`, `share` (of the assessment's instances) |
| `figure_A2_data.csv` | 38 relevant PIs | `assessment`, `pi_code`, `pi_name`, `n_attributes` (attributes the PI covers) |

## How scores are combined

- Attributes with two components are scored on how much of the whole attribute a PI covers. Comprehensive (3): one component comprehensive and the other at least moderate. Moderate (2): one component comprehensive and the other minimal or missing, or one moderate and the other moderate or minimal. Minimal (1): one component covered moderately or minimally with the other missing, or both minimal. Missing (0): both components missing. This is equivalent to the mean of the two component scores rounded half up.
- An assessment's score for an attribute is the best score of its PIs, raised in the five cases where its PIs together covered more (`collective_coverage_adjustments.csv`).
- The ERA+SRA score is the higher of the ERA and SRA scores. Percent coverage is the sum of scores divided by 3 times the number of attributes.

## References

Mason, J. G. et al. Attributes of climate resilience in fisheries: from theory to practice. *Fish and Fisheries* **23**, 522–544 (2022). https://doi.org/10.1111/faf.12630

Claude Opus 5.5 (Anthropic) was used to help clean up and annotate the code and to improve the design of the original figures. All code changes were reviewed by LZZ.

## Contact

Lily Z. Zhao
