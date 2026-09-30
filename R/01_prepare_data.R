# 01 Prepare data
#
# Outputs in data/processed/:
#   pi_attribute_scores.csv          every PI scored against every attribute (49 x 27 = 1,323 rows)
#   assessment_attribute_scores.csv  every attribute scored for the ERA, SRA and ERA+SRA (27 rows)
#   figure_1_data.csv                Figure 1: coverage of each attribute by each PI
#   figure_2_data.csv                Figure 2: instrumental assessment and degree of coverage
#   figure_3_data.csv                Figure 3: percent coverage by dimension and assessment approach
#   figure_A1_data.csv               Fig. A.1: strength of coverage in each instance
#   figure_A2_data.csv               Fig. A.2: number of attributes covered by each relevant PI
#
# Scoring rules (Methods):
#   1. Each PI was scored 0-3 against each attribute component. Pairs with no
#      recorded score are 0 (no conceptual overlap).
#   2. An attribute with one component takes that component's score. An attribute
#      with two components takes the mean of the two, rounded half up
#      (0.5 -> 1, 1.5 -> 2, 2.5 -> 3).
#   3. An assessment's score for an attribute starts as the best score of any of
#      its PIs. The collective review raised five of these scores
#      (data/raw/collective_coverage_adjustments.csv).
#   4. The ERA+SRA score for an attribute is the higher of the ERA and SRA scores.

source(here::here("R", "setup.R"))

# Read raw data ----------------------------------------------------------------

performance_indicators <- read_csv(path_raw("performance_indicators.csv"), show_col_types = FALSE)
resilience_attributes <- read_csv(path_raw("resilience_attributes.csv"), show_col_types = FALSE)
attribute_components <- read_csv(path_raw("attribute_components.csv"), show_col_types = FALSE)
component_scores <- read_csv(path_raw("pi_component_scores.csv"), show_col_types = FALSE)
adjustments <- read_csv(path_raw("collective_coverage_adjustments.csv"), show_col_types = FALSE)

# Step 1. Score every PI against every attribute component (rule 1) -----------

all_component_scores <- expand_grid(
  pi_code = performance_indicators$pi_code,
  select(attribute_components, attribute, component)
) %>%
  left_join(select(component_scores, pi_code, attribute, component, score),
            by = c("pi_code", "attribute", "component")) %>%
  mutate(score = replace_na(score, 0))

# Step 2. Combine components into one score per PI and attribute (rule 2) -----

round_half_up <- function(x) floor(x + 0.5)  # base round() sends 0.5 and 2.5 to the even number

pi_attribute_scores <- all_component_scores %>%
  summarise(score = round_half_up(mean(score)), .by = c(pi_code, attribute)) %>%
  left_join(performance_indicators, by = "pi_code") %>%
  left_join(select(resilience_attributes, attribute, dimension), by = "attribute") %>%
  select(assessment, principle, pi_code, pi_name, pi_label, sra_core,
         dimension, attribute, score) %>%
  arrange(assessment, principle, pi_code, match(attribute, attribute_order))

# Step 3. Score each assessment, then both together (rules 3 and 4) -----------

assessment_attribute_scores <- pi_attribute_scores %>%
  summarise(best_pi_score = max(score), .by = c(assessment, attribute)) %>%
  left_join(select(adjustments, assessment, attribute, collective_score = assessment_score),
            by = c("assessment", "attribute")) %>%
  mutate(score = coalesce(collective_score, best_pi_score)) %>%
  # One row per attribute: era_best_pi_score, era_score, sra_best_pi_score, sra_score
  pivot_wider(id_cols = attribute, names_from = assessment,
              values_from = c(best_pi_score, score),
              names_glue = "{str_to_lower(assessment)}_{.value}") %>%
  mutate(era_sra_score = pmax(era_score, sra_score)) %>%
  left_join(select(resilience_attributes, attribute, dimension), by = "attribute") %>%
  select(dimension, attribute, era_best_pi_score, era_score,
         sra_best_pi_score, sra_score, era_sra_score) %>%
  arrange(match(dimension, dimensions), match(attribute, attribute_order))

# Figure 1 data ----------------------------------------------------------------
# One row per PI and attribute. Core SRA PIs are marked with an asterisk. A PI is
# highly relevant if it covers at least one attribute comprehensively (score 3).

figure_1_data <- pi_attribute_scores %>%
  mutate(highly_relevant = any(score == 3), .by = pi_code) %>%
  mutate(pi_label = if_else(sra_core, str_c(pi_label, "*"), pi_label)) %>%
  select(assessment, principle, pi_code, pi_label, highly_relevant,
         dimension, attribute, score)

# Figure 2 data ----------------------------------------------------------------
# One row per attribute. The instrumental assessment is the one whose PIs
# together gave greater coverage. When both gave the same coverage, both are
# instrumental ("ERA and SRA"). The coverage level is the ERA+SRA score.

figure_2_data <- assessment_attribute_scores %>%
  transmute(
    dimension, attribute, era_score, sra_score,
    coverage_score = era_sra_score,
    coverage_level = unname(score_names[as.character(coverage_score)]),
    instrumental = case_when(
      coverage_score == 0 ~ "Neither",
      era_score > sra_score ~ "ERA",
      sra_score > era_score ~ "SRA",
      .default = "ERA and SRA"
    )
  )

# Figure 3 data ----------------------------------------------------------------
# Percent coverage of each dimension, and of all 27 attributes ("Overall"), by
# each assessment approach: the sum of the attribute scores divided by the
# maximum possible score (3 x the number of attributes).

approach_scores <- assessment_attribute_scores %>%
  select(dimension, attribute, ERA = era_score, `ERA+SRA` = era_sra_score, SRA = sra_score) %>%
  pivot_longer(c(ERA, `ERA+SRA`, SRA), names_to = "approach", values_to = "score")

figure_3_data <- bind_rows(mutate(approach_scores, dimension = "Overall"), approach_scores) %>%
  summarise(attributes = n(), points = sum(score), .by = c(dimension, approach)) %>%
  mutate(max_points = 3 * attributes, coverage = points / max_points) %>%
  arrange(match(dimension, c("Overall", dimensions)), approach)

# Fig. A.1 and A.2 data ----------------------------------------------------------
# Each PI and attribute pair with a score of 1 or more is one instance of coverage.

instances <- filter(pi_attribute_scores, score > 0)

# Fig. A.1: number and share of each assessment's instances at each strength
figure_A1_data <- instances %>%
  count(assessment, score) %>%
  mutate(strength = unname(score_names[as.character(score)]),
         share = n / sum(n), .by = assessment)

# Fig. A.2: number of attributes each relevant PI covers
figure_A2_data <- instances %>%
  count(assessment, pi_code, pi_name, name = "n_attributes")

# Save -------------------------------------------------------------------------

processed <- list(
  pi_attribute_scores = pi_attribute_scores,
  assessment_attribute_scores = assessment_attribute_scores,
  figure_1_data = figure_1_data,
  figure_2_data = figure_2_data,
  figure_3_data = figure_3_data,
  figure_A1_data = figure_A1_data,
  figure_A2_data = figure_A2_data
)
iwalk(processed, \(data, name) write_csv(data, path_processed(str_c(name, ".csv"))))

heading("01 Prepare data")
iwalk(processed, \(data, name) say("  %-30s %5d rows", str_c(name, ".csv"), nrow(data)))
