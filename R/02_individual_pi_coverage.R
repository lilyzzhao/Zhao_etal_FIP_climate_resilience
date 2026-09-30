# 02 Comparisons of coverage provided by individual PIs
#
# Results sections: "Relevant PIs" and "Highly relevant PIs"
# Input:  data/processed/pi_attribute_scores.csv, figure_A1_data.csv and
#         figure_A2_data.csv (from 01_prepare_data.R)
# Output: the reported numbers, printed to the console, and output/tables/02_*.csv
# Figures for these sections: figure_1.R, figure_A1.R and figure_A2.R

source(here::here("R", "setup.R"))

pi_attribute_scores <- read_csv(path_processed("pi_attribute_scores.csv"), show_col_types = FALSE)
coverage_strength <- read_csv(path_processed("figure_A1_data.csv"), show_col_types = FALSE)
attributes_per_pi <- read_csv(path_processed("figure_A2_data.csv"), show_col_types = FALSE)

# Relevant PIs -----------------------------------------------------------------
# A PI is relevant if it covers at least one attribute (a score of 1 or more).
# Each PI and attribute pair with a score of 1 or more is one instance of coverage.

relevant_pis <- bind_rows(
  pi_attribute_scores,
  mutate(pi_attribute_scores, assessment = "ERA and SRA")  # both assessments together
) %>%
  summarise(pis = n_distinct(pi_code),
            relevant_pis = n_distinct(pi_code[score > 0]),
            instances = sum(score > 0),
            attributes_covered = n_distinct(attribute[score > 0]),
            .by = assessment)

# Number of attributes each relevant PI covers, compared with a Mann-Whitney U
# test (normal approximation with continuity correction)
mann_whitney <- wilcox.test(n_attributes ~ assessment, data = attributes_per_pi, exact = FALSE)
attributes_per_pi_test <- attributes_per_pi %>%
  summarise(relevant_pis = n(), median_attributes = median(n_attributes), .by = assessment) %>%
  mutate(mann_whitney_w = unname(mann_whitney$statistic), p_value = mann_whitney$p.value)

heading("Relevant PIs")
both <- filter(relevant_pis, assessment == "ERA and SRA")
say("%d of %d PIs are relevant. Together they cover %d of %d attributes in %d instances.",
    both$relevant_pis, both$pis, both$attributes_covered,
    n_distinct(pi_attribute_scores$attribute), both$instances)
relevant_pis %>%
  filter(assessment != "ERA and SRA") %>%
  pwalk(\(assessment, pis, relevant_pis, instances, ...) {
    say("  %s: %d of %d PIs relevant, %d instances", assessment, relevant_pis, pis, instances)
  })
coverage_strength %>%
  filter(strength == "Minimal") %>%
  pwalk(\(assessment, share, ...) say("  %s instances that are minimal: %.0f%%", assessment, 100 * share))
attributes_per_pi_test %>%
  pwalk(\(assessment, median_attributes, ...) {
    say("  %s: median of %g attributes per relevant PI", assessment, median_attributes)
  })
say("  Mann-Whitney U test: W = %.1f, p = %.4f", mann_whitney$statistic, mann_whitney$p.value)

# Highly relevant PIs ----------------------------------------------------------
# A PI is highly relevant if it covers at least one attribute comprehensively (score 3).

highly_relevant <- pi_attribute_scores %>%
  filter(score == 3) %>%
  select(assessment, pi_code, pi_name, attribute)

heading("Highly relevant PIs")
highly_relevant %>%
  summarise(pis = n_distinct(pi_code),
            era = n_distinct(pi_code[assessment == "ERA"]),
            sra = n_distinct(pi_code[assessment == "SRA"]),
            attributes = n_distinct(attribute)) %>%
  pwalk(\(pis, era, sra, attributes) {
    say("%d PIs (%d ERA, %d SRA) give comprehensive coverage of %d attributes:",
        pis, era, sra, attributes)
  })
highly_relevant %>%
  pwalk(\(pi_code, pi_name, attribute, ...) say("  %s %s -> %s", pi_code, pi_name, attribute))

# Save tables ------------------------------------------------------------------

write_csv(relevant_pis, path_tables("02_relevant_pis.csv"))
write_csv(attributes_per_pi_test, path_tables("02_attributes_per_pi_test.csv"))
write_csv(highly_relevant, path_tables("02_highly_relevant_pis.csv"))
