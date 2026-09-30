# 03 Comparison of collective coverage provided by the ERA and SRA
#
# Results sections: "The ERA", "The SRA" and "Summary of the extent of coverage by assessment"
# Input:  data/processed/assessment_attribute_scores.csv, pi_attribute_scores.csv
#         and figure_2_data.csv (from 01_prepare_data.R)
# Output: the reported numbers, printed to the console, and output/tables/03_collective_increases.csv
# Figure for these sections: figure_2.R

source(here::here("R", "setup.R"))

assessment_attribute_scores <- read_csv(path_processed("assessment_attribute_scores.csv"), show_col_types = FALSE)
pi_attribute_scores <- read_csv(path_processed("pi_attribute_scores.csv"), show_col_types = FALSE)
typology <- read_csv(path_processed("figure_2_data.csv"), show_col_types = FALSE)

# One row per assessment and attribute: the best single PI score and the score
# for the assessment's PIs considered together
assessment_scores <- assessment_attribute_scores %>%
  pivot_longer(c(era_best_pi_score, era_score, sra_best_pi_score, sra_score),
               names_to = c("assessment", ".value"), names_pattern = "(era|sra)_(.*)") %>%
  transmute(dimension, attribute, assessment = str_to_upper(assessment),
            best_pi_score, assessment_score = score)

# Attributes where the collective review raised the score, with the PIs reviewed
collective_increases <- assessment_scores %>%
  filter(assessment_score > best_pi_score) %>%
  left_join(
    pi_attribute_scores %>%
      filter(score > 0) %>%
      summarise(pis = str_c(pi_code, collapse = "; "), .by = c(assessment, attribute)),
    by = c("assessment", "attribute")
  ) %>%
  arrange(assessment)  # ERA first, then SRA

# The ERA and The SRA ----------------------------------------------------------

walk(c("ERA", "SRA"), \(a) {
  heading(paste("The", a))
  comprehensive <- filter(assessment_scores, assessment == a, assessment_score == 3)
  say("Comprehensive coverage of %d attributes: %s", nrow(comprehensive),
      str_c(comprehensive$attribute, collapse = ", "))
  collective_increases %>%
    filter(assessment == a) %>%
    pwalk(\(attribute, best_pi_score, assessment_score, pis, ...) {
      say("  Considered together, %s rose from %s to %s (PIs: %s)", attribute,
          score_names[as.character(best_pi_score)], score_names[as.character(assessment_score)], pis)
    })
  greater <- filter(typology, instrumental == a)
  say("Instrumental (greater coverage than the other assessment) for %d attributes:", nrow(greater))
  greater %>%
    summarise(n = n(), attributes = str_c(attribute, collapse = ", "), .by = dimension) %>%
    pwalk(\(dimension, n, attributes) say("  %s (%d): %s", dimension, n, attributes))
})

both <- filter(typology, instrumental == "ERA and SRA")
say("\nERA and SRA PIs instrumental (same coverage) for %d attributes: %s",
    nrow(both), str_c(both$attribute, collapse = ", "))

# Summary of the extent of coverage by assessment ------------------------------

heading("Summary of the extent of coverage by assessment")
typology %>%
  arrange(desc(coverage_score)) %>%
  summarise(n = n(), attributes = str_c(sprintf("%s (%s)", attribute, instrumental), collapse = ", "),
            .by = coverage_level) %>%
  pwalk(\(coverage_level, n, attributes) say("%s: %d attributes. %s", coverage_level, n, attributes))

write_csv(select(collective_increases, dimension, attribute, assessment,
                 best_pi_score, assessment_score, pis),
          path_tables("03_collective_increases.csv"))
