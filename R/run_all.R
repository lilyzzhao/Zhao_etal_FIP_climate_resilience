# Runs the full analysis in order.

scripts <- c(
  # Data: raw scores -> processed scores and one CSV per figure
  "01_prepare_data.R",
  # Results: the numbers reported in each Results section
  "02_individual_pi_coverage.R",  # Relevant PIs; Highly relevant PIs
  "03_collective_coverage.R",     # The ERA; The SRA; Summary of coverage
  "04_dimension_coverage.R",      # Coverage by dimension and assessment approach
  # Figures: each reads its own CSV from data/processed
  "figure_1.R",
  "figure_2.R",
  "figure_3.R",
  "figure_A1.R",
  "figure_A2.R"
)

source(here::here("R", "setup.R"))  # loads the tidyverse
walk(scripts, \(script) source(here::here("R", script)))
