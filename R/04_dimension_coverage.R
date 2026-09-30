# 04 Coverage of climate resilience by dimension and assessment approach
#
# Results section: "Coverage of climate resilience by dimension and assessment approach"
# Input:  data/processed/figure_3_data.csv (from 01_prepare_data.R)
# Output: the reported percentages, printed to the console
# Figure for this section: figure_3.R
#
# Percent coverage = sum of the attribute scores / the maximum possible score
# (3 x the number of attributes), for each dimension and for all 27 attributes.

source(here::here("R", "setup.R"))

dimension_coverage <- read_csv(path_processed("figure_3_data.csv"), show_col_types = FALSE)

heading("Coverage of climate resilience by dimension and assessment approach")
dimension_coverage %>%
  summarise(text = str_c(sprintf("%s %.0f%%", approach, 100 * coverage), collapse = ", "),
            .by = dimension) %>%
  pwalk(\(dimension, text) say("%s: %s", dimension, text))
