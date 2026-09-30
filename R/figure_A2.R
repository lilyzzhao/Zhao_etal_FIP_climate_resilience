# Fig. A.2. Number of attributes covered by each relevant PI, by assessment
#
# Input:  data/processed/figure_A2_data.csv (from 01_prepare_data.R)
# Output: output/figures/Figure_A2.png

source(here::here("R", "setup.R"))

figure_A2_data <- read_csv(path_processed("figure_A2_data.csv"), show_col_types = FALSE)

# Mann-Whitney U test, as reported in 02_individual_pi_coverage.R
mann_whitney <- wilcox.test(n_attributes ~ assessment, data = figure_A2_data, exact = FALSE)

# Legend labels with the number of relevant PIs and the median
legend_labels <- figure_A2_data %>%
  summarise(pis = n(), median = median(n_attributes), .by = assessment) %>%
  transmute(assessment, label = sprintf("%s (%d PIs, median = %g)", assessment, pis, median)) %>%
  deframe()

figure_A2 <- figure_A2_data %>%
  count(assessment, n_attributes, name = "pis") %>%
  complete(assessment, n_attributes = 1:max(n_attributes), fill = list(pis = 0)) %>%
  ggplot(aes(x = factor(n_attributes), y = pis, fill = assessment)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_fill_manual(values = assessment_colours, labels = legend_labels, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1, size = pt(7), family = font,
           label = sprintf("Mann-Whitney U: W = %.1f, p = %.3f",
                           mann_whitney$statistic, mann_whitney$p.value)) +
  labs(x = "Number of attributes covered by a relevant PI", y = "Number of PIs") +
  theme_figure() +
  theme(legend.position = "top", legend.justification = "left", legend.direction = "vertical")

save_figure(figure_A2, "Figure_A2", 89, 65)
