# Fig. A.1. Strength of coverage in each instance, by assessment
#
# Input:  data/processed/figure_A1_data.csv (from 01_prepare_data.R)
# Output: output/figures/Figure_A1.png

source(here::here("R", "setup.R"))

figure_A1_data <- read_csv(path_processed("figure_A1_data.csv"), show_col_types = FALSE)

strength_levels <- c("Minimal", "Moderate", "Comprehensive")
strength_fills <- set_names(tint("#404040", c(0.4, 0.68, 1)), strength_levels)  # as in Figure 1

figure_A1 <- figure_A1_data %>%
  mutate(total = sum(n), .by = assessment) %>%
  mutate(assessment = sprintf("%s (n = %d)", assessment, total),
         strength = factor(strength, levels = rev(strength_levels))) %>%
  ggplot(aes(x = n, y = fct_rev(assessment), fill = strength)) +
  geom_col(width = 0.6, colour = "white", linewidth = 0.3) +
  # Share of each assessment's instances, printed inside each segment
  geom_text(aes(label = scales::percent(share, accuracy = 1), group = strength,
                colour = if_else(strength == "Comprehensive", "white", ink)),
            position = position_stack(vjust = 0.5), size = pt(7), family = font) +
  scale_fill_manual(values = strength_fills, breaks = strength_levels,
                    name = "Strength of coverage") +
  scale_colour_identity() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.03))) +
  labs(x = sprintf("Instances of attribute coverage by relevant PIs (n = %d)",
                   sum(figure_A1_data$n)),
       y = NULL) +
  theme_figure() +
  theme(legend.position = "top", legend.justification = "left")

save_figure(figure_A1, "Figure_A1", 140, 45)
