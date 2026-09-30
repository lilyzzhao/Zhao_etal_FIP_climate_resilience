# Figure 3. Percent coverage of climate resilience by dimension and assessment approach
#
# Input:  data/processed/figure_3_data.csv (from 01_prepare_data.R)
# Output: output/figures/Figure_3.png
#
# A 2 x 2 grid: all 27 attributes (a), then one panel per dimension (b-d). Each
# bar is the sum of the attribute scores over the maximum possible score.

source(here::here("R", "setup.R"))

figure_3_data <- read_csv(path_processed("figure_3_data.csv"), show_col_types = FALSE)

panel_order <- c("Overall", dimensions)
panel_colours <- c(Overall = overall_colour, dimension_colours)

# Bar chart for one panel. Axis titles go on the outer panels only.
coverage_panel <- function(panel_dimension, y_title = FALSE, x_title = FALSE) {
  colour <- panel_colours[[panel_dimension]]
  panel <- figure_3_data %>%
    filter(dimension == panel_dimension) %>%
    mutate(
      # Label in the middle of the bar, or just above the axis for small bars
      label_y = pmax(coverage / 2, 0.045),
      label_colour = if_else(coverage < 0.09 | panel_dimension == "Socio-economic", ink, "white")
    )
  ggplot(panel, aes(x = approach, y = coverage)) +
    geom_col(fill = colour, width = 0.6) +
    geom_text(aes(y = label_y, label = scales::percent(coverage, accuracy = 1), colour = label_colour),
              size = pt(7), family = font) +
    scale_colour_identity() +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                       labels = scales::percent, expand = expansion(mult = c(0, 0.03))) +
    labs(title = str_c(panel_dimension, " resilience"),
         x = if (x_title) "FIP assessment approach" else NULL,
         y = if (y_title) "Percent coverage" else NULL) +
    theme_figure() +
    theme(plot.title = element_text(size = 7.5, face = "bold", hjust = 0.5, colour = colour),
          axis.title = element_text(size = 7, face = "bold"))
}

panels <- list(
  coverage_panel("Overall", y_title = TRUE),
  coverage_panel("Ecological"),
  coverage_panel("Governance", y_title = TRUE, x_title = TRUE),
  coverage_panel("Socio-economic", x_title = TRUE)
)

# Legend box: the number of attributes behind each panel
legend_items <- figure_3_data %>%
  distinct(dimension, attributes) %>%
  arrange(match(dimension, panel_order)) %>%
  mutate(label = sprintf("%s (%d attributes)", dimension, attributes),
         x = c(0, 1, 0, 1), y = c(1, 1, 0, 0))

legend_box <- ggplot(legend_items, aes(x, y)) +
  geom_point(aes(colour = dimension), size = 2.2) +
  geom_text(aes(x = x + 0.06, label = label), hjust = 0, size = pt(7), family = font, colour = ink) +
  annotate("text", x = -0.06, y = 1.85, hjust = 0, label = "Number of climate attributes in each panel",
           size = pt(7), fontface = "bold", family = font, colour = ink) +
  scale_colour_manual(values = panel_colours, guide = "none") +
  coord_cartesian(xlim = c(-0.1, 1.9), ylim = c(-0.6, 2.4), clip = "off") +
  theme_void() +
  theme(panel.border = element_rect(colour = "grey65", fill = NA, linewidth = 0.4))

# Panels in a 2 x 2 grid with the legend below, tagged a-d in bold
figure_3 <- wrap_plots(c(panels, list(legend_box)), design = "AB\nCD\nEE",
                       heights = c(1, 1, 0.3)) +
  plot_annotation(tag_levels = list(c("a", "b", "c", "d", ""))) &
  theme(plot.tag = element_text(size = 8, face = "bold", family = font))

save_figure(figure_3, "Figure_3", 140, 140)
