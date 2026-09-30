# Figure 1. Coverage of each resilience attribute (columns) by each PI (rows)
#
# Input:  data/processed/figure_1_data.csv (from 01_prepare_data.R)
# Output: output/figures/Figure_1.png
#
# Cells are shaded by the PI's score, in the colour of the attribute's dimension.
# The figure is laid out in mm, with y measured downward from the top.

source(here::here("R", "setup.R"))

figure_1_data <- read_csv(path_processed("figure_1_data.csv"), show_col_types = FALSE)

# Layout settings (mm) ---------------------------------------------------------

fig_width <- 183
label_pt <- 7             # PI and attribute labels
row_h <- 3.5              # height of one PI row
principle_gap <- 1.2      # gap between principles
assessment_gap <- 3       # gap between the ERA and SRA
dimension_gap <- 1.5      # gap between dimensions
band_w <- 5               # assessment band on the left
tag_w <- 5                # principle tags on the right
header_h <- 5.5           # dimension headers on top
score_tints <- c(0.15, 0.4, 0.68, 1)  # fill strength for scores 0 to 3

# Rows: one per PI, grouped by principle ---------------------------------------

pis <- figure_1_data %>%
  distinct(assessment, principle, pi_code, pi_label, highly_relevant) %>%
  arrange(match(principle, principles), pi_code) %>%
  mutate(
    gap = if_else(principle != lag(principle, default = first(principle)), principle_gap, 0) +
      if_else(assessment != lag(assessment, default = first(assessment)),
              assessment_gap - principle_gap, 0),
    y0 = header_h + 1 + (row_number() - 1) * row_h + cumsum(gap),
    y1 = y0 + row_h
  )
grid_bottom <- max(pis$y1)

# Columns: one per attribute, grouped by dimension ------------------------------

label_w <- max(text_width_mm(pis$pi_label, label_pt, bold = TRUE)) + 2
grid_x0 <- band_w + label_w + 1
grid_x1 <- fig_width - tag_w - 1
cell_w <- (grid_x1 - grid_x0 - 2 * dimension_gap) / length(attribute_order)

attributes <- tibble(attribute = attribute_order) %>%
  left_join(distinct(figure_1_data, attribute, dimension), by = "attribute") %>%
  mutate(x0 = grid_x0 + (row_number() - 1) * cell_w +
           (match(dimension, dimensions) - 1) * dimension_gap,
         x1 = x0 + cell_w)

# Below the grid: the rotated attribute labels, then the x-axis title
axis_title_y <- grid_bottom + 1 + max(text_width_mm(attribute_order, label_pt)) + 3

# Groups for the bands, tags and headers ---------------------------------------

assessment_bands <- pis %>%
  summarise(y0 = min(y0), y1 = max(y1), n = n(), .by = assessment) %>%
  mutate(colour = assessment_colours[assessment])

principle_tags <- pis %>%
  summarise(y0 = min(y0), y1 = max(y1), .by = c(assessment, principle)) %>%
  mutate(amount = c(1, 0.65, 0.35)[as.integer(str_sub(principle, -1))],  # P1 darkest
         fill = tint(assessment_colours[assessment], amount))

dimension_headers <- attributes %>%
  summarise(x0 = min(x0), x1 = max(x1), n = n(), .by = dimension)

# Legend and notes, below the PI labels
legend_y <- grid_bottom + 4
legend_keys <- tibble(score = 0:3) %>%
  mutate(y0 = legend_y + 3.2 + score * 3.8, y1 = y0 + 2.8,
         label = sprintf("%s (%d)", score_names[as.character(score)], score))
notes <- tibble(
  label = c("Bold", "PI: highly relevant", "(covers an attribute", "comprehensively)",
            "* Core SRA PI, required for", "FIPs in high-risk contexts", "(2021 SRA)"),
  face = c("bold", rep("plain", 6)),
  x = c(0, text_width_mm("Bold ", label_pt, bold = TRUE), 0, 0, 0, 0, 0),
  y = legend_y + 3.2 + 4 * 3.8 + 2 + c(0, 0, 3, 6, 9.6, 12.6, 15.6)
)

# Leave room below the grid for the attribute labels and axis title, or the legend
fig_height <- max(axis_title_y, max(notes$y)) + 3

# Rectangles, drawn in this order ----------------------------------------------

rects <- bind_rows(
  # Grid cells, with a thin white border
  figure_1_data %>%
    left_join(select(pis, pi_code, y0, y1), by = "pi_code") %>%
    left_join(select(attributes, attribute, x0, x1), by = "attribute") %>%
    transmute(x0, x1, y0, y1, colour = "white",
              fill = tint(dimension_colours[dimension], score_tints[score + 1])),
  # Assessment bands and the pale strip behind the PI labels
  transmute(assessment_bands, x0 = 0, x1 = band_w, y0, y1, fill = colour),
  transmute(assessment_bands, x0 = band_w, x1 = grid_x0 - 0.5, y0, y1, fill = tint(colour, 0.15)),
  # Principle tags on the right
  transmute(principle_tags, x0 = grid_x1 + 1, x1 = grid_x1 + 1 + tag_w, y0, y1, fill),
  # Dimension headers on top
  transmute(dimension_headers, x0, x1, y0 = 0.5, y1 = header_h, fill = dimension_colours[dimension]),
  # Legend keys, in grey
  transmute(legend_keys, x0 = 0, x1 = 4, y0, y1, fill = tint("#404040", score_tints[score + 1]))
)

# Labels -----------------------------------------------------------------------

labels <- bind_rows(
  transmute(assessment_bands, x = band_w / 2, y = (y0 + y1) / 2,
            label = sprintf("%s (%d PIs)", assessment, n),
            points = 7, colour = "white", face = "bold", angle = 90),
  # PI labels, in bold for highly relevant PIs
  transmute(pis, x = grid_x0 - 1.5, y = (y0 + y1) / 2, label = pi_label,
            points = label_pt, face = if_else(highly_relevant, "bold", "plain"), hjust = 1),
  transmute(principle_tags, x = grid_x1 + 1 + tag_w / 2, y = (y0 + y1) / 2, label = principle,
            colour = if_else(amount > 0.5, "white", ink), face = "bold", angle = -90),
  transmute(dimension_headers, x = (x0 + x1) / 2, y = (0.5 + header_h) / 2,
            label = sprintf("%s (%d)", dimension, n), points = 7, face = "bold",
            colour = if_else(dimension == "Socio-economic", ink, "white")),
  tibble(x = grid_x0 - 1.5, y = (0.5 + header_h) / 2, label = "Performance indicators (PIs)",
         points = 7, face = "bold", hjust = 1),
  transmute(attributes, x = (x0 + x1) / 2, y = grid_bottom + 1, label = attribute,
            points = label_pt, hjust = 1, angle = 90),
  # x-axis title, centred under the grid
  tibble(x = (grid_x0 + grid_x1) / 2, y = axis_title_y,
         label = "Attributes that confer climate resilience", points = 7, face = "bold"),
  # Legend and notes, below the PI labels
  tibble(x = 0, y = legend_y, label = "Coverage score", face = "bold", hjust = 0),
  transmute(legend_keys, x = 5.5, y = y0 + 1.4, label, hjust = 0),
  mutate(notes, hjust = 0)
)

figure_1 <- draw_canvas(rects, labels, fig_width, fig_height)
save_figure(figure_1, "Figure_1", fig_width, fig_height)
