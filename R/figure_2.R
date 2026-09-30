# Figure 2. Typology of resilience attributes by instrumental assessment
#
# Input:  data/processed/figure_2_data.csv (from 01_prepare_data.R)
# Output: output/figures/Figure_2.png
#
# Each attribute is a box placed by its dimension (row) and the degree of
# coverage the instrumental assessment provided (column), and colored by the
# instrumental assessment. The figure is laid out in mm, with y measured
# downward from the top.

source(here::here("R", "setup.R"))

figure_2_data <- read_csv(path_processed("figure_2_data.csv"), show_col_types = FALSE)

coverage_levels <- c("Comprehensive", "Moderate", "Minimal", "Missing")  # columns, left to right
instrumental_order <- c("ERA", "ERA and SRA", "SRA", "Neither")          # legend and stacking order
cell_fill <- "#F2F2EF"
missing_outline <- "#8C8C8C"

# Layout settings (mm) ---------------------------------------------------------

fig_width <- 183
strip_w <- 20             # dimension labels on the left
col_w <- (fig_width - strip_w) / length(coverage_levels)
cell_gap <- 1             # gap between cells
pad_x <- 1                # cell padding
pad_y <- 1.5
box_h <- 4.4              # attribute box height
box_gap <- 1.1            # gap between boxes in a cell
box_w <- col_w - cell_gap - 2 * pad_x
legend_y <- 0.5
header_y <- 9
rows_top <- 18.5

# Rows (dimensions) and columns (coverage levels) ------------------------------

boxes <- figure_2_data %>%
  mutate(row = match(dimension, dimensions),
         col = match(coverage_level, coverage_levels)) %>%
  arrange(row, col, match(instrumental, instrumental_order), attribute) %>%
  mutate(slot = row_number(), .by = c(row, col))  # position within its cell

# Each row is tall enough for its fullest cell
rows <- boxes %>%
  count(row, col) %>%
  summarise(n_max = max(n), .by = row) %>%
  mutate(dimension = dimensions[row],
         height = n_max * box_h + (n_max - 1) * box_gap + 2 * pad_y,
         y0 = rows_top + lag(cumsum(height + cell_gap), default = 0))
fig_height <- max(rows$y0 + rows$height) + 0.5

columns <- tibble(col = seq_along(coverage_levels), coverage_level = coverage_levels) %>%
  mutate(x0 = strip_w + (col - 1) * col_w)

cells <- expand_grid(select(rows, row, y0, height), select(columns, col, x0))

boxes <- boxes %>%
  left_join(select(rows, row, row_y0 = y0), by = "row") %>%
  left_join(select(columns, col, col_x0 = x0), by = "col") %>%
  mutate(x0 = col_x0 + cell_gap / 2 + pad_x,
         x1 = x0 + box_w,
         y0 = row_y0 + pad_y + (slot - 1) * (box_h + box_gap),
         y1 = y0 + box_h)

# Box fills: one colour per assessment. Attributes where the ERA and SRA are
# both instrumental get a box split into an ERA half and an SRA half.
box_rects <- function(boxes) {
  both <- filter(boxes, instrumental == "ERA and SRA")
  bind_rows(
    boxes %>% filter(instrumental %in% c("ERA", "SRA")) %>%
      mutate(fill = assessment_colours[instrumental]),
    both %>% mutate(x1 = (x0 + x1) / 2, fill = assessment_colours[["ERA"]]),
    both %>% mutate(x0 = (x0 + x1) / 2, fill = assessment_colours[["SRA"]]),
    boxes %>% filter(instrumental == "Neither") %>%
      mutate(fill = "white", colour = missing_outline, linetype = "22")
  ) %>%
    select(x0, x1, y0, y1, fill, any_of(c("colour", "linetype")))
}

# Legend: one key per instrumental assessment -----------------------------------

legend <- tibble(
  instrumental = instrumental_order,
  label = c("ERA", "ERA and SRA equally", "SRA", "Not covered by either")
) %>%
  mutate(title_w = text_width_mm("Instrumental assessment", 7, bold = TRUE),
         item_w = 6.2 + text_width_mm(label, 7) + 5,  # key, label and space after
         x0 = title_w + 4 + lag(cumsum(item_w), default = 0),
         x1 = x0 + 5,
         y0 = legend_y,
         y1 = y0 + box_h)

# Rectangles, drawn in this order ----------------------------------------------

rects <- bind_rows(
  # Cell backgrounds, inset by half the gap on each side
  transmute(cells, x1 = x0 + col_w - cell_gap / 2, x0 = x0 + cell_gap / 2,
            y1 = y0 + height, y0, fill = cell_fill),
  box_rects(boxes),
  box_rects(legend)
)

# Labels -----------------------------------------------------------------------

labels <- bind_rows(
  # Attribute names inside the boxes
  transmute(boxes, x = x0 + 1.2, y = (y0 + y1) / 2, label = attribute, hjust = 0,
            colour = if_else(instrumental == "Neither", muted, "white")),
  # Cells with no attributes
  cells %>%
    anti_join(boxes, by = c("row", "col")) %>%
    transmute(x = x0 + cell_gap / 2 + pad_x + 1.2, y = y0 + pad_y + box_h / 2,
              label = "None", colour = muted, hjust = 0),
  # Row labels: dimension, level with the first box in the row
  transmute(rows, x = 0, y = y0 + pad_y + box_h / 2, label = dimension,
            points = 7, face = "bold", hjust = 0),
  # Column headers: coverage level
  tibble(x = strip_w + cell_gap / 2, y = header_y, hjust = 0, colour = muted,
         label = "Degree of coverage provided by the instrumental assessment"),
  transmute(columns, x = x0 + cell_gap / 2 + pad_x, y = header_y + 5.2,
            label = coverage_level, points = 7, face = "bold", hjust = 0),
  # Legend
  tibble(x = 0, y = legend_y + box_h / 2, label = "Instrumental assessment",
         points = 7, face = "bold", hjust = 0),
  transmute(legend, x = x1 + 1.2, y = (y0 + y1) / 2, label, hjust = 0)
)

figure_2 <- draw_canvas(rects, labels, fig_width, fig_height)
save_figure(figure_2, "Figure_2", fig_width, fig_height)
