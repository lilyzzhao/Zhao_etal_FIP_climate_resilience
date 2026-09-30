# Shared settings for every script

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(patchwork)
  library(systemfonts)
})

# File paths -------------------------------------------------------------------

path_raw <- function(file) here("data", "raw", file)
path_processed <- function(file) here("data", "processed", file)
path_tables <- function(file) here("output", "tables", file)
path_figures <- function(file) here("output", "figures", file)

# Display orders ---------------------------------------------------------------

dimensions <- c("Ecological", "Governance", "Socio-economic")
principles <- c("ES P1", "ES P2", "ES P3", "SR P1", "SR P2", "SR P3")
score_names <- c(`0` = "Missing", `1` = "Minimal", `2` = "Moderate", `3` = "Comprehensive")

# Attributes within each dimension, as in Figure 1 (covered attributes first)
attribute_order <- c(
  "Age structure", "Population abundance", "Habitat diversity",
  "Ecosystem connectivity", "Species diversity",
  "Participatory", "Equitable and inclusive", "Accountable", "Transparent",
  "Efficient and effective", "Adaptive", "Polycentric", "Responsive",
  "Cross-scale integration", "Leadership and initiative",
  "Agency", "Access to knowledge", "Knowledge diversity",
  "Access to economic opportunity", "Wealth and reserves", "Social capital",
  "Economic diversity", "Learning capacity", "Mobility",
  "Flexible and agile infrastructure", "Resilience mindset", "Technology transfer"
)

# Colours ----------------------------------------------------------------------

assessment_colours <- c(ERA = "#23764F", SRA = "#65428C")
dimension_colours <- c(Ecological = "#007F54", Governance = "#005EA2",
                       `Socio-economic` = "#C9B800")
overall_colour <- "#6C6D4F"  # all 27 attributes together (Figure 3a)
ink <- "#1A1A1A"
muted <- "#666666"

# Mix a colour with white: amount = 1 returns the colour, 0 returns white
tint <- function(colour, amount) scales::col_mix("white", colour, amount)

# Figure helpers ---------------------------------------------------------------

font <- "Arial"
pt <- function(points) points / .pt  # font size in points to ggplot size units

# Width of a string in mm, used to lay out labels
text_width_mm <- function(text, points, bold = FALSE) {
  string_width(text, family = font, size = points, res = 1000,
               weight = if (bold) "bold" else "normal") / 1000 * 25.4
}

theme_figure <- function() {
  theme_classic(base_size = 7, base_family = font) +
    theme(
      text = element_text(colour = ink),
      axis.text = element_text(colour = ink, size = 7),
      axis.line = element_line(linewidth = 0.3),
      axis.ticks = element_line(linewidth = 0.3),
      plot.title = element_text(size = 7, face = "bold"),
      plot.tag = element_text(size = 8, face = "bold"),
      legend.title = element_text(size = 7, face = "bold"),
      legend.text = element_text(size = 7),
      legend.key.size = unit(3, "mm")
    )
}

# Add any style columns a table leaves out, and fill their gaps, with defaults
fill_defaults <- function(data, defaults) {
  bind_rows(as_tibble(defaults)[0, ], data) %>%
    replace_na(defaults)
}

# Draw a figure laid out by hand in mm (Figures 1 and 2).
#   rects:  x0, x1, y0, y1, fill, and optionally colour (outline) and linetype
#   labels: x, y, label, and optionally points, colour, face, hjust, vjust, angle
# y is measured downward from the top of the figure and flipped here.
# Rectangles are drawn in row order, so backgrounds must come first.
draw_canvas <- function(rects, labels, width, height) {
  rects <- fill_defaults(rects, list(colour = NA_character_, linetype = "solid"))
  labels <- fill_defaults(labels, list(points = 7, colour = ink, face = "plain",
                                       hjust = 0.5, vjust = 0.5, angle = 0))
  ggplot() +
    geom_rect(data = rects,
              aes(xmin = x0, xmax = x1, ymin = height - y1, ymax = height - y0,
                  fill = fill, colour = colour, linetype = linetype),
              linewidth = 0.25) +
    geom_text(data = labels,
              aes(x, height - y, label = label, size = pt(points), colour = colour,
                  fontface = face, hjust = hjust, vjust = vjust, angle = angle),
              family = font) +
    scale_fill_identity() +
    scale_colour_identity() +
    scale_linetype_identity() +
    scale_size_identity() +
    scale_x_continuous(limits = c(0, width), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, height), expand = c(0, 0)) +
    coord_fixed(clip = "off") +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0),
          plot.background = element_rect(fill = "white", colour = NA))
}

# Save a figure as a PNG (600 dpi). Sizes are in mm.
save_figure <- function(plot, name, width, height) {
  ggsave(path_figures(str_c(name, ".png")), plot, width = width, height = height,
         units = "mm", dpi = 600, device = ragg::agg_png, bg = "white")
}

# Console output ---------------------------------------------------------------

say <- function(...) cat(sprintf(...), "\n", sep = "")
heading <- function(title) cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")
