#-------------------------------------------------------------------------------
# cross-stitch.r
#-------------------------------------------------------------------------------
#' create a cross stitch chart
#' requires a pattern with the following:
#' - row
#' - column
#' - width x (default 1)
#' - width y (default 1)
#' - colour
#' - direction (/ < or X (default))
#' will need to play with the chart parameters to adjust for the number of
#' stitches, so the corners can be seen
#' other ideas
#' - border size
#' - coord_fixed ratio 1.1
#' - calculate save ratio
#-------------------------------------------------------------------------------

library(tidyverse)

#---- chart parameters ----

padding <- 0.3

line_width <- 1.5

background_colour <- "grey10"

height_inches <- 6

coord_fixed_ratio <- 1.1

expand_mult <- 0.05

#---- create the pattern ----

base <- crossing(col = 1:15, row = 1:20) |>
  mutate(
    width = 1,
    height = 1,
    colour = if_else((col + row) %% 2 == 0, "#a01e1e", "#0c7dbe"),
    type = "X"
  ) |>
  filter(
    !(col %in% 4:6 & row %in% 14:17)
  ) |>
  add_row(
    col = 4, row = 14, width = 1, height = 2, colour = "grey20", type = "/"
  ) |>
  add_row(
    col = 5, row = 14, width = 1, height = 2, colour = "grey20", type = "<"
  ) |>
  add_row(
    col = 4, row = 16, width = 1, height = 2, colour = "grey20", type = "<"
  ) |>
  add_row(
    col = 5, row = 16, width = 1, height = 2, colour = "grey20", type = "/"
  ) |> 
  add_row(
    col = 6, row = 14:16, width = 1, height = 2, colour = "grey20", type = "X"
  )

#---- split out the / direction stitches and set start and end points ----

base_slash <- base |>
  filter(type %in% c("X", "/")) |>
  mutate(
    border_col = colorspace::darken(colour, 0.3),
    x = (col + width / 2) - (width - padding) / 2,
    xend = (col + width / 2) + (width - padding) / 2,
    y = (row + height / 2) - (height - padding) / 2,
    yend = (row + height / 2) + (height - padding) / 2
  )

#---- split out the \ (<) direction stitches and set start and end points ----

base_backslash <- base |>
  filter(type %in% c("X", "<")) |>
  mutate(
    border_col = colorspace::darken(colour, 0.3),
    x = (col + width / 2) - (width - padding) / 2,
    xend = (col + width / 2) + (width - padding) / 2,
    y = (row + height / 2) + (height - padding) / 2,
    yend = (row + height / 2) - (height - padding) / 2
  )

#---- create the chart ----

ggplot() +
  geom_segment(
    data = base_backslash,
    aes(
      x = x, xend = xend, y = y, yend = yend,
      colour = border_col
    ),
    linewidth = line_width * 1.2,
    lineend = "round"
  ) +
  geom_segment(
    data = base_backslash,
    aes(
      x = x, xend = xend, y = y, yend = yend,
      colour = colour
    ),
    linewidth = line_width,
    lineend = "round"
  ) +
  geom_segment(
    data = base_slash,
    aes(
      x = x, xend = xend, y = y, yend = yend,
      colour = border_col
    ),
    linewidth = line_width * 1.2,
    lineend = "round"
  ) +
  geom_segment(
    data = base_slash,
    aes(
      x = x, xend = xend, y = y, yend = yend,
      colour = colour
    ),
    linewidth = line_width,
    lineend = "round"
  ) +
  scale_x_continuous(expand = expansion(mult = c(expand_mult, expand_mult))) +
  scale_y_reverse(expand = expansion(mult = c(expand_mult, expand_mult))) +
  scale_colour_identity() +
  coord_fixed(ratio = 1.1) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = background_colour)
  )

#---- save ----

width <- max(base$col)

height <- max(base$row)

ratio <- (height * (coord_fixed_ratio + expand_mult + expand_mult)) /
  ((width * (1 + expand_mult + expand_mult)))

ggsave(
  "outputs/seed-9919_steps-b-m10x-m10y.png",
  #"outputs/cross-stitch.png",
  height = height_inches,
  width = height_inches / ratio,
  dpi = 300
)

#-------------------------------------------------------------------------------
