#-------------------------------------------------------------------------------
#' tukutuku-functions.r
#-------------------------------------------------------------------------------
#' jo dudding
#' May 2025
#' functions to adjust and print tututuku patterns
#-------------------------------------------------------------------------------

library(tidyverse)
library(glue)
library(scales)
library(cli)

# default colour palette ----------------------------------------------------------
#' only three colours are used for the random patterns
#' you can change what r, b and w point to

pal <- tibble(
  short = c("r", "b", "w"),
  col = c("red", "black", "white")
)

# read pattern file -------------------------------------------------------

read_pattern <- function(name) {
  patt_raw <- tibble(lines = readLines(glue("patterns/{name}.txt"))) |>
    mutate(
      section = case_when(lines %in% c("pattern", "key") ~ lines)
    ) |>
    fill(section) |>
    filter(lines != section, lines != "") |>
    print()

  key <- patt_raw |>
    filter(section == "key") |>
    select(-section) |>
    separate(lines, into = c("short", "col"), sep = " +")

  pattern <- patt_raw |>
    filter(section == "pattern") |>
    select(-section) |>
    separate(lines, into = c("row", "short"), sep = " +") |>
    filter(row >= 1) |>
    group_by(row) |>
    mutate(short = str_split(short, "")) |>
    unnest_longer(short) |>
    mutate(
      row = as.integer(row),
      col = row_number(),
      seed = 0,
      steps = name,
      height = 1,
      width = 1
    ) |>
    rename(x = col, y = row)

  list(
    key = key,
    pattern = pattern
  )
}

# taku_ara_list <- read_pattern('taku-ara')
# taku_ara_pattern <- taku_ara_list$pattern
# taku_ara_key <- taku_ara_list$key

# check pattern -----------------------------------------------------------

check_pattern <- function(pattern, key) {
  pattern |>
    left_join(key, by = "short") |>
    mutate(marker = "X") |>
    ggplot(aes(x, y, colour = col, label = marker)) +
    geom_tile(fill = "white", colour = "black") +
    geom_text() +
    scale_colour_identity() +
    scale_x_continuous(expand = expansion(add = 1)) +
    scale_y_continuous(expand = expansion(add = 1), 
      transform = transform_reverse()) +
    coord_fixed() +
    theme_void()
}

# check_pattern(taku_ara_pattern, taku_ara_key)

# initial pattern function --------------------------------------------------

init_pattern <- function(w = 10, h = 10, seed = 9919) {
  set.seed(seed)

  tibble(
    x = 1:w,
    y = list(1:h),
    height = 1,
    width = 1,
    seed = seed,
    steps = "b"
  ) |>
    unnest_longer(y) |>
    mutate(
      rand = runif(n()),
      short = case_when(
        rand <= 0.33 ~ "r",
        rand <= 0.67 ~ "w",
        TRUE ~ "b"
      )
    ) |>
    select(-rand)
}

# rand_pattern <- init_pattern(7, 7) |> print()

# check_pattern(rand_pattern, pal)

# doubling function -------------------------------------------------------
#' turns each stitch into four in 2x2 format

double <- function(pattern) {
  step <- "d"
  cli_alert("step {step}")

  pattern |>
    mutate(
      x = x * 2,
      y = y * 2,
    ) |>
    cross_join(
      tibble(add_x = c(0, 0, 1, 1), add_y = c(0, 1, 0, 1))
    ) |>
    mutate(
      x = x + add_x,
      y = y + add_y,
      steps = paste(steps, step, sep = "-")
    ) |>
    select(-add_x, -add_y)
}

# double(rand_pattern) |>
#  check_pattern(pal)


# rotate to next colour combo function ------------------------------------

col_rotate <- function(short) {
  case_match(
    short,
    "w" ~ "r",
    "r" ~ "b",
    "b" ~ "w"
  )
}

# double(rand_pattern) |>
#  mutate(short = col_rotate(short)) |>
#  check_pattern(pal)

# stack three copies with rotated colours function ------------------------

stack_col <- function(pattern, space = 0, dir = x) {
  step <- paste0("c", as.integer(space), quo_name(quo({{ dir }})))
  cli_alert("step {step}")

  bind_rows(
    pattern,
    mutate(
      pattern,
      short = col_rotate(short),
      {{ dir }} := {{ dir }} + max({{ dir }}) + space
    ),
    mutate(
      pattern,
      short = col_rotate(col_rotate(short)),
      {{ dir }} := {{ dir }} + 2 * (max({{ dir }}) + space)
    )
  ) |>
    mutate(
      steps = paste(steps, step, sep = "-")
    )
}

# stack_col(rand_pattern, dir = y) |>
#  check_pattern(pal)

# stack a mirror copy function --------------------------------------------
# TODO check this is working correctly for both directions

stack_mirror <- function(pattern, even = TRUE, space = FALSE, dir = x) {
  step <- paste0("m", as.integer(even), as.integer(space), quo_name(quo({{ dir }})))
  cli_alert("step {step}")

  bind_rows(
    pattern,
    mutate(pattern, {{ dir }} := (max({{ dir }}) * 2) - {{ dir }} + (even + space))
  ) |>
    mutate(
      steps = paste(steps, step, sep = "-")
    )
}

# stack_mirror(rand_pattern) |>
#  stack_mirror(FALSE, dir = y) |>
#  check_pattern(pal)


# add binding -------------------------------------------------------------
#' add a binding stitch to the left and right

add_binding <- function(pattern, colour_short = "b") {
  bind_rows(
    pattern |> 
      mutate(x = x + 1),
    crossing(
      x = c(1, max(pattern$x) + 2),
      y = 1:(max(pattern$y) - 1)
    ) |> 
      mutate(
        seed = first(pattern$seed),
        steps = first(pattern$steps),
        height = 2,
        width = 1,
        short = colour_short
      )
    )
}

#stack_mirror(rand_pattern) |>
#  add_binding() |>
#  check_pattern(pal)

# create tukutuku ---------------------------------------------------------

create_tukutuku <- function(
    base,
    padding = 0.3,
    line_width = 1.5,
    background_colour = "grey10",
    height_inches = 6,
    coord_fixed_ratio = 1.1,
    expand_mult = 0.05,
    save = TRUE) {
  #---- split out the / direction stitches and set start and end points ----

  base_slash <- base |>
    filter(type %in% c("X", "/")) |>
    mutate(
      border_col = colorspace::darken(colour, 0.3),
      xend = (x + width / 2) + (width - padding) / 2,
      x = (x + width / 2) - (width - padding) / 2,
      yend = (y + height / 2) + (height - padding) / 2,
      y = (y + height / 2) - (height - padding) / 2
    )

  #---- split out the \ (<) direction stitches and set start and end points ----

  base_backslash <- base |>
    filter(type %in% c("X", "<")) |>
    mutate(
      border_col = colorspace::darken(colour, 0.3),
      xend = (x + width / 2) + (width - padding) / 2,
      x = (x + width / 2) - (width - padding) / 2,
      yend = (y + height / 2) - (height - padding) / 2,
      y = (y + height / 2) + (height - padding) / 2
    )

  #---- create the chart ----

  p <- ggplot() +
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

  if (save) {
    width <- max(base$col)

    height <- max(base$row)

    ratio <- (height * (coord_fixed_ratio + expand_mult + expand_mult)) /
      ((width * (1 + expand_mult + expand_mult)))

    seed <- first(base$seed)
    steps <- first(base$steps)

    name_string <- glue("tukutuku_seed-{seed}_steps-{steps}_width-{width}_height-{height}")
    name_path <- paste0("outputs/", name_string, ".png")

    ggsave(
      name_path,
      height = height_inches,
      width = height_inches / ratio,
      dpi = 300
    )

    cli_alert("{.file {name_path}} created")
  }

  return(p)
}
