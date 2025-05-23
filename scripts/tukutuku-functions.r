
library(tidyverse)
library(glue)
library(scales)
library(cli)

# default colour palette ----------------------------------------------------------
#' only three colours are used
#' you can change what r b and w point to
#' and set the background colour

pal <- tibble(
  short = c('r', 'b', 'w'),
  col = c('red', 'black', 'white')
)

bg_col <- 'grey15'

# read pattern file -------------------------------------------------------

read_pattern <- function(name) {
  patt_raw <- tibble(lines = readLines(glue('patterns/{name}.txt'))) |> 
    mutate(
      section = case_when(lines %in% c('pattern', 'key') ~ lines)
    ) |> 
    fill(section) |> 
    filter(lines != section, lines != '') |> 
    print()

  key <- patt_raw |> 
    filter(section == 'key') |> 
    select(-section) |> 
    separate(lines, into = c('short', 'col'), sep = ' +') 

  pattern <- patt_raw |> 
    filter(section == 'pattern') |> 
    select(-section) |> 
    separate(lines, into = c('row', 'short'), sep = ' +') |> 
    filter(row >= 1) |> 
    group_by(row) |> 
    mutate(short = str_split(short, '')) |> 
    unnest_longer(short) |> 
    mutate(
      row = as.integer(row),
      col = row_number(),
      seed = 0,
      steps = name
    ) |> 
    rename(x = col, y = row)
  
  list(
    key = key,
    pattern = pattern
  )
  
}

taku_ara_list <- read_pattern('taku-ara')
taku_ara_pattern <- taku_ara_list$pattern
taku_ara_key <- taku_ara_list$key

# check pattern -----------------------------------------------------------

check_pattern <- function(pattern, key) {
  pattern |> 
    left_join(key, by = 'short') |> 
    mutate(marker = 'X') |> 
    ggplot(aes(x, y, colour = col, label = marker)) +
    geom_tile(fill = 'white', colour = 'black') +
    geom_text() +
    scale_colour_identity() +
    scale_x_continuous(expand = expansion(add = 1)) +
    scale_y_continuous(expand = expansion(add = 1), transform = transform_reverse()) +
    coord_fixed() +
    theme_void()
}

check_pattern(taku_ara_pattern, taku_ara_key)

# initial pattern function --------------------------------------------------

init_pattern <- function(w = 10, h = 10, seed = 9919) {
  
  set.seed(seed)
  
  tibble(
    x = (1:w) - 0.5,
    y = list((1:h) - 0.5),
    seed = seed,
    steps = 'b'
  ) |>
    unnest_longer(y) |>
    mutate(
      rand = runif(n()),
      short = case_when(
        rand <= 0.33 ~ 'r',
        rand <= 0.67 ~ 'w',
        TRUE ~ 'b'
      )
    ) |> 
    select(-rand)
  
}

rand_pattern <- init_pattern(7, 7) |> print()

check_pattern(rand_pattern, pal)

# doubling function -------------------------------------------------------

double <- function(pattern) {
  
  step <- 'd'
  cli_alert('step {step}')
  
  pattern |>
    mutate(
      x = x * 2,
      y = y * 2,
    ) |>
    cross_join(
      tibble(add_x = c(0, 0, 1, 1), add_y = c(0, 1, 0, 1) )
    ) |>
    mutate(
      x = x + add_x,
      y = y + add_y,
      steps = paste(steps, step, sep = '-')
    ) |>
    select(-add_x, -add_y)
}

double(rand_pattern) |> 
  check_pattern(pal)


# rotate to next colour combo function ------------------------------------

col_rotate <- function(short) {
  case_match(
    short,
    'w' ~ 'r',
    'r' ~ 'b',
    'b' ~ 'w'
  )
}

double(rand_pattern) |> 
  mutate(short = col_rotate(short)) |> 
  check_pattern(pal)

# stack three copies with rotated colours function ------------------------

stack_col <- function(pattern, space = FALSE, dir = x) {
  
  step <- paste0('c', as.integer(space), quo_name(quo({{ dir }})))
  cli_alert('step {step}')
  
  bind_rows(
    pattern, 
    mutate(
      pattern, 
      short = col_rotate(short), 
      {{ dir }} := {{ dir }} + max({{ dir }}) + 0.5 + space
    ),
    mutate(
      pattern, 
      short = col_rotate(col_rotate(short)), 
      {{ dir }} := {{ dir }} + 2 * (max({{ dir }}) + 0.5 + space)
    )
  ) |>
    mutate(    
      steps = paste(steps, step, sep = '-')
    )
}

stack_col(rand_pattern, dir = y) |> 
  check_pattern(pal)

# stack a mirror copy function --------------------------------------------

stack_mirror <- function(pattern, even = TRUE, space = FALSE, dir = x) {
  
  step <- paste0('m', as.integer(even), as.integer(space), quo_name(quo({{ dir }})))
  cli_alert('step {step}')
  
  bind_rows(
    pattern, 
    mutate(pattern, {{ dir }} := (max({{ dir }}) * 2) - {{ dir }} + (even + space))
  ) |>
    mutate(    
      steps = paste(steps, step, sep = '-')
    )
}

stack_mirror(rand_pattern) |>
  stack_mirror(TRUE, dir = y) |> 
  check_pattern(pal)

# create the tukutuku plots -----------------------------------------------

tukutuku <- function(
    pattern, 
    pal,
    save = TRUE, 
    save_width = 4.667,
    border = FALSE,
    border_col = 'white',
    bg_col = 'grey15',
    aj = 0.4
) {
  
  #pattern <- taku_ara_pattern
  #pal <- taku_ara_key  
  
  border_pattern <- pattern |> 
    filter(
      x == min(x) | x == max(x),
      y != 1
    ) |> 
    transmute(
      x = if_else(x == min(x), x - 1, x + 1), 
      y, 
      col = border_col
    )
  
  width <- max(pattern$x) - min(pattern$x) + 1 + (border * 2)
  height <- max(pattern$y) - min(pattern$y) + 1
  h_ratio = (height + 2) / (width + 2)
  
  lw <- 25 / width
  
  seed <- first(pattern$seed)
  steps <- first(pattern$steps)
  
  name_string <- glue('tukutuku_seed-{seed}_steps-{steps}_width-{width}_height-{height}')
  
  cli_alert('width = {width}')
  cli_alert('height = {height}')
  cli_alert('linewidth = {comma(lw, 0.01)}')
  cli_alert(name_string)
  
  p <- pattern |>
    left_join(pal, by = 'short') |> 
    ggplot(aes(colour = col)) +
    #geom_hline(yintercept = 0:10, colour = 'black', linetype = 1) +
    geom_segment(aes(x = x - aj, xend = x + aj, y = y - aj, yend = y + aj),
                 lineend = 'round', linewidth = lw) +
    geom_segment(aes(x = x + aj, xend = x - aj, y = y - aj, yend = y + aj),
                 lineend = 'round', linewidth = lw) +
    scale_colour_identity() +
    theme_void() +
    coord_fixed(ratio = 1.25) +
    scale_x_continuous(expand = expansion(add = 1)) +
    scale_y_continuous(expand = expansion(add = 1), transform = transform_reverse()) +
    theme(
      panel.background = element_rect(fill = bg_col)
    ) +
    guides(linewidth = 'none', colour = 'none', lineend = 'none')
  
  if(border) {
    p <- p + 
    geom_segment(data = border_pattern, aes(x = x + aj, xend = x - aj, y = y - aj - 1, yend = y + aj),
                 lineend = 'round', linewidth = lw) +
    geom_segment(data = border_pattern, aes(x = x - aj, xend = x + aj, y = y - aj - 1, yend = y + aj),
                 lineend = 'round', linewidth = lw)
  }
  
  if(save) {
    ggsave(
      paste0("outputs/", name_string, ".png"),
      plot = p,
      device = "png", 
      width = save_width, 
      height = h_ratio * save_width,
      dpi = 150, 
    )
  }
  
  return(p)    
}


taku_ara_pattern |> 
  stack_mirror(even = FALSE, dir = x) |> 
  #stack_mirror(even = TRUE, dir = y) |> 
  tukutuku(taku_ara_key, save = FALSE, border = FALSE)

#' need to adjust size for coord_fixed ratio 1.25

  
  