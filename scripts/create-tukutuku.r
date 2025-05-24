#-------------------------------------------------------------------------------
#' create-tukutuku.r
#-------------------------------------------------------------------------------
#' jo dudding
#' May 2025
#' create different tukutuku panels using the functions created
#-------------------------------------------------------------------------------

library(patchwork)

#--- read tukutuku functions ---

source("scripts/tukutuku-functions.r")

# using random pattern generator ------------------------------------------

#--- random default ---

init_pattern() |> 
  mutate(
    colour = case_match(
      short,
      'r' ~ '#a01e1e',
      'b' ~ 'grey5',
      'w' ~ 'grey95'
    )
  ) |>
  create_tukutuku()

#--- random smaller with mirroring ---

init_pattern(7, 7) |> 
  stack_mirror() |>
  stack_mirror(TRUE, dir = y) |>
  mutate(
    colour = case_match(
      short,
      'r' ~ '#a01e1e',
      'b' ~ 'grey5',
      'w' ~ 'grey95'
    )
  ) |>
  create_tukutuku()

#--- same steps with different seeds ---

p1 <- init_pattern(6, 9, seed = 656) |> 
  stack_mirror() |>
  stack_mirror(TRUE, dir = y) |>
  mutate(
    colour = case_match(
      short,
      'r' ~ '#a01e1e',
      'b' ~ 'grey5',
      'w' ~ 'grey95'
    )
  ) |>
  create_tukutuku()

p2 <- init_pattern(6, 9, seed = 657) |> 
  stack_mirror() |>
  stack_mirror(TRUE, dir = y) |>
  mutate(
    colour = case_match(
      short,
      'r' ~ '#a01e1e',
      'b' ~ 'grey5',
      'w' ~ 'grey95'
    )
  ) |>
  create_tukutuku()

p3 <- init_pattern(6, 9, seed = 658) |> 
  stack_mirror() |>
  stack_mirror(TRUE, dir = y) |>
  mutate(
    colour = case_match(
      short,
      'r' ~ '#a01e1e',
      'b' ~ 'grey5',
      'w' ~ 'grey95'
    )
  ) |>
  create_tukutuku()

p1 + p2 + p3


# using patterns ----------------------------------------------------------

#--- taku ara ---

taku_ara_list <- read_pattern('taku-ara')

taku_ara_list$pattern |>
  stack_mirror(even = FALSE, dir = x) |>
  #stack_mirror(even = TRUE, dir = y) |> # not working
  left_join(taku_ara_list$key, by = "short") |>
  create_tukutuku()

#--- pātikitiki ---

patikitiki_list <- read_pattern('patikitiki')

patikitiki_list$pattern |>
  stack_mirror(even = FALSE, dir = x) |>
  stack_mirror(even = TRUE, dir = y) |>
  stack_col(dir = x) |> # y version not working
  left_join(patikitiki_list$key, by = "short") |>
  create_tukutuku(padding = 0.2, line_width = 2.5)

#--- poutama ---

poutama_list <- read_pattern('poutama')

poutama_list$pattern |>
  stack_mirror(even = TRUE, dir = x) |>
  #stack_mirror(even = TRUE, dir = y) |>
  #stack_col(dir = y) |> # y version not working
  left_join(poutama_list$key, by = "short") |>
  create_tukutuku(line_width = 1)

#--- niho taniwha ---

niho_taniwha <- read_pattern('niho-taniwha')

niho_taniwha$pattern |>
  stack_mirror(even = TRUE, dir = x) |>
  #stack_col(dir = y) |> # y version not working
  left_join(niho_taniwha$key, by = "short") |>
  create_tukutuku(line_width = 1)

#-------------------------------------------------------------------------------
