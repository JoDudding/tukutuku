#-------------------------------------------------------------------------------
#' teal-health.r
#-------------------------------------------------------------------------------
#' jo dudding
#' May 2025
#' use health pattern to create tukutuku
#-------------------------------------------------------------------------------

source("scripts/tukutuku-functions.r")

#--- read pattern ---

health_list <- read_pattern("teal-health")

#--- adjust the key ---

health_key <- health_list$key |>
  mutate(
    colour = case_match(
      colour,
      "turquoise" ~ "Turquoise 4",
      "navy" ~ "Chocolate 4",
      "white" ~ "Bisque"
    )
  )

#--- check the pattern ---

check_pattern(health_pattern, health_key)

#--- create the tukutuku panel ---

health_list$pattern |>
  # stack_mirror(even = FALSE, dir = x) |>
  # stack_mirror(even = TRUE, dir = y) |>
  # left_join(health_list$key, by = "short") |>
  left_join(health_key, by = "short") |>
  create_tukutuku(line_width = 1)

#-------------------------------------------------------------------------------
