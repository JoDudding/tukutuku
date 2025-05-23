

source('scripts/tukutuku-functions.r')

health_list <- read_pattern('teal-health')
health_pattern <- health_list$pattern
#health_key <- health_list$key
health_key <- tibble(
  short = c('t', 'n', '1', '2', 'w', '3', '4'),
  col = c('Turquoise 4', 'Chocolate 4', 'Chocolate 4', 'Chocolate 4', 'Bisque', 'Bisque', 'Bisque')
)



check_pattern(health_pattern, health_key)

health_pattern |> 
  #stack_mirror(even = FALSE, dir = x) |> 
  #stack_mirror(even = TRUE, dir = y) |> 
  tukutuku(
    health_key, 
    save = TRUE, 
    save_width = 10,
    border = TRUE, 
    border_col = 'Chocolate 4',
    bg_col <- 'Navy Blue'
    #bg_col <- 'Midnight Blue'
  )
