

generate_points <- function(formula, range, density) {
  df <- seq(from = -range/2, to = range/2, by = 1/density) %>%
    expand.grid(x_i = ., y_i = .) %>%
    dplyr::mutate(!!!formula)
  return(df)
}
