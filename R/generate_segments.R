
generate_segments <- function(x, y, range, density) {
  data <- data.frame(angle = runif(density, 0, 2 * pi)) %>%
    mutate(
      x = range * cos(angle)*3 + x,
      y = range * sin(angle)*3 + y
      ) %>%
    select(x, y)
  data2 <- data[sample(nrow(data)),]
  names(data) <- c("x1", "y1")
  names(data2) <- c("x2", "y2")
  data <- bind_cols(data, data2)
  data
}
