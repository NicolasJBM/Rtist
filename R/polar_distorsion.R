



my_formula <- list(
  x = quote(runif(1, -1, 1) * x_i^1 - sin(y_i^2)),
  y = quote(runif(1, -1, 1) * y_i^2 - cos(x_i^5))
)

df <- generate_points(my_formula, range = 6, density = 150)

nest <- df %>%
  ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_point(alpha = 0.07, size = 0.75, shape = 20, color =  hsv(0.4, 0.4, 1)) +
  ggplot2::theme_void() +
  ggplot2::coord_polar() +
  ggplot2::theme(
    panel.background = element_rect(fill = hsv(0.6, 0.5, 0.2,1)),
    plot.background = element_rect(fill = hsv(0.6, 0.5, 0.2,1))
  )


ggsave("~/Downloads/nest_tp.png", nest, width = 100, height = 100, units = "cm", dpi = "print") #, bg = "transparent")
