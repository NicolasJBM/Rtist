#' @name rosas
#' @title Draw a rosas
#' @author Nicolas Mangin
#' @description Module 
#' @param range Numeric
#' @param density Numeric
#' @param colors Character vector
#' @param curv Numeric
#' @param size Numeric
#' @param alpha Numeric
#' @param background Character
#' @param limx Numeric
#' @param limy Numeric
#' @param coord Character. "equal", "polar" or "radial"
#' @return ggplot graph.
#' @importFrom dplyr mutate
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_polar
#' @importFrom ggplot2 coord_radial
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_void
#' @importFrom grDevices hsv
#' @export


rosas <- function(
    range = 0.5,
    density = 5000,
    colors = base::rep(c("yellow","orange","red","firebrick"), 2),
    curv = 0.5,
    size = 1,
    alpha = 0.01,
    background = "#330000",
    limx = 3,
    limy = 3,
    coord = "equal"
){
  
  angle <- NULL
  x1 <- NULL
  x2 <- NULL
  y1 <- NULL
  y2 <- NULL
  
  number <- base::length(colors)
  
  generate_segments <- function(x, y, range, density) {
    data <- base::data.frame(angle = stats::runif(density, 0, 2 * pi)) |>
      dplyr::mutate(
        x = range * base::cos(angle)*3 + x,
        y = range * base::sin(angle)*3 + y
      ) |>
      dplyr::select(x, y)
    data2 <- data[base::sample(base::nrow(data)),]
    base::names(data) <- c("x1", "y1")
    base::names(data2) <- c("x2", "y2")
    data <- dplyr::bind_cols(data, data2)
    data
  }
  
  data <- generate_segments(
    x = base::cos(0),
    y = base::sin(0),
    range = range,
    density = density
  ) |>
    dplyr::mutate(color = colors[[base::length(colors)]])
  
  for (i in 1:(number-1)){
    data <- dplyr::bind_rows(
      data,
      generate_segments(
        x = base::cos(i*pi/(number/2)),
        y = base::sin(i*pi/(number/2)),
        range = range,
        density = density
      ) |>
        dplyr::mutate(color = colors[[i]])
    )
  }
  
  data <- dplyr::filter(data, x1 != x2)
  
  rosas <- data |>
    ggplot2::ggplot(ggplot2::aes(x = x1, xend = x2, y = y1, yend = y2)) +
    ggplot2::geom_curve(color=data$color, alpha = alpha, linewidth = size, curvature = curv) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = background),
      plot.background = ggplot2::element_rect(fill = background)
    ) +
    ggplot2::xlim(-limx,limx) +
    ggplot2::ylim(-limy,limy)
  
  if (coord == "equal"){
    rosas <- rosas + ggplot2::coord_equal()
  } else if (coord == "polar"){
    rosas <- rosas + ggplot2::coord_polar()
  } else if (coord == "radial"){
    rosas <- rosas + ggplot2::coord_radial()
  }
  
  return(rosas)
}





