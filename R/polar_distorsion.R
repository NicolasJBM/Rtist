#' @name polar_distorsion
#' @title Draw a polar distorsion
#' @author Nicolas Mangin
#' @description Module 
#' @param a Numeric
#' @param b Numeric
#' @param c Numeric
#' @param d Numeric
#' @param gamma Numeric
#' @param range Numeric
#' @param density Numeric
#' @param x Numeric
#' @param y Numeric
#' @param seed Numeric
#' @param alp Numeric
#' @param sz Numeric
#' @param shp Numeric
#' @param drwcol Character
#' @param bkgcol Character
#' @param rotation Numeric. angle of rotation
#' @param coord Character. nothing, "polar" or "radial"
#' @return ggplot graph.
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_polar
#' @importFrom ggplot2 coord_radial
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_void
#' @importFrom grDevices hsv
#' @importFrom dplyr mutate
#' @importFrom SpatialGraph rotation
#' @export


#drwcol <- "#FFF333" 
#bkgcol <- "#770000"
#drwcol <- "#33FFFF"
#bkgcol <- "#000044"


polar_distorsion <- function(
    a = 2,
    b = 2,
    c = 3,
    d = 2,
    gamma = 2,
    range = 5,
    density = 50,
    x = 1,
    y = -1,
    seed = 20240713,
    alp = 0.05,
    sz = 0.75,
    shp = 16,
    drwcol = grDevices::hsv(0.4, 0.4, 1),
    bkgcol = grDevices::hsv(0.6, 0.5, 0.2,1),
    rotation = 0,
    coord = "polar"
){
  
  x_i <- NULL
  y_i <- NULL
  
  my_formula <- base::list(
    x = base::quote(stats::rgamma(1, gamma) * x_i^a - base::sin(y_i^b)),
    y = base::quote(stats::rgamma(1, gamma) * y_i^c - base::cos(x_i^d))
  )
  
  generate_points <- function(my_formula, range, density, y, x, seed) {
    base::set.seed(seed)
    x <- seq(from = -range/2, to = range/2, by = 1/density)*x
    y <- seq(from = -range/2, to = range/2, by = 1/density)*y
    df <-   base::expand.grid(x_i = x, y_i = y) |>
      dplyr::mutate(!!!my_formula)
    return(df)
  }
  
  df <- generate_points(my_formula, range = range, density = density, x, y, seed)
  
  df <- df |>
    dplyr::select(x, y) |>
    base::as.matrix() |>
    SpatialGraph::rotation(rotation)
  
  nest <- df |>
    ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(alpha = alp, size = sz, shape = shp, color = drwcol) +
    ggplot2::theme_void()
  
  if (coord == "polar"){
    nest <- nest +
      ggplot2::coord_polar()
  } else if (coord == "radial"){
    nest <- nest +
      ggplot2::coord_radial()
  }
    
  nest +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = bkgcol),
      plot.background = ggplot2::element_rect(fill = bkgcol)
    )
  
}

