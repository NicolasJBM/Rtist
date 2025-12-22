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


flame_trees <- function(
    
){
  
  shades <- c("#330000", "#661100", "#FF7700", "#DDD000")
  
  colors <- c("#001100","#003322","#005533","#006644")
  
  # data structure defining the trees
  flametree::flametree_grow(
    seed = 20240713,
    time = 10, 
    scale = c(0.6, 0.8, 0.9),
    angle = c(-20, 0, 20),
    split = 3,
    trees = 4,
    seg_col = flametree::spark_linear(tree = 2, time = 1),
    seg_wid = flametree::spark_decay(time = 0.3, multiplier = 5, constant = 0.1),
    shift_x = flametree::spark_random(multiplier = 3),
    shift_y = flametree::spark_random(multiplier = 1)
  ) |>
    flametree::flametree_plot(
      background = "#000033",
      palette = colors,
      style = "plain" # native
    )
  
  
  
  return(rosas)
}





