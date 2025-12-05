#' @name color_points
#' @title Define colors based on coordinates
#' @author Nicolas Mangin
#' @description Function 
#' @param x Numeric
#' @param y Numeric
#' @param cx Numeric
#' @param ex Numeric
#' @param cy Numeric
#' @param ey range Numeric
#' @param ntl Numeric
#' @param plt Character vector
#' @return ggplot color
#' @importFrom dplyr ntile
#' @importFrom grDevices colorRampPalette
#' @export



color_points <- function(
    x,
    y,
    cx = 1,
    ex = 1,
    cy = 0,
    ey = 1,
    ntl = 300,
    plt = c("#FFFAAA","#EECC00","#DD5500","#993300","#550000")
){
  col <- dplyr::ntile(cx*x^ex+cy*y^ey,ntl)
  colfunc <- grDevices::colorRampPalette(plt)
  palette <- colfunc(base::max(col))
  palette[col]
}
