#' @name save_art
#' @title Select and load a intake
#' @author Nicolas Mangin
#' @description Module facilitating the loading of a classification intake.
#' @param art ggplot graph.
#' @param name Character.
#' @param format Character
#' @param folder Character
#' @param wdth Numeric
#' @param hght Numeric
#' @return A list containing the intake as a table and a json object
#' @importFrom ggplot2 ggsave
#' @importFrom sjPlot save_plot
#' @export


save_art <- function(
    art,
    name = "plot",
    format = ".png",
    folder = base::getwd(),
    wdth = 95,
    hght = 95
){
  
  path <- base::paste0(folder, "/", name, format)
  
  if (format == ".png"){
    ggplot2::ggsave(
      path,
      art,
      width = wdth,
      height = hght,
      units = "cm",
      dpi = "print"
    )
  } else {
    sjPlot::save_plot(
      path,
      fig = art,
      width=wdth,
      height=hght
    )
  }
  
}


