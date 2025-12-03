#' @name genart
#' @title Launch generative app
#' @author Nicolas Mangin
#' @description Launcher 
#' @importFrom shiny runApp
#' @export

genart <- function() {
  appFile <- base::paste0(base::path.package("Rtist"), "/myapp/genart.R")
  shiny::runApp(appFile, display.mode = "normal")
}
