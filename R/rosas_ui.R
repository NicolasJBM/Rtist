#' @name polar_ui
#' @title Collect and process polar parameters
#' @author Nicolas Mangin
#' @description Module collecting the paraleters used to produce a polar distorsion.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny mainPanel
#' @importFrom shiny numericInput
#' @importFrom shiny plotOutput
#' @importFrom shiny selectInput
#' @importFrom shiny sidebarLayout
#' @importFrom shiny sidebarPanel
#' @importFrom shiny sliderInput
#' @importFrom shiny textInput
#' @importFrom shinyFiles shinyDirButton
#' @importFrom shinyWidgets colorPickr
#' @export


polar_ui <- function(id){
  ns <- shiny::NS(id)
  
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::sliderInput(ns("a"), "Parameter a", min = -10, max = 10, step = 0.1, value = 2, width = "100%"),
      shiny::sliderInput(ns("b"), "Parameter b", min = -10, max = 10, step = 0.1, value = 2, width = "100%"),
      shiny::sliderInput(ns("c"), "Parameter c", min = -10, max = 10, step = 0.1, value = 3, width = "100%"),
      shiny::sliderInput(ns("d"), "Parameter d", min = -10, max = 10, step = 0.1, value = 2, width = "100%"),
      shiny::sliderInput(ns("gamma"), "gamma", min = 1, max = 10, step = 0.1, value = 2, width = "100%"),
      shiny::sliderInput(ns("range"), "range", min = 1, max = 10, step = 1, value = 5, width = "100%"),
      shiny::sliderInput(ns("density"), "Density", min = 20, max = 1000, step = 1, value = 20, width = "100%"),
      shiny::numericInput(ns("x"), "Parameter x", min = -10, max = 10, step = 0.5, value = 1, width = "100%"),
      shiny::numericInput(ns("y"), "Parameter y", min = -10, max = 10, step = 0.5, value = -1, width = "100%"),
      shiny::numericInput(ns("seed"), "seed", min = 1, step = 1, value = 20240713, width = "100%"),
      shiny::sliderInput(ns("alp"), "alpha", min = 0.01, max = 1, step = 0.01, value = 0.05, width = "100%"),
      shiny::sliderInput(ns("sz"), "Size", min = 0.01, max = 5, step = 0.01, value = 0.75, width = "100%"),
      shiny::numericInput(ns("shp"), "Shape", min = 1, max = 25, step = 1, value = 16, width = "100%"),
      shinyWidgets::colorPickr(ns("drwcol"), "Dot color", selected = "#99FFC2"),
      shinyWidgets::colorPickr(ns("bkgcol"), "Background color", selected = "#1A2433FF"),
      shiny::selectInput(ns("coord"), "Coordinates", choices = c("cartesian","polar","radial"), selected = "polar")
    ),
    shiny::mainPanel(
      shiny::fluidRow(
        shiny::column(
          2,
          shiny::actionButton(
            ns("draw"), "Draw", icon = shiny::icon("pen"),
            style = "background-color:#330066;color:#FFF;width:100%;margin-top:25px;"
          )
        ),
        shiny::column(
          2,
          shinyFiles::shinyDirButton(ns('folder'), 'Select a folder', 'Please select a folder', FALSE)
        ),
        shiny::column(
          2,
          shiny::textInput(ns("name"), "Name:")
        ),
        shiny::column(
          2,
          shiny::selectInput(ns("format"), "Format", choices = c(".png",".svg"), selected = ".png")
        ),
        shiny::column(
          1,
          shiny::numericInput(ns("wdth"), "Width", min = 10, max = 1000, step = 1, value = 95, width = "100%")
        ),
        shiny::column(
          1,
          shiny::numericInput(ns("hght"), "Height", min = 10, max = 1000, step = 1, value = 95, width = "100%")
        ),
        shiny::column(
          2,
          shiny::actionButton(
            ns("print"), "Print", icon = shiny::icon("pen"),
            style = "background-color:#660033;color:#FFF;width:100%;margin-top:25px;"
          )
        )
      ),
      shiny::plotOutput(ns("polardist"), height = "800px", width = "800px")
    )
  )
}
