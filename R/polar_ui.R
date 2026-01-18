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
#' @importFrom shinycssloaders withSpinner
#' @export


polar_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        1,
        shinyFiles::shinyDirButton(ns('folder'), 'Select a folder', 'Please select a folder', FALSE)
      ),
      shiny::column(
        2,
        shiny::uiOutput(ns("select_polar"))
      ),
      shiny::column(
        1,
        shiny::actionButton(
          ns("draw"), "Draw", icon = shiny::icon("pen"),
          style = "background-color:#330066;color:#FFF;width:100%;margin-top:25px;"
        )
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
        1,
        shiny::actionButton(
          ns("print"), "Print", icon = shiny::icon("pen"),
          style = "background-color:#660033;color:#FFF;width:100%;margin-top:25px;"
        )
      ),
      shiny::column(
        1,
        shiny::actionButton(
          ns("openfolder"), "Open folder", icon = shiny::icon("folder-open"),
          style = "background-color:#003366;color:#FFF;width:100%;margin-top:25px;"
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::tags$h3("Shape"),
        shiny::uiOutput(ns("shapeui"))
      ),
      shiny::column(
        6,
        shinycssloaders::withSpinner(shiny::plotOutput(ns("polardist"), height = "800px", width = "800px"), type = 5)
      ),
      shiny::column(
        3,
        shiny::tags$h3("Colors"),
        shiny::uiOutput(ns("colorui")),
        shinyWidgets::colorPickr(ns("searchcol"), "Color picker:", selected = "#1A2433FF")
      )
    )
  )
}
