#' @name rosas_server
#' @title Collect and process polar parameters
#' @author Nicolas Mangin
#' @description Module collecting the paraleters used to produce a polar distorsion.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @importFrom shiny NS
#' @importFrom shiny eventReactive
#' @importFrom shiny moduleServer
#' @importFrom shiny observeEvent
#' @importFrom shiny renderPlot
#' @importFrom shiny req
#' @importFrom shinyFiles getVolumes
#' @importFrom shinyFiles parseDirPath
#' @importFrom shinyFiles shinyDirChoose
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_progress_line
#' @importFrom shinybusy update_modal_progress
#' @importFrom tibble tibble
#' @importFrom stringr str_split
#' @export


rosas_server <- function(id){
  
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    rosasplot <- shiny::eventReactive(input$draw, {
      Rtist::rosas(
        range = input$range,
        density = input$density,
        colors = base::as.vector(stringr::str_split(input$colors, pattern = " ", simplify = TRUE)),
        curv = input$curv,
        size = input$size,
        alpha = input$alpha,
        background = input$background,
        limx = input$limx,
        limy = input$limy,
        coord = input$coord
      )
    })
    
    output$rosas <- shiny::renderPlot({
      shiny::req(!base::is.null(rosasplot()))
      rosasplot()
    })
    
    volumes = shinyFiles::getVolumes()()
    shinyFiles::shinyDirChoose(input, "folder", roots = volumes, session = session)
    folder_selected <- shiny::eventReactive(input$folder, {
      shiny::req(input$folder)
      shinyFiles::parseDirPath(volumes, input$folder)
    })
    
    shiny::observeEvent(input$print, {
      
      shinybusy::show_modal_progress_line(
        value = 1/3, text = "Generate art"
      )
      
      art <- Rtist::rosas(
        range = input$range,
        density = input$density,
        colors = base::as.vector(stringr::str_split(input$colors, pattern = " ", simplify = TRUE)),
        curv = input$curv,
        size = input$size,
        alpha = input$alpha,
        background = input$background,
        limx = input$limx,
        limy = input$limy,
        coord = input$coord
      )
      
      shinybusy::update_modal_progress(
        value = 2/3,
        text = "Save art"
      )
      
      Rtist::save_art(
        art,
        name = input$name,
        format = input$format,
        folder = folder_selected(),
        wdth = input$wdth,
        hght = input$hght
      )
      
      shinybusy::update_modal_progress(
        value = 3/3,
        text = "Save parameters"
      )
      
      tibble::tibble(
        range = input$range,
        density = input$density,
        colors = input$colors,
        curv = input$curv,
        size = input$size,
        alpha = input$alpha,
        background = input$background,
        limx = input$limx,
        limy = input$limy,
        coord = input$coord
      ) |>
        utils::write.csv(base::paste0(folder_selected(), "/rosas_", input$name, ".csv"), row.names = FALSE)
      
      shinybusy::remove_modal_spinner()
      shinyalert::shinyalert(
        title = "Art saved!",
        text = "You should now see it in the chosen folder.",
        type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
      )
    })
    
  })
  
}

