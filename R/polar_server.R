#' @name polar_server
#' @title Collect and process polar parameters
#' @author Nicolas Mangin
#' @description Module collecting the paraleters used to produce a polar distorsion.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @importFrom shiny NS
#' @importFrom shiny eventReactive
#' @importFrom shiny moduleServer
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny renderPlot
#' @importFrom shiny req
#' @importFrom shinyFiles getVolumes
#' @importFrom shinyFiles shinyDirChoose
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_progress_line
#' @importFrom shinybusy update_modal_progress
#' @importFrom tibble tibble
#' @export


polar_server <- function(id){
  
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    polplot <- shiny::eventReactive(input$draw, {
      Rtist::polar_distorsion(
        a = input$a,
        b = input$b,
        c = input$c,
        d = input$d,
        gamma = input$gamma,
        range = input$range,
        density = input$density,
        x = input$x,
        y = input$y,
        seed = input$seed,
        alp = input$alp,
        sz = input$sz,
        shp = input$shp,
        cx = input$cx,
        ex = input$ex,
        cy = input$cy,
        ey = input$ey,
        ntl = input$ntl,
        plt = input$plt,
        bkgcol = input$bkgcol,
        rotation = input$rotation,
        coord = input$coord
      )
    })
    
    output$polardist <- shiny::renderPlot({
      shiny::req(!base::is.null(polplot()))
      polplot()
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
      
      art <- Rtist::polar_distorsion(
        a = input$a,
        b = input$b,
        c = input$c,
        d = input$d,
        gamma = input$gamma,
        range = input$range,
        density = input$density,
        x = input$x,
        y = input$y,
        seed = input$seed,
        alp = input$alp,
        sz = input$sz,
        shp = input$shp,
        cx = input$cx,
        ex = input$ex,
        cy = input$cy,
        ey = input$ey,
        ntl = input$ntl,
        plt = base::as.vector(stringr::str_split(input$plt, pattern = " ", simplify = TRUE)),
        bkgcol = input$bkgcol,
        rotation = input$rotation,
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
        a = input$a,
        b = input$b,
        c = input$c,
        d = input$d,
        gamma = input$gamma,
        range = input$range,
        density = input$density,
        x = input$x,
        y = input$y,
        seed = input$seed,
        alp = input$alp,
        sz = input$sz,
        shp = input$shp,
        cx = input$cx,
        ex = input$ex,
        cy = input$cy,
        ey = input$ey,
        ntl = input$ntl,
        plt = base::paste(input$plt, collapse = " "),
        bkgcol = input$bkgcol,
        rotation = input$rotation,
        coord = input$coord
      ) |>
        utils::write.csv(base::paste0(folder_selected(), "/polar_", input$name, ".csv"), row.names = FALSE)
      
      shinybusy::remove_modal_spinner()
      shinyalert::shinyalert(
        title = "Art saved!",
        text = "You should now see it in the chosen folder.",
        type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
      )
    })
    
  })
  
}

