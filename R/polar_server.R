#' @name polar_server
#' @title Collect and process polar parameters
#' @author Nicolas Mangin
#' @description Module collecting the paraleters used to produce a polar distorsion.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @importFrom dplyr filter
#' @importFrom shiny NS
#' @importFrom shiny eventReactive
#' @importFrom shiny moduleServer
#' @importFrom shiny numericInput
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny renderPlot
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny selectInput
#' @importFrom shiny selectizeInput
#' @importFrom shinyFiles getVolumes
#' @importFrom shinyFiles parseDirPath
#' @importFrom shinyFiles shinyDirChoose
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_progress_line
#' @importFrom shinybusy update_modal_progress
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @export


polar_server <- function(id){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    name <- NULL
    
    volumes = shinyFiles::getVolumes()()
    shinyFiles::shinyDirChoose(input, "folder", roots = volumes, session = session)
    folder_selected <- shiny::eventReactive(input$folder, {
      shiny::req(input$folder)
      shinyFiles::parseDirPath(volumes, input$folder)
    })
    
    parameters <- shiny::reactive({
      if ("path" %in% base::names(input$folder)){
        Rtist::gather_polar_parameters(path = folder_selected())
      } else {
        tibble::tibble(
          name = "Panda_dans_Bambou",
          a = 1.5,
          b = 3,
          c = 2,
          d = 2.5,
          gamma = 3,
          range = 2.8,
          density = 650,
          x = 2,
          y = 1,
          seed = 20240713,
          alp = 0.1,
          sz = 0.46,
          shp = 16,
          cx = -1,
          ex = 1,
          cy = 1,
          ey = 1,
          ntl = 300,
          plt = "#FFFFFF #EEEEEE #DDDDDD #00FF33 #00AA66",
          bkgcol = "#1A2433",
          rotation = 340,
          coord = "polar"
        )
      }
    })
    
    output$select_polar <- shiny::renderUI({
      shiny::req(!base::is.null(parameters()))
      shiny::selectInput(
        ns("slctpolar"),
        "Select a pattern:",
        choices = parameters()$name,
        selected = parameters()$name[[1]]
      )
    })
    
    startparam <- shiny::reactive({
      shiny::req(!base::is.null(input$slctpolar))
      shiny::req(!base::is.null(parameters()))
      parameters() |>
        dplyr::filter(name == input$slctpolar)
    })
    
    output$shapeui <- shiny::renderUI({
      shiny::req(!base::is.null(startparam()))
      
      base::list(
        shiny::numericInput(ns("seed"), "seed", value = startparam()$seed[[1]], width = "100%"),
        shiny::fluidRow(
          shiny::column(3, shiny::numericInput(ns("a"), "a (0, 10)", value = startparam()$a[[1]], width = "100%")),
          shiny::column(3, shiny::numericInput(ns("b"), "b (-5, 5)", value = startparam()$b[[1]], width = "100%")),
          shiny::column(3, shiny::numericInput(ns("c"), "c (0, 10)", value = startparam()$c[[1]], width = "100%")),
          shiny::column(3, shiny::numericInput(ns("d"), "d (0, 10)", value = startparam()$d[[1]], width = "100%"))
        ),
        shiny::fluidRow(
          shiny::column(4, shiny::numericInput(ns("gamma"), "gamma (0, 10)", value = startparam()$gamma[[1]], width = "100%")),
          shiny::column(4, shiny::numericInput(ns("range"), "range (1, 10)", value = startparam()$range[[1]], width = "100%")),
          shiny::column(4, shiny::numericInput(ns("density"), "density (10, 1000)", value = startparam()$density[[1]], width = "100%"))
        ),
        shiny::fluidRow(
          shiny::column(6, shiny::numericInput(ns("x"), "x (0.1, 10)", value = startparam()$x[[1]], width = "100%")),
          shiny::column(6, shiny::numericInput(ns("y"), "y (0.1, 10)", value = startparam()$y[[1]], width = "100%"))
        ),
        shiny::fluidRow(
          shiny::column(4, shiny::numericInput(ns("alp"), "alpha (0.01, 1)", value = startparam()$alp[[1]], width = "100%")),
          shiny::column(4, shiny::numericInput(ns("sz"), "size (0.01, 10)", value = startparam()$sz[[1]], width = "100%")),
          shiny::column(4, shiny::numericInput(ns("shp"), "shape", value = startparam()$shp[[1]], width = "100%"))
        ),
        shiny::fluidRow(
          shiny::column(6, shiny::numericInput(ns("rotation"), "rotation (0, 360)", value = startparam()$rotation[[1]], width = "100%")),
          shiny::column(6, shiny::selectInput(ns("coord"), "Coordinates", choices = c("cartesian","polar","radial"), selected = "polar"))
        )
        
      )
      
    })
    
    
    output$colorui <- shiny::renderUI({
      shiny::req(!base::is.null(startparam()))
      
      base::list(
        shiny::fluidRow(
          shiny::column(3, shiny::numericInput(ns("cx"), "coefficient x", value = startparam()$cx[[1]], width = "100%")),
          shiny::column(3, shiny::numericInput(ns("ex"), "exponent x", value = startparam()$ex[[1]], width = "100%")),
          shiny::column(3, shiny::numericInput(ns("cy"), "coefficient y", value = startparam()$cy[[1]], width = "100%")),
          shiny::column(3, shiny::numericInput(ns("ey"), "exponent y", value = startparam()$ey[[1]], width = "100%"))
        ),
        shiny::fluidRow(
          shiny::column(4, shiny::numericInput(ns("ntl"), "Parameter ntl", min = 50, max = 1000, step = 100, value = 300, width = "100%")),
          shiny::column(
            4,
            shiny::selectizeInput(
              ns("plt"), label = "Drawing colors:",
              choices = stringr::str_split(startparam()$plt, pattern = " ", simplify = TRUE),
              selected = stringr::str_split(startparam()$plt, pattern = " ", simplify = TRUE),
              multiple = TRUE, options=base::list(create=TRUE)
            )
          ),
          shiny::column(
            4,
            shiny::selectizeInput(
              ns("bkgcol"), label = "Background color:",
              choices = startparam()$bkgcol,
              selected = startparam()$bkgcol,
              multiple = FALSE, options=base::list(create=TRUE)
            )
          )
        )
        
      )
      
    })
    
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
      
      if (input$name == ""){
        artname <- "genart"
      } else {
        artname <- input$name
      }
      
      if ("path" %in% base::names(input$folder)){
        artfolder <- folder_selected()
      } else {
        artfolder <- base::getwd()
        base::print(base::paste0("Files will be saved in ", artfolder))
      }
      
      Rtist::save_art(
        art,
        name = artname,
        format = input$format,
        folder = artfolder,
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
        utils::write.csv(base::paste0(artfolder, "/polar_", artname, ".csv"), row.names = FALSE)
      
      shinybusy::remove_modal_spinner()
      shinyalert::shinyalert(
        title = "Art saved!",
        text = "You should now see it in the chosen folder.",
        type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
      )
    })
    
    shiny::observeEvent(input$openfolder, {
      if (base::dir.exists(folder_selected())){
        if (.Platform['OS.type'] == "windows"){
          shell.exec(folder_selected())
        } else {
          system2("open", folder_selected())
        }
      }
    })
    
  })
  
}

