genart <- shiny::shinyApp(
  
  # INTERFACE ##################################################################
  
  ui = shinydashboardPlus::dashboardPage(
    
    options = base::list(sidebarExpandOnHover = TRUE),
    
    # HEADER ###################################################################
    
    header = shinydashboardPlus::dashboardHeader(
      fixed = FALSE,
      leftUi = shiny::tagList(
        
        shiny::icon("creative-commons"),
        shiny::icon("creative-commons-by"),
        shiny::icon("creative-commons-sa"),
        
        shiny::tags$button(
          id = "exit", type = "button", class = "btn action-button",
          onclick = "setTimeout(function(){window.close();},100);",
          style = "background-color:#660033;color:#FFF;width:150px;border:0px;",
          shiny::icon("power-off"),
          shiny::span("Exit", title = "Exit the application without saving your unsaved work.")
        )
        
      )
    ),
    
    # SIDEBAR ##################################################################
    
    # Menus are only displayed if the corresponding header switch is TRUE
    sidebar = shinydashboardPlus::dashboardSidebar(
      minified = TRUE, collapsed = TRUE, width = 230,
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          shiny::span("Polar", title = "Polar"),
          tabName = "polar", icon = shiny::icon("sun")
        ),
        shinydashboard::menuItem(
          shiny::span("Rosas", title = "Rosas"),
          tabName = "rosas", icon = shiny::icon("coins")
        ),
        shinydashboard::menuItem(
          shiny::span("FlameTrees", title = "FlameTrees"),
          tabName = "flametree", icon = shiny::icon("tree")
        )
      )
    ),
    
    # BODY #####################################################################
    
    body = shinydashboard::dashboardBody(
      shiny::tags$head(),
      
      shinydashboard::tabItems(
        
        shinydashboard::tabItem(
          tabName = "polar", shiny::tags$br(),
          Rtist::polar_ui("polar")
        ),
        
        shinydashboard::tabItem(
          tabName = "rosas",  shiny::tags$br(),
          Rtist::rosas_ui("rosas")
        )
        
      )
    ),
    
    # CONTROLS #################################################################
    
    controlbar = shinydashboardPlus::dashboardControlbar(
      id = "rightsidebar", width = 600, collapsed = TRUE, overlay = TRUE,
      shinydashboardPlus::controlbarMenu(
        id = "controlbar",
        
        shinydashboardPlus::controlbarItem(
          title = shiny::span("Tree", title = "Filter documents based on a tree structure."),
          icon = shiny::icon("sitemap"),
          shiny::tags$br()
        )
        
      )
    )
  ),
  
  # SERVER #####################################################################
  
  server = function(session, input, output) {
    
    base::options(
      scipen = 100,
      shiny.maxRequestSize=300*1024^2
    )
    
    Rtist::polar_server("polar")
    Rtist::rosas_server("rosas")
    
    shiny::observeEvent(input$exit, {
      shiny::stopApp()
    })
  }
)
