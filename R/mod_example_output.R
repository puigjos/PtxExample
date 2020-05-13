# Module UI
  
#' @title   mod_example_output_ui and mod_example_output_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_example_output
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_example_output_ui <- function(id){
  ns <- NS(id)
  shinydashboard::tabItem(
    tabName = 'Example_output', 
    shiny::fluidRow(
      shiny::uiOutput(ns('ui_output_example'))
    )
  )
}
    
# Module Server
    
#' @rdname mod_example_output
#' @export
#' @keywords internal
    
mod_example_output_server <- function(input, output, session, 
                                      datos){
  ns <- session$ns
  
  output$ui_output_example <- shiny::renderUI({
    shinydashboard::tabBox(width = 12,
      tabPanel(title = 'Calendar', 
               div(shinycssloaders::withSpinner(
                 timevis::timevisOutput(ns('calendar_table'))
                 ))
               ), 
      tabPanel(title = 'Complejidad vs Prioridad', 
               plotOutput(ns('plot_prior_compl'))
               )
    )
  })
  
  calendar = reactive({
    req(datos$Initiatives())
    gen_calendar_example(dt_ini = datos$Initiatives(), 
                         dt_tare = datos$Tasks())
  })
  
  output$calendar_table <- timevis::renderTimevis({
    df = calendar()
    dtx2 = df$data
    ini_names = df$ini_names
    timevis::timevis(
      dtx2[,1:length(ini_names)] %>% 
        dplyr::mutate(content = ID_Iniciativa, 
               group = ID_Iniciativa), 
      groups = data.frame(content = dtx2$ID_Iniciativa, 
                          id = dtx2$ID_Iniciativa)
    )
  })
  
  output$plot_prior_compl <- shiny::renderPlot({
    plot_com_prio(datos$Initiatives())
  })
  
}
    
## To be copied in the UI
# mod_example_output_ui("example_output_ui_1")
    
## To be copied in the server
# callModule(mod_example_output_server, "example_output_ui_1")
 
