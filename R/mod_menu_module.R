# Module UI
  
#' @title   mod_menu_module_ui and mod_menu_module_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_menu_module
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_menu_module_ui <- function(id){
  ns <- NS(id)
  shinydashboard::menuItemOutput(ns('menu'))
}
    
# Module Server
    
#' @rdname mod_menu_module
#' @export
#' @keywords internal
    
mod_menu_module_server <- function(input, output, session){
  ns <- session$ns
  output$menu <- shinydashboard::renderMenu({
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem('Introduction', tabName = 'Introduction'), 
      shinydashboard::menuItem('Example', 
                               shinydashboard::menuSubItem('Input', 
                                                           tabName = 'Example_input'), 
                               shinydashboard::menuSubItem('Output', 
                                                           tabName = 'Example_output'))
    )
  })
}
    
## To be copied in the UI
# mod_menu_module_ui("menu_module_ui_1")
    
## To be copied in the server
# callModule(mod_menu_module_server, "menu_module_ui_1")
 
