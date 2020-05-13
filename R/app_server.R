#' @import shiny
app_server <- function(input, output,session) {
  # rm(tmp_gap, tmp_ini)
  observeEvent(input$browser,{
    browser()
  })
  callModule(mod_Introduction_server, "Introduction_ui_1")
  # List the first level callModules here
  callModule(mod_menu_module_server, "menu_module_ui_1")
  datos = callModule(mod_example_input_server, "example_input_ui_1")
  
  callModule(module = mod_GeneratePowerPoint_server, 
             id = "GeneratePowerPoint_ui_1", session = session,
             datos = datos)
  
  callModule(mod_example_output_server, "example_output_ui_1", 
             datos = datos)
  callModule(dlmodule_server, "input1")
 
  
}
