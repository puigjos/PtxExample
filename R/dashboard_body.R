
dashboard_body <- function(){
  shinydashboard::dashboardBody(
    shinyDashboardThemes(
      theme = "blue_gradient"
    ),
    actionButton("browser", "browser"),
    tags$script("$('#browser').hide();"),
    shinydashboard::tabItems(
      mod_Introduction_ui("Introduction_ui_1"),
      mod_example_input_ui("example_input_ui_1"), 
      mod_example_output_ui("example_output_ui_1")
    )
  )
}