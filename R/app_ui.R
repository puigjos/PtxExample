#' @import shiny
app_ui <- function() {
  
  shinydashboard::dashboardPage(
    header = dashboard_header(),
    sidebar = dashboard_Sidebar(),
    body = dashboard_body()
    )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'PtxExample')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
