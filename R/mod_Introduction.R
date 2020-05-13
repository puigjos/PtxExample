# Module UI
  
#' @title   mod_Introduction_ui and mod_Introduction_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_Introduction
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_Introduction_ui <- function(id){
  ns <- NS(id)
  shinydashboard::tabItem(
    tabName = 'Introduction', 
    shiny::fluidRow(
      shinydashboard::box(width = 12, 
                          title = 'Introduction', 
                          div(shiny::includeMarkdown(
                            system.file("inst",'Introduction.md',
                                        package = 'PtxExample')
                            ), style = 'font-size: 10px'), 
                          h5('Recommended methodology'), 
                          shiny::imageOutput(ns("myImage_process"), 
                                             height = '250px')
                          ),
      shinydashboard::box(
        title = 'Example: GAP analysis & Project planning', width = 12, 
        column(4, 
               div(shiny::includeMarkdown(
                 system.file("inst",'Example.md',
                             package = 'PtxExample')
                 ), style = 'font-size: 10px')
               ),
        column(8, shiny::imageOutput(ns("myImage_gap")))
        )
      )
  )
}
    
# Module Server
    
#' @rdname mod_Introduction
#' @export
#' @keywords internal
    
mod_Introduction_server <- function(input, output, session){
  ns <- session$ns
  output$myImage_process <- shiny::renderImage({
    path_img = system.file("inst/extdata",'process.png',
                         package = 'PtxExample')
    
    # Return a list containing the filename
    list(src = path_img,
         contentType = 'image/png',
         width = 800,
         height = 250)
  }, deleteFile = FALSE)
  
  output$myImage_gap <- shiny::renderImage({
    path_img = system.file("inst/extdata",'gap_example.png',
                           package = 'PtxExample')
    # Return a list containing the filename
    list(src = path_img,
         contentType = 'image/png',
         width = 550,
         height = 220)
  }, deleteFile = FALSE)
  
}
    
## To be copied in the UI
# mod_Introduction_ui("Introduction_ui_1")
    
## To be copied in the server
# callModule(mod_Introduction_server, "Introduction_ui_1")
 
