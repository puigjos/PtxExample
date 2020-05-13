# Module UI
  
#' @title   mod_example_input_ui and mod_example_input_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_example_input
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_example_input_ui <- function(id){
  ns <- NS(id)
  css <- ".nowrap {
            white-space: nowrap;
          }"
  shinydashboard::tabItem(
    tabName = 'Example_input', 
    shiny::fluidRow(
      tags$head(
        tags$style(HTML(css))
      ),
      shinydashboard::box(
        title = 'Inputs', solidHeader = T, width = 3,
        selectInput(ns('id_type'), ' ', c('Example', 'Excel file')),
        conditionalPanel(
          "input.id_type == 'Excel file'", ns = ns,
          div(style = "font-size: 12px",
              shiny::fileInput(ns('example_file_input'), 
                           label = 'Import the Excel File',
                           accept = c('xlsx', 'xls'))
            )
        ), 
        mod_GeneratePowerPoint_ui("GeneratePowerPoint_ui_1")
      ), 
      shiny::uiOutput(ns('insert_excel'))
    )
  )
}
    
# Module Server
    
#' @rdname mod_example_input
#' @export
#' @keywords internal
    
mod_example_input_server <- function(input, output, session){
  ns <- session$ns
  
  path = reactive({
    if(input$id_type == 'Example'){
      normalizePath(system.file("inst/extdata",'Example.xlsx',
                                package = 'PtxExample'))
    }else{
      if(!is.null(input$example_file_input)){
        input$example_file_input$datapath
      }else{NULL}
    }
  })
  
  output$insert_excel = shiny::renderUI({
    
    if(is.null(input$example_file_input) & 
       input$id_type == 'Excel file'){
      return(NULL)
    }
    
    sheets = readxl::excel_sheets(path())
    
    tmp = lapply(sheets, function(sheet){
      shiny::tabPanel(title = sheet, 
                      div(DT::dataTableOutput(ns(paste0('dt_sheet_', sheet))),
                      style = "overflow-y: scroll;
                          overflow-x: scroll;
                          font-size:60%;")
                      )
    })
    do.call(shinydashboard::tabBox, args = c(width = 9, tmp))
  })
  
  datos = reactiveValues()
  
  shiny::observeEvent(path(), {
    sheets = readxl::excel_sheets(path())
    lapply(sheets, function(sheet){
      datos[[sheet]] <<- reactive(readxl::read_excel(
        path(),
        sheet = sheet))
    })
    
    lapply(sheets, function(sheet){
      output[[paste0('dt_sheet_', sheet)]] = DT::renderDataTable({
        datos[[sheet]]() %>% 
          DT::datatable(options = list(pageLength = 10, dom = 'ltip', 
                                       columnDefs = list(
                                         list(className = "nowrap", 
                                              targets = "_all")
                                       )),
                        rownames = FALSE)
      })
    })
  })
  return(datos)
}
    
## To be copied in the UI
# mod_example_input_ui("example_input_ui_1")
    
## To be copied in the server
# callModule(mod_example_input_server, "example_input_ui_1")
 
