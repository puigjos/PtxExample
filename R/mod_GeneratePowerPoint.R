# Module UI
  
#' @title   mod_GeneratePowerPoint_ui and mod_GeneratePowerPoint_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_GeneratePowerPoint
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_GeneratePowerPoint_ui <- function(id){
  ns <- NS(id)
  tagList(
    checkboxInput(ns('input_gaps'), 'Gaps Slides', value = T), 
    checkboxInput(ns('input_iniciativas'), 'Initiatives Slides', value = T), 
    # checkboxInput(ns('input_calendar'), 'Calendar'), 
    numericInput(ns('num_slides'), 'Number of slides', value = 4),
    actionButton(ns('generate_slide'), 'Generate'), 
    uiOutput(ns('download_call_module'))
    # dlmodule_ui('input1')
  )
}
    
# Module Server
    
#' @rdname mod_GeneratePowerPoint
#' @export
#' @keywords internal
    
mod_GeneratePowerPoint_server <- function(input, output, 
                                          session, 
                                          datos){
  ns <- session$ns
  
  observeEvent(input$generate_slide, {
    # rm(tmp_gap, tmp_ini)
    if(!any(c(input$input_gaps, input$input_iniciativas))){
      return(
        shinyWidgets::sendSweetAlert(session = session,
                                     title = "Error!!", 
                                     text = 'Check almost one box', type = 'error')
      )
    }
    path_temp = system.file("inst/extdata",'Template.pptx',
                            package = 'PtxExample')
    path_gen = system.file("inst/extdata",'DocExamp.pptx',
                           package = 'PtxExample')
    tabla_gaps = datos[['GAPS']]()
    tabla_iniciativas = datos[['Initiatives']]()
    tabla_tareas = datos[['Tasks']]()
    # my_general <- officer::read_pptx(path_gen)
    # my_pres <- officer::read_pptx(path = path_temp)
    if(input$input_gaps){
      function_generation = px_gen_presentation_gaps
      gen = do.call(function_generation, args = list(general_path = path_gen,
                                                     file_template = path_temp,
                                                     tabla_gaps = tabla_gaps,
                                                     num_slides = input$num_slides))
      tmp_gap <<- tempfile(fileext = '.pptx')
      print(gen, target = tmp_gap)
    }
    if(input$input_iniciativas){
      function_generation = px_gen_presentation_iniciativas
      gen = do.call(function_generation, args = list(general_path = path_gen,
                                                     file_template = path_temp,
                                                     tabla_iniciativas = tabla_iniciativas,
                                                     tabla_tareas = tabla_tareas,
                                                     tabla_gaps = tabla_gaps,
                                                     num_slides = input$num_slides))
      tmp_ini <<- tempfile(fileext = '.pptx')
      print(gen, target = tmp_ini)
    }
    output$download_call_module = renderUI({
      dlmodule_ui('input1')
    })
  })
  

}
   
dlmodule_ui <- function(id){
  ns <- NS(id)
  # uiOutput(ns('ui_download'))
  downloadButton(ns('downloadData'), 'Download')
}

dlmodule_server <- function(input, output, session) {
  ns <- session$ns
  output$ui_download = renderUI({
    # if(any(c('tmp_ini', 'tmp_gap') %in% ls())){
    # }
    downloadButton(ns('downloadData'), 'Download')
  })
  
  
  output$downloadData = downloadHandler(
    filename = function(){
      paste("output", "zip", sep=".")
    }, 
    content = function(ffile){
      # if(!any(c('tmp_ini', 'tmp_gap') %in% ls())){
      #   return(NULL)
      # }
      files_names = c('tmp_ini', 'tmp_gap')
      files_names = files_names[files_names %in% ls()]
      
      zip::zipr(zipfile = ffile,
               files = as.character(sapply(files_names, get)),
               # files = c(tmp_ini, tmp_gap),
               include_directories = F)
    },contentType = "application/zip"
  )
}


## To be copied in the UI
# mod_GeneratePowerPoint_ui("GeneratePowerPoint_ui_1")
    
## To be copied in the server
# callModule(mod_GeneratePowerPoint_server, "GeneratePowerPoint_ui_1")
 
