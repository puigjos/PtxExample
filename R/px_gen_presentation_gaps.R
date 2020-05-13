

px_gen_presentation_gaps <- function(general_path, file_template,
                                     tabla_gaps, num_slides){
  master = 'Office Theme'
  n1 = unique(tabla_gaps$ID_GAP)
  n1 = min(num_slides, length(n1))
  N = n1+2+1
  shiny::withProgress(message = 'Making slides gaps', value = 0,{
    progress = 0/N
    print('hola')
    my_general <- officer::read_pptx(general_path)
    print('adios')
    shiny::incProgress(progress, detail = 'Generating slides...')
    for(i in 1:n1){
      print(i)
      shiny::incProgress((i-1)/N, detail = paste0('Generating slides...', i))
      my_pres <- px_gen_slide(slide_1 = tabla_gaps[i, ],
                              file_template = file_template)
      my_pres <- officer::read_pptx(path = file_template) %>% 
        PtxGenerator::select_slides(index = 1)
      PtxGenerator::append_slide(x = my_general,
                                 slide = my_pres$slide$get_slide(1), media_copy = T)
    }
    incProgress((N-1)/N, detail = paste0('Generating slides...', i))
  })
  return(my_general)
}