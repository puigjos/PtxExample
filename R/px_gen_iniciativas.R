
px_gen_iniciativas <- function(general_path, file_template, tabla_gaps){
  
  master = 'Capgemini Invent 2019'
  n1 = 2
  N = n1+2+1
  shiny::withProgress(message = 'Making slides', value = 0,{
    progress = 0/N
    my_general <- officer::read_pptx(general_path) %>% 
      officer::add_slide(layout = '2_Divider Page', master = master)
    shiny::incProgress(progress, detail = 'Generating slides...')
    for(i in 1:n1){
      incProgress((i-1)/N, detail = paste0('Generating slides...', i))
      my_pres <- px_gen_slide(tabla_gaps[i, ], file_template = file_template)
      PtxGenerator::append_slide(x = my_general, 
                                 slide = my_pres$slide$get_slide(1), media_copy = T)
    }
    incProgress((N-1)/N, detail = paste0('Generating slides...', i))
  })
  return(my_general)
}