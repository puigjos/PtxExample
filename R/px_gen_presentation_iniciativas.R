
px_gen_presentation_iniciativas <- function(general_path, file_template, 
                                            tabla_iniciativas, tabla_gaps, 
                                            tabla_tareas, num_slides){
  master = 'Capgemini Invent 2019'
  n0 = unique(tabla_iniciativas$ID_Iniciativa)
  nn = min(num_slides, length(n0))
  n0 = n0[1:nn]
  N = length(n0)+2+1
  shiny::withProgress(message = 'Making slides Iniciativas', value = 0,{
    progress = 0/N
    my_general <- officer::read_pptx(general_path)
    ini_gap = PtxGenerator::relaciones_gen(tabla_iniciativas, id = 'ID_Iniciativa', rel = 'ID_GAPS')
    for(i in 1:length(n0)){
      print(i)
      nm = n0[i]
      shiny::incProgress((i-1)/N, detail = paste0('Generating slides...', i))
      my_pres <- gen_slide_iniciativas(tabla_iniciativas[tabla_iniciativas$ID_Iniciativa == nm, ],
                                       file_template = file_template, 
                                       tabla_tareas = tabla_tareas)
      PtxGenerator::append_slide(x = my_general, 
                   slide = my_pres$slide$get_slide(1), media_copy = T)
      total_slides = gen_slide_conj_gaps(gaps = ini_gap$ID_GAPS[ini_gap$ID_Iniciativa == nm], 
                                         tabla_gaps = tabla_gaps, 
                                         file_template =  file_template)
      for(jj in 1:length(total_slides)){
        PtxGenerator::append_slide(x = my_general, 
                     slide = total_slides[[jj]]$slide$get_slide(1), 
                     media_copy = F)
        }
      }
    })
  return(my_general)
  }