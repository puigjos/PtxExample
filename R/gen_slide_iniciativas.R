
gen_slide_iniciativas <- function(slide_1, file_template, tabla_tareas){
  
  ################################# styles ################################
  style_subtitle = officer::fp_text(font.size = 16, bold = T, 
                           font.family = 'ubuntu')
  style_ID = officer::fp_text(font.size = 12, bold = F, 
                     font.family = 'ubuntu (Body)')
  style_text = officer::fp_text(font.size = 10, bold = F, 
                       font.family = 'ubuntu (Body)')
  
  ################################# start #################################
  
  my_pres <- officer::read_pptx(path = file_template) %>% 
    PtxGenerator::select_slides(index = 2)
  sl_sm <- officer::slide_summary(my_pres, 1)
  # Title
  my_pres <- officer::ph_with(my_pres, value = slide_1$Category, 
                     location = officer::ph_location_type(type = "title"))
  # Subtitle
  my_pres <- officer::ph_add_text(my_pres, str = slide_1$SubCategory, pos = 'before',
                         style = style_subtitle,
                         ph_label = 'Subtitle')
  ## Iniciativa ID
  my_pres <- officer::ph_add_text(my_pres, str = slide_1$ID_Iniciativa, 
                         pos = 'before',
                         style = style_ID,
                         ph_label = 'IniciativaID')
  ## ResponsableIniciativa
  my_pres <- officer::ph_add_text(my_pres, str = slide_1$Responsable, 
                         pos = 'before',
                         style = style_ID,
                         ph_label = 'ResponsableIniciativa')
  ## Título
  my_pres <- officer::ph_add_text(my_pres, str = slide_1$`Título de la inciativa`, 
                         pos = 'before',
                         style = style_ID,
                         ph_label = 'TituloIniciativa')
  
  # Objetivo
  my_pres <- officer::ph_add_text(my_pres, str = slide_1$Objective, 
                                  pos = 'before',
                         style = style_text,
                         ph_label = 'ObjetivoIniciativa')
  # Riesgos
  my_pres <- officer::ph_add_text(my_pres, str = slide_1$Risks, 
                         pos = 'before',
                         style = style_text,
                         ph_label = 'RiesgosIniciativa')
  
  #  GAPs resueltos
  my_pres <- officer::ph_add_text(my_pres, str = gsub('\r\n', ', ', slide_1$ID_GAPS), 
                         pos = 'before',
                         style = style_text,
                         ph_label = 'GapsResueltos')
  
  # Tareas
  
  dt_tareas <- tabla_tareas %>% 
    dplyr::filter(ID_Iniciativa == slide_1$ID_Iniciativa)
  
  dt_tareas_flex <- dt_tareas %>% 
    dplyr::select(Tareas, `Responsable tarea`, `Tiempo estimado`, `Recursos estimados`) %>% 
    flextable::flextable() %>% 
    flextable::bg(bg = "#e7e6e6", part = "all") %>% 
    flextable::delete_part(part = 'header') %>% 
    flextable::width(j=1, width = 8.062992126) %>% 
    flextable::width(j=2, width = 1.645669291) %>% 
    flextable::width(j=3, width = 1.42519685) %>% 
    flextable::width(j=4, width = 1.291338583) %>% 
    flextable::align(j = 1, align = 'left') %>% 
    flextable::align(j = 2:4, align = 'center') %>% 
    flextable::valign(valign = "center", part = "body") %>% 
    flextable::font(fontname = "Ubuntu (Body)") %>% 
    flextable::fontsize(j = 1, size = 9) %>% 
    flextable::fontsize(j = 2:4, size = 10) %>% 
    flextable::border_remove() %>% 
    flextable::border_inner(border = officer::fp_border(color="white"))
  
  
  my_pres <- officer::ph_with(my_pres, value = dt_tareas_flex, 
                     location = officer::ph_location(left = 0.5393700787,
                                            top = 3.980314961))
  
  #### total personas
  tot_pers <- PtxGenerator::split_function(dt_tareas$`Recursos estimados`, 'p')
  tot_pers <- paste0(sum(tot_pers$V1), 'p - ', sum(tot_pers$V2), 'p')
  
  # Dependencias 
  my_pres <- officer::ph_add_text(my_pres, str = tot_pers, 
                         pos = 'before',
                         style = style_text,
                         ph_label = 'Total Personas')
  colors <- c('ff3300', 'ffe600', '00f53d')
  #  Complejidad
  slide = my_pres$slide$get_slide(1)
  slide <- PtxGenerator::figure_number(slide, GeneralName = 'C', colors = colors, 
                         blank_colors = 'd4d2d4',
                         number = slide_1$Complexity)
  #  Prioridad
  slide <- PtxGenerator::figure_number(slide, GeneralName = 'P', colors = colors, 
                         blank_colors = 'd4d2d4',
                         number = slide_1$Priority)
  
  my_pres$slide$save_slides()
  
  return(my_pres)
  
}
