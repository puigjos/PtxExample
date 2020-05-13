

gen_slide_conj_gaps <- function(gaps, tabla_gaps, file_template){
  ################################# styles ################################
  style_subtitle = officer::fp_text(font.size = 16, bold = T, 
                           font.family = 'ubuntu')
  style_ID = officer::fp_text(font.size = 12, bold = T, 
                     font.family = 'ubuntu (Body)')
  style_text = officer::fp_text(font.size = 10, bold = F, 
                       font.family = 'ubuntu (Body)')
  ################################ FIGURES ##################################
  
  fig = c('Título GAP_', 'GAP_', 'g_', 'sistema_', 'proceso_', 
          'flag_', 'palo_', 'id_gap_', 'linea_', 'Objetivo_')
  
  ################################# start #################################
  d = 1:length(gaps)
  slides_total = split(d, ceiling(seq_along(d)/3))
  dt_gaps = tabla_gaps
  
  total_slides <- lapply(slides_total, function(split){
    my_pres <- officer::read_pptx(path = file_template) %>% 
      PtxGenerator::select_slides(index = 5)
    
    for(i in seq_along(split)){
      id_gap = split[i]
      dt_g = dt_gaps[dt_gaps$ID_GAP == gaps[id_gap],]
      
      # ID_GAP
      id_gap = as.numeric(substr(dt_g$ID_GAP, 5,6))
      
      my_pres <- officer::ph_add_text(my_pres, str = id_gap, pos = 'before',
                             style = style_text,
                             ph_label = paste0('id_gap_', i))
      ## Título
      my_pres <- PtxGenerator::ph_add_fpar_jp(x = my_pres, 
                                value = officer::fpar(officer::ftext(dt_g$`GAP title`, prop = style_ID), 
                                             # fp_t = style_ID,
                                             fp_p = officer::fp_par(text.align = 'center')),
                                pos = 'before',
                                ph_label = paste0('Título GAP_', i), par_default = F)
      # Situacion actual
      my_pres <- officer::ph_add_text(my_pres, str = dt_g$`Current situation`,
                                      pos = 'before',
                             style = style_text,
                             ph_label = paste0('GAP_', i))
      # Objetivo
      my_pres <- officer::ph_add_text(my_pres, str = dt_g$`Objective situation`, pos = 'before',
                             style = style_text,
                             ph_label = paste0('Objetivo_', i))
      
      
      slide = my_pres$slide$get_slide(1)
      pf = PtxGenerator::get_slide_summary(slide)
    }
    id_not = c(1:3)[-seq_along(split)]
    if(length(id_not) > 0){
      for(elim in id_not){
        for(f in fig){
          slide = my_pres$slide$get_slide(1)
          pf = PtxGenerator::get_slide_summary(slide)
          office_id = pf$id[pf$ph_label == paste0(f,elim)]
          current_elt <- PtxGenerator::get_xml_id(slide, id = office_id)
          xml2::xml_remove(current_elt)
        }
      }
    }
    slide$save()
    return(my_pres)
  }) 
  return(total_slides)
  
}