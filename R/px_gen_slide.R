

px_gen_slide <- function(slide_1, file_template){
  ################################# styles ################################
  style_subtitle = officer::fp_text(font.size = 16, bold = T, 
                           font.family = 'ubuntu')
  style_ID = officer::fp_text(font.size = 12, bold = F, 
                     font.family = 'ubuntu (Body)')
  style_text = officer::fp_text(font.size = 10, bold = F, 
                       font.family = 'ubuntu (Body)')
  
  
  ################################# start #################################
  
  my_pres <- officer::read_pptx(path = file_template) %>% 
    PtxGenerator::select_slides(index = 1)
  sl_sm <- officer::slide_summary(my_pres, 1)
  # Title
  my_pres <- officer::ph_with(my_pres, value = slide_1$Category,
                              location = officer::ph_location_type(type = "title"))
  # Subtitle
  my_pres <- officer::ph_add_text(my_pres, str = slide_1$Subcategory, 
                                  pos = 'before',
                         style = style_subtitle,
                         ph_label = 'Text Placeholder 3')
  ## GAP ID
  my_pres <- officer::ph_add_text(my_pres, str = slide_1$ID_GAP, 
                         pos = 'before',
                         style = style_ID,
                         ph_label = 'ID_GAP')
  
  # Title GAP
  my_pres <- officer::ph_add_text(my_pres, str = slide_1$`GAP title`, 
                                  pos = 'before',
                                  style = style_ID,
                                  ph_label = 'TitleGAP')
  
  # Situación Actual
  my_pres <- officer::ph_add_text(my_pres, str = slide_1$`Current situation`,
                                  pos = 'before',
                         style = style_text,
                         ph_label = 'Situación Actual')
  # Situación Objetivo
  my_pres <- officer::ph_add_text(my_pres, str = slide_1$`Objective situation`, pos = 'before',
                         style = style_text,
                         ph_label = 'Situación Objetivo')
  
  #  GAP
  my_pres <- officer::ph_add_text(my_pres, str = slide_1$GAP, pos = 'before',
                         style = style_text,
                         ph_label = 'GAP')
  
  #  Impacto
  my_pres <- officer::ph_add_text(my_pres, str = slide_1$Impact,
                         ph_label = 'Impacto',
                         style = style_text, pos = 'before')
  slide = my_pres$slide$get_slide(1)
  colors <- rep('9b2ab8', 4)
  #  Impacto en negocio
  slide <- PtxGenerator::figure_number(slide, GeneralName = 'IN', 
                                       colors = colors, 
                         blank_colors = 'd4d2d4',
                         number = slide_1$`Business impact`)
  #  Impacto operativo
  slide <- PtxGenerator::figure_number(slide, GeneralName = 'IO', 
                                       colors = colors, 
                         blank_colors = 'd4d2d4',
                         number = slide_1$`Operative impact`)
  #  Impacto sistemas
  slide <- PtxGenerator::figure_number(slide, GeneralName = 'IS', colors = colors, 
                         blank_colors = 'd4d2d4',
                         number = slide_1$`System impact`)
  
  ####### Área Impactada/Áreas Impactadas
  
  my_pres <- officer::ph_add_text(my_pres, str = slide_1$`Área Impactada/Áreas Impactadas`,
                         ph_label = 'AreasImplicadas',
                         style = style_text, pos = 'before')
  # ProcesosImpactados
  
  proc = PtxGenerator::relaciones_gen(slide_1, id = 'ID_GAP', 
                        rel = 'Procesos Impactados')[[2]]
  proc = as.character(proc)
  posi = list(
    'Gestión interna de la tesorería' = c(x = 19.33 * EMU_cm, y = 15.13*EMU_cm), 
    'Liquidación en cuenta' = c(x = 19.33 * EMU_cm, y = 15.79*EMU_cm),
    'Configuración CRDM' = c(x = 19.33 * EMU_cm, y = 16.43*EMU_cm),
    'Transferencia de tesorería entre cuentas' = c(x = 19.33 * EMU_cm, y = 17.09*EMU_cm),
    'Reserva de liquidez' = c(x = 19.33 * EMU_cm, y = 17.77*EMU_cm)
  )
  if(length(proc) > 0){
    for(p in proc){
      sl <- officer::read_pptx(path = file_template) %>% 
        PtxGenerator::select_slides(3)
      slide_add <- sl$slide$get_slide(1)
      id_shape = PtxGenerator::get_slide_summary(slide_add)
      id_shape = id_shape$id[id_shape$ph_label == 'Cruz_procesos']
      new_node = PtxGenerator::get_xml_id(slide_add, id = id_shape)
      j = which(p == names(posi))
      if(length(j) != 1){
        next
      }
      coord = posi[[j]]
      new_node = PtxGenerator::change_position(node = new_node, 
                                 x = coord['x'], y = coord['y'])
      slide = PtxGenerator::insert_shape(slide, new_node)
    }
    
  }
  my_pres$slide$save_slides()

  
  return(my_pres)
}