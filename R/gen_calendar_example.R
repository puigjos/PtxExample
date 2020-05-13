

gen_calendar_example <- function(dt_ini, dt_tare){
  
  dt_tare$TotalDays = PtxGenerator::split_function(dt_tare$`Tiempo estimado`,
                                                   'd')$V1
  
  dt_tare <- dt_tare %>% 
    dplyr::group_by(ID_Iniciativa) %>% 
    dplyr::summarise(
      Dias = sum(TotalDays)
    )
  dt_ini = dt_ini %>% 
    dplyr::left_join(dt_tare)
  
  dt_dependencias = PtxGenerator::relaciones_gen(tabla = dt_ini, 
                                                 id = 'ID_Iniciativa',
                                                 rel = 'Dependencias') %>% 
    dplyr::left_join(dt_tare) %>% dplyr::filter(!is.na(Dias)) %>% 
    dplyr::mutate(Dias = as.numeric(Dias)) %>% 
    dplyr::mutate_if(is.factor, as.character)
  
  dtx = PtxGenerator::gen_calendar_data(dt_dependencias, id_var = 'ID_Iniciativa')
  
  dtx = dtx[!is.na(dtx$start), ] %>% 
    dplyr::select(-Dependencias) %>% unique() %>% 
    dplyr::left_join(., 
                     dt_ini %>%
                       dplyr::select(ID_Iniciativa,
                                     `Título de la inciativa`, 
                                     Definition)
                     ) %>% 
    dplyr::select(ID_Iniciativa, `Título de la inciativa`, 
           Definition, Dias, start, end)
  
  ini_names = c('ID_Iniciativa', 'Título de la inciativa',
                'Definition', 'Dias', 'start', 
                'end')
  dtx1 = dtx %>% 
    dplyr::select(ini_names) %>% 
    data.table::as.data.table()
  tm = (as.character(seq(from = min(dtx$start),
                         to = max(dtx$end), by = 'day')))
  new = rep(0, length(tm))
  names(new) = tm
  
  dtx1 = dtx1 %>% 
    dplyr::mutate(!!!new)
  
  dtx2 <- dtx1 %>% 
    data.table::melt(id = ini_names) %>% 
    unique() %>% 
    dplyr::mutate(value = ifelse(data.table::between(as.Date(variable),
                                         start, end), 1, 0 )) %>% 
    tidyr::spread(key = variable, value = value) %>% 
    dplyr::arrange(start)
  
  colorgrid = ifelse(dtx2[,(length(ini_names)+1):ncol(dtx2)] == 1, "#cceecc", NA) 
  
  
  id_time = (length(ini_names)+1):ncol(dtx2)
  
  y = lubridate::year(lubridate::ymd(names(dtx2)[id_time]))
  m = as.character(format(as.Date(names(dtx2)[id_time]), format = '%b'))
  
  
  typology <- data.frame(
    col_keys = names(dtx2),
    measure = c(ini_names, as.character(format(as.Date(names(dtx2)[id_time]),
                                               format = '%Y'))),
    idat = c(ini_names, as.character(format(as.Date(names(dtx2)[id_time]), 
                                            format = '%b'))),
    stringsAsFactors = FALSE )
  
  # id_time[which(!duplicated(typology$idat[id_time]))]
  
  nm = format(as.Date(names(dtx2)[id_time]), format = '%d %b')
  fontname = "Ubuntu (Body)"
  border_color = 'grey'
  df = dtx2 %>% 
    flextable::flextable(
      # cwidth = c(rep(3*inches, length(ini_names)),
      #            rep(.08*inches, length(id_time)))
        ) %>% 
    # autofit() %>%
    flextable::set_header_df(mapping = typology, key = 'col_keys') %>% 
    flextable::merge_v(part = 'header') %>%
    flextable::merge_h(part = 'body') %>%
    flextable::merge_h(part = 'header') %>%
    flextable::height(height = .5, part = "header") %>% 
    flextable::align(part = 'header', align = 'center') %>% 
    flextable::align(part = 'body', j = 1:length(ini_names)) %>% 
    flextable::align(part = 'body', j = id_time) %>% 
    flextable::compose(j = id_time, value = flextable::as_paragraph(""), 
                       part = "body") %>%
    flextable::bg(j = id_time, bg= colorgrid) %>% 
    flextable::border_inner_h(border = officer::fp_border(color = border_color)) %>% 
    flextable::border_inner_h(border = officer::fp_border(color = border_color),
                              part = 'header') %>%
    flextable::border_inner_v(border = officer::fp_border(color = border_color),
                   part = 'header') %>% 
    flextable::border_outer(border = officer::fp_border(color = border_color)) %>% 
    flextable::width(j = id_time, width = .08*inches) %>%
    flextable::width(j = 1:length(ini_names), width = 3*inches) %>%
    flextable::valign(valign = 'center') %>% 
    flextable::bold(part = 'header') %>% 
    flextable::fontsize(part = 'header', size = 12) %>% 
    flextable::font(fontname = fontname) %>% 
    flextable::vline(j = 1:length(ini_names), 
                     border = officer::fp_border(color=border_color)) %>% 
    flextable::vline(j = id_time[which(!duplicated(typology$idat[id_time]))] - 1, 
                     border = officer::fp_border(color=border_color))
  return(list(flex = df, data = dtx2, ini_names = ini_names))
}
