
gen_calendar_xlsx <- function(datos){
  dtx2 = datos
  ini_names = c('ID_Iniciativa', 'Título de la inciativa',
                'Definition', 'Dias', 'start', 
                'end')
  colorgrid = ifelse(dtx2[,(length(ini_names)+1):ncol(dtx2)] == 1, "#cceecc", NA) 
  id_time = (length(ini_names)+1):ncol(dtx2)
  y = lubridate::year(lubridate::ymd(names(dtx2)[id_time]))
  m = as.character(format(as.Date(names(dtx2)[id_time]), format = '%b'))
  typology <- data.frame(
    measure = c(rep('', length(ini_names)), as.character(format(as.Date(names(dtx2)[id_time]),
                                                                format = '%Y'))),
    idat = c(rep('', length(ini_names)), as.character(format(as.Date(names(dtx2)[id_time]), 
                                                             format = '%b'))),
    day = c(ini_names, as.character(format(as.Date(names(dtx2)[id_time]), 
                                           format = '%d-%b'))),
    stringsAsFactors = FALSE )
  
  t_typology = as.data.frame(t(typology))
  names(t_typology) = NULL
  t_dtx2 = dtx2
  colnames(t_dtx2) = NULL
  nm = format(as.Date(names(dtx2)[id_time]), format = '%d %b')
  fontname = "Ubuntu (Body)"
  border_color = 'grey'
  tmfile = tempfile(fileext = '.xlsx')
  wb = openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetName = 'Calendar', zoom = 55)
  openxlsx::writeData(wb = wb, sheet = 'Calendar', x = t_dtx2, 
                      startCol = 2, startRow = 7, 
                      colNames = F)
  openxlsx::writeData(wb, sheet = 'Calendar' ,
                      x = t_typology, startCol = 2, startRow = 4,
                      colNames = F)
  
  
  # Fill time
  negStyle <- openxlsx::createStyle(fontColour = "#9C0006", fgFill = "#FFC7CE",
                                    bgFill = "#FFC7CE")
  for (i in 1:nrow(dtx2)){
    openxlsx::addStyle(wb, sheet = 'Calendar', rows = i + 6, 
                       cols = id_time[dtx2[i, - (1:length(ini_names))] == 1] + 1, 
                       style = negStyle, gridExpand = T)
  }
  
  # Fill headers
  fillStyle <- openxlsx::createStyle(fontColour = "#ffffff", fgFill = "#999999",
                                     bgFill = "#999999", halign = 'center', 
                                     valign = 'center', wrapText = T,
                                     textDecoration = 'bold')
  openxlsx::addStyle(wb, sheet = 'Calendar', rows = 6, 
                     cols = 1:length(ini_names) + 1, 
                     style = fillStyle, gridExpand = T)
  
  fillStyle2 <- openxlsx::createStyle(fontColour = "#787777", fgFill = "#787777",
                                      bgFill = "#787777", halign = 'center',
                                      wrapText = T,
                                      valign = 'center', textDecoration = 'bold')
  openxlsx::addStyle(wb, sheet = 'Calendar', rows = 6, 
                     cols = id_time + 1, 
                     style = fillStyle2, gridExpand = T)
  fillStyle3 <- openxlsx::createStyle(fontColour = "#ffffff", fgFill = "#2d5e49",
                                      bgFill = "#2d5e49", wrapText = T, 
                                      halign = 'center', valign = 'center')
  openxlsx::addStyle(wb, sheet = 'Calendar', rows = 4:5, 
                     cols = id_time + 1, 
                     style = fillStyle3, gridExpand = T)
  
  ## Merge
  
  for(i in 1:ncol(typology)){
    values = typology[id_time, i]
    for(j in unique(values)){
      openxlsx::mergeCells(wb, "Calendar", 
                 cols = id_time[typology[[i]][-c(1:length(ini_names))] == j] + 1,
                 rows = 3 + i)
    }
  }
  
  
  openxlsx::modifyBaseFont(wb, fontSize = 8, 
                           fontColour = "black",
                           fontName = "Arial")
  
  openxlsx::deleteData(wb, 'Calendar', cols = 1 + id_time, 
                       rows = 6 + (1:nrow(t_dtx2)), gridExpand = T)
  openxlsx::setColWidths(wb, 'Calendar', 
                         cols = 1:(length(id_time) + length(ini_names)),
                         widths = "auto")
  openxlsx::setColWidths(wb, 'Calendar', widths = .72, cols = 1 + id_time)
  openxlsx::setRowHeights(wb, sheet = 'Calendar', rows = 6, heights = 34)
  openxlsx::showGridLines(wb, sheet = 'Calendar', showGridLines = F)
  
  # Grid white
  
  grid = openxlsx::createStyle(border = 'TopBottomLeftRight', 
                               borderColour = '#ffffff')
  openxlsx::addStyle(wb, 'Calendar', rows = 3:(nrow(dtx2) + 7), 
                     cols = 1:(length(ini_names) + length(id_time) + 1 ),
                     style = grid, gridExpand = T, stack = T)
  openxlsx::saveWorkbook(wb = wb, file = tmfile)
  browseURL(tmfile)
  
  
  
}

