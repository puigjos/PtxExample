

plot_com_prio <- function(tabla_iniciativas){
  comp_prop <- tabla_iniciativas %>% 
    ggplot2::ggplot(ggplot2::aes(x = Priority, y = Complexity)) + 
    ggplot2::geom_vline(xintercept = c(1.5, 2.5),linetype = 2, 
                        color = '#bfecff', size = 1) + 
    ggplot2::geom_hline(yintercept  = c(1.5, 2.5), alpha = .5, linetype = 2, 
                        color = '#bfecff', size = 1) + 
    ggrepel::geom_label_repel(ggplot2::aes(label = ID_Iniciativa), 
                              fill = '#ffd152',
                     alpha = 1, segment.color = NA, 
                     seed = 123, label.r = .4) +
    ggplot2::scale_y_continuous(limits = c(.5, 3.5), name = 'Priority') + 
    ggplot2::scale_x_reverse(limits = c(3.5, 0.5), name = 'Complexity') + 
    ggplot2::theme_minimal() + 
    ggplot2::labs(title = 'Complejidad vs Prioridad')
  return(comp_prop)
}