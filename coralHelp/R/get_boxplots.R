get_boxplots <-
function(data, x1, y1, title1 = "Boxplot1", x2, y2, title2 = "Boxplot2",
                         x3, y3, title3 = "Boxplot3", x4, y4, title4 = "Boxplot4"){
  panel_a <- ggplot(data = data)+
    geom_boxplot(aes(x = {{ x1 }}, y = {{ y1 }}, fill = {{ x1 }}))+
    geom_point(aes(x = {{ x1 }}, y = {{ y1 }}))+
    theme_classic(base_size = 8)+
    ggtitle(title1)
  
  
  panel_b <- ggplot(data = data)+
    geom_boxplot(aes(x = {{ x2 }}, y = {{ y2 }}, fill = {{ x2 }}))+
    geom_point(aes(x = {{ x2 }}, y = {{ y2 }}))+
    theme_classic(base_size = 8)+
    ggtitle(title2)
  
  panel_c <- ggplot(data = data)+
    geom_boxplot(aes(x = {{ x3 }}, y = {{ y3 }}, fill = {{ x3 }}))+
    geom_point(aes(x = {{ x3 }}, y = {{ y3 }}))+
    theme_classic(base_size = 8)+
    ggtitle(title3)
  
  panel_d <- ggplot(data = data)+
    geom_boxplot(aes(x = {{ x4 }}, y = {{ y4 }}, fill = {{ x4 }}))+
    geom_point(aes(x = {{ x4 }}, y = {{ y4 }}))+
    theme_classic(base_size = 8)+
    ggtitle(title4)
  
  combine <- plot_grid(panel_a, panel_b, panel_c, panel_d)
  
  return(combine)
}
