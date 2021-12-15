plot.data.distribution.by.numbers <- 
  function(data, var, 
           bin_size = 5,
           coord_long,
           coord_lat,
           title = var) {
    
    
    if(missing(coord_long)){
      xmin <-  min(data$long)
      xmax <-  max(data$long)  
    } else {
      xmin <-  min(coord_long)
      xmax <-  max(coord_long)
    }
    
    if(missing(coord_lat)){
      ymin <-  min(data$lat)
      ymax <-  max(data$lat)  
    } else {
      ymin <-  min(coord_lat)
      ymax <-  max(coord_lat)
    }
    
    data_work <-
      data %>% 
      dplyr::filter(is.na(get(var)) == FALSE) %>% 
      dplyr::mutate(
        bin = floor(get(var) / bin_size) * bin_size)
    
    p_1 <- 
      get.plot.by.numbers(
        data_source = data_work,
        var = "bin",
        title = title,
        break_size = bin_size)
    
    p_2 <- 
      data_work %>% 
      dplyr::group_by(bin) %>% 
      dplyr::summarise(
        N = dplyr::n()) %>% 
      dplyr::rename(!!var := bin) %>% 
      ggplot2::ggplot(
        ggplot2::aes(
          y = N,
          x = get(var),
          label  = N,
          fill = get(var))) + 
      ggplot2::geom_bar(
        stat = "identity", 
        color = gray_dark,  #[Config]
        size = line_size,  #[Config]
        alpha = 0.3) +
      ggplot2::geom_text(
        vjust = 0,
        nudge_y = 0.5) +
      ggplot2::scale_fill_viridis_c()+
      #ggplot2::scale_x_continuous(breaks = custom_breaks)+
      ggplot2::theme(
        legend.position = "none") +
      ggplot2::labs(
        x = title,
        y = "Number of sequences")
    
    ggpubr::ggarrange(
      p_1, p_2,
      nrow = 1, 
      widths = c(1, 0.4)) %>% 
      return()
  }
