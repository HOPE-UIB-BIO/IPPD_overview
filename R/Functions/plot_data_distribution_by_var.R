plot_data_distribution_by_var <-
  function(data, var,
           title = var,
           coord_long,
           coord_lat,
           custom_pallete,
           pallete_set = "Set1") {
    if (missing(custom_pallete)) {
      var_list <-
        data %>%
        dplyr::distinct(get(var)) %>%
        purrr::pluck(1)

      var_list_length <- length(var_list)

      get.palette.set <-
        grDevices::colorRampPalette(
          RColorBrewer::brewer.pal(min(max(var_list_length, 3), 8), pallete_set)
        )

      custom_pallete <- get.palette.set(length(var_list))

      names(custom_pallete) <- var_list
    }


    if (missing(coord_long)) {
      xmin <- min(data$long)
      xmax <- max(data$long)
    } else {
      xmin <- min(coord_long)
      xmax <- max(coord_long)
    }

    if (missing(coord_lat)) {
      ymin <- min(data$lat)
      ymax <- max(data$lat)
    } else {
      ymin <- min(coord_lat)
      ymax <- max(coord_lat)
    }

    p_1 <-
      data %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = long,
          y = lat,
          col = get(var)
        )
      ) +
      ggplot2::borders(
        fill = map_color_fill, # [Config]
        colour = map_color_border
      ) + # [Config]
      ggplot2::geom_point(
        size = point_size, # [Config]
        shape = 1
      ) +
      ggplot2::geom_point(
        size = 1,
        shape = 20
      ) +
      ggplot2::coord_quickmap(
        ylim = c(ymin, ymax),
        xlim = c(xmin, xmax)
      ) +
      ggplot2::scale_color_manual(values = custom_pallete) +
      ggplot2::scale_fill_manual(values = custom_pallete) +
      ggplot2::theme(
        legend.title = element_blank(),
        plot.title = element_text(size = text_size * 1.2), # [Config]
        legend.position = "bottom"
      ) +
      ggplot2::labs(
        title = title,
        caption = paste0("Distribution of the sequences (N = ", nrow(data), ")"),
        x = "Longitude",
        y = "Latitude"
      ) +
      ggplot2::guides(
        colour = ggplot2::guide_legend(
          override.aes = list(
            size = point_size, # [Config]
            shape = 19
          ),
          nrow = 3
        )
      )

    xbp <-
      data %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = long,
          fill = get(var)
        )
      ) +
      ggplot2::geom_histogram(
        binwidth = 5,
        size = line_size, # [Config]
        color = gray_dark, # [Config]
        alpha = 0.3
      ) +
      ggplot2::scale_color_manual(values = custom_pallete) +
      ggplot2::scale_fill_manual(values = custom_pallete) +
      ggplot2::theme(
        legend.position = "none"
      ) +
      ggplot2::labs(
        x = "Longitude",
        y = "Number of sequences"
      )

    ybp <-
      data %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = lat,
          fill = get(var)
        )
      ) +
      ggplot2::geom_histogram(
        binwidth = 5,
        size = line_size, # [Config]
        color = gray_dark, # [Config]
        alpha = 0.3
      ) +
      ggplot2::scale_color_manual(values = custom_pallete) +
      ggplot2::scale_fill_manual(values = custom_pallete) +
      ggplot2::theme(
        legend.position = "none"
      ) +
      ggplot2::labs(
        x = "Latitude",
        y = "Number of sequences"
      )


    p_2 <-
      data %>%
      dplyr::group_by(get(var)) %>%
      dplyr::summarise(N = dplyr::n()) %>%
      dplyr::rename(!!var := `get(var)`) %>%
      ggplot2::ggplot(
        ggplot2::aes(
          y = N,
          x = get(var),
          label = N,
          fill = get(var)
        )
      ) +
      ggplot2::geom_bar(
        stat = "identity",
        color = gray_dark, # [Config]
        size = line_size, # [Config]
        alpha = 0.3
      ) +
      ggplot2::geom_text(vjust = 0, nudge_y = 0.5) +
      ggplot2::scale_color_manual(values = custom_pallete) +
      ggplot2::scale_fill_manual(values = custom_pallete) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(
          angle = 45,
          hjust = 1
        ),
        axis.title.x = ggplot2::element_blank()
      ) +
      ggplot2::labs(
        x = var,
        y = "Number of sequences"
      )

    # construct figure
    global_fig <-
      ggpubr::ggarrange(
        p_2, xbp, ybp,
        nrow = 3,
        heights = c(1, 1, 1)
      )

    ggpubr::ggarrange(
      p_1, global_fig,
      nrow = 1,
      widths = c(1, 0.4)
    )
  }
