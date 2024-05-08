plot_data_distribution_spatial <-
  function(data,
           var,
           coord_long,
           coord_lat,
           custom_pallete,
           pallete_set = "Set1",
           point_size = 1,
           point_alpha_outer = 0.3,
           point_alpha_inner = 1,
           text_size = 10,
           line_size = 0.1,
           breaks_y_by = 25,
           breaks_x_by = 20,
           map_color_fill = "grey90",
           map_color_border = "black",
           legend_n_col = 3,
           caption_label = TRUE) {
    if (
      missing(custom_pallete)
    ) {
      var_list <-
        data %>%
        dplyr::distinct(get(var)) %>%
        purrr::pluck(1)

      var_list_length <- length(var_list)

      get_palette_set <-
        grDevices::colorRampPalette(
          RColorBrewer::brewer.pal(
            min(
              max(var_list_length, 3),
              8
            ),
            pallete_set
          )
        )

      custom_pallete <- get_palette_set(var_list_length)

      names(custom_pallete) <- var_list
    }

    if (
      missing(coord_long)
    ) {
      xmin <- min(data$long)
      xmax <- max(data$long)
    } else {
      xmin <- min(coord_long)
      xmax <- max(coord_long)
    }

    if (
      missing(coord_lat)
    ) {
      ymin <- min(data$lat)
      ymax <- max(data$lat)
    } else {
      ymin <- min(coord_lat)
      ymax <- max(coord_lat)
    }

    y_breaks <- seq(ymin, ymax, by = breaks_y_by)
    x_breaks <- seq(xmin, xmax, by = breaks_x_by)

    p_map <-
      data %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = long,
          y = lat,
          col = get(var)
        )
      ) +
      ggplot2::borders(
        fill = map_color_fill,
        colour = map_color_border
      ) +
      ggplot2::geom_point(
        size = point_size,
        shape = 1,
        alpha = point_alpha_outer
      ) +
      ggplot2::geom_point(
        size = 1,
        shape = 20,
        alpha = point_alpha_inner
      ) +
      ggplot2::coord_quickmap(
        ylim = c(ymin, ymax),
        xlim = c(xmin, xmax)
      ) +
      ggplot2::scale_x_continuous(
        breaks = x_breaks
      ) +
      ggplot2::scale_y_continuous(
        breaks = y_breaks
      ) +
      ggplot2::scale_color_manual(values = custom_pallete) +
      ggplot2::scale_fill_manual(values = custom_pallete) +
      ggplot2::theme(
        text = ggplot2::element_text(size = text_size),
        plot.title = ggplot2::element_text(size = text_size),
        line = ggplot2::element_line(size = line_size),
        legend.title = ggplot2::element_blank(),
        legend.position = "bottom"
      ) +
      ggplot2::labs(
        x = "Longitude",
        y = "Latitude"
      ) +
      ggplot2::guides(
        colour = ggplot2::guide_legend(
          override.aes = list(
            size = point_size,
            shape = 19
          ),
          ncol = legend_n_col
        )
      )

    if (
      isTRUE(caption_label)
    ) {
      p_map <-
        p_map +
        ggplot2::labs(
          caption = paste0(
            "Distribution of the sequences (N = ", nrow(data), ")"
          )
        )
    }

    return(p_map)
  }
