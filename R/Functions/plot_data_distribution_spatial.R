plot_data_distribution_spatial <-
  function(data,
           var,
           coord_long,
           coord_lat,
           point_size = 1,
           point_alpha_inner = 1,
           text_size = 10,
           line_size = 0.1,
           breaks_y_by = 25,
           breaks_x_by = 20,
           map_color_fill = "grey90",
           map_color_border = "black",
           legend_position = "bottom",
           var_as_factor = TRUE,
           caption_label = TRUE) {
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


    if (
      isTRUE(var_as_factor)
    ) {
      p_basic <-
        data %>%
        ggplot2::ggplot(
          ggplot2::aes(
            x = long,
            y = lat,
            col = as.factor(get(var))
          )
        )
    } else {
      p_basic <-
        data %>%
        ggplot2::ggplot(
          ggplot2::aes(
            x = long,
            y = lat,
            col = get(var)
          )
        )
    }

    p_map <-
      p_basic +
      ggplot2::borders(
        fill = map_color_fill,
        colour = map_color_border
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
      ggplot2::theme(
        text = ggplot2::element_text(size = text_size),
        plot.title = ggplot2::element_text(size = text_size),
        line = ggplot2::element_line(size = line_size),
        legend.title = ggplot2::element_blank(),
        legend.position = legend_position
      ) +
      ggplot2::labs(
        x = "Longitude",
        y = "Latitude"
      )

    if (
      isTRUE(caption_label)
    ) {
      p_map <-
        p_map +
        ggplot2::labs(
          caption = paste0(
            "Distribution of the records (N = ", nrow(data), ")"
          )
        )
    }

    return(p_map)
  }
