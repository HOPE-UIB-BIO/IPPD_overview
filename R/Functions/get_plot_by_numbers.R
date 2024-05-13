get_plot_by_numbers <-
  function(data_source,
           var,
           title = var,
           break_size = 5,
           limits = c(-Inf, Inf)) {
    xmin <- min(data_source$long)
    xmax <- max(data_source$long)

    ymin <- min(data_source$lat)
    ymax <- max(data_source$lat)

    val_limits <-
      data_source %>%
      dplyr::select(any_of(var)) %>%
      purrr::pluck(1) %>%
      range()

    sel_min <-
      c(min(limits), min(val_limits), 0) %>%
      max()

    sel_max <-
      c(max(limits), max(val_limits)) %>%
      min()

    custom_breaks <-
      seq(
        from = sel_min,
        to = ceiling(sel_max / break_size) * break_size,
        by = break_size
      )

    data_sub <-
      data_source %>%
      dplyr::mutate(
        !!var := ifelse(get(var) > sel_max, sel_max, get(var)),
        !!var := ifelse(get(var) < sel_min, sel_min, get(var))
      )

    data_sub %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = long,
          y = lat,
          col = get(var),
          size = get(var)
        )
      ) +
      ggplot2::borders(
        fill = map_color_fill, # [Config]
        colour = map_color_border
      ) + # [Config]
      ggplot2::geom_point(
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
      ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = text_size), # [Config]
        legend.position = "bottom"
      ) +
      ggplot2::labs(
        title = title,
        caption = paste0(
          "Distribution of the sequences (N = ", nrow(data_sub), ")"
        ),
        x = "Longitude",
        y = "Latitude"
      ) +
      ggplot2::scale_color_viridis_c(
        limits = c(sel_min, sel_max),
        breaks = custom_breaks
      ) +
      ggplot2::scale_size_continuous(
        limits = c(sel_min, sel_max),
        breaks = custom_breaks
      ) +
      ggplot2::guides(
        colour = ggplot2::guide_legend(
          override.aes = list(shape = 19),
          nrow = 1
        ),
        size = ggplot2::guide_legend(
          override.aes = list(shape = 19),
          nrow = 1
        )
      ) %>%
      return()
  }
