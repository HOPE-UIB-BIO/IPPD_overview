plot_data_distribution_by_age <-
  function(data,
           coord_long,
           coord_lat,
           break_size = 10e3,
           limits = c(0, 50e3)) {
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

    data_w <-
      data %>%
      dplyr::mutate(
        age_young = purrr::map_dbl(
          .x = age_range,
          .f = ~ min(.x)
        ),
        age_old = purrr::map_dbl(
          .x = age_range,
          .f = ~ max(.x)
        ),
        age_mid = c(age_young + age_old) / 2,
        age_lenght = abs(age_young - age_old)
      )

    p_a <-
      data_w %>%
      ggplot2::ggplot(
        aes(
          y = reorder(dataset_id, -age_mid),
          yend = dataset_id,
          x = age_young,
          xend = age_old
        )
      ) +
      ggplot2::geom_segment() +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = text_size), # [Config]
        legend.position = "none"
      ) +
      ggplot2::scale_x_continuous(
        breaks = seq(
          from = 0,
          to = max(limits),
          by = break_size
        )
      ) +
      ggplot2::labs(
        title = "Age distribution of sequences",
        x = "Age (cal yr BP)",
        y = "Sequences"
      ) +
      ggplot2::coord_cartesian(
        xlim = c(min(limits), max(limits))
      )

    p_1 <-
      get_plot_by_numbers(
        data_source = data_w,
        var = "age_young",
        break_size = break_size,
        limits = c(min(limits), max(limits))
      ) +
      labs(
        title = "Distribution of the youngest level age",
        subtitle = paste("Limit by", min(limits), "years")
      )

    p_2 <-
      get_plot_by_numbers(
        data_source = data_w,
        var = "age_old",
        break_size = break_size,
        limits = c(min(limits), max(limits))
      ) +
      ggplot2::labs(
        title = "Distribution of the oldest level age",
        subtitle = paste("Limit by", max(limits), "years")
      )

    p_3 <-
      get_plot_by_numbers(
        data_source = data_w,
        var = "age_lenght",
        break_size = break_size,
        limits = c(min(limits), max(limits))
      ) +
      ggplot2::labs(
        title = "Distribution of the total lenght of sequence",
        subtitle = paste("Limit by", max(limits), "years")
      )

    global_fig <-
      ggpubr::ggarrange(
        p_1, p_2, p_3,
        nrow = 1
      )

    ggpubr::ggarrange(
      global_fig, p_a,
      nrow = 2,
      heights = c(0.5, 0.5)
    )
  }
