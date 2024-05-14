plot_data_distribution_by_age <- function(data,
                                          coord_long,
                                          coord_lat,
                                          vec_breaks,
                                          limits = c(0, 50e3),
                                          ...) {
  if (
    missing(coord_long)
  ) {
    xmin <- min(data$long)
    xmax <- max(data$long)
    coord_long <- c(xmin, xmax)
  } else {
    xmin <- min(coord_long)
    xmax <- max(coord_long)
  }

  if (
    missing(coord_lat)
  ) {
    ymin <- min(data$lat)
    ymax <- max(data$lat)
    coord_lat <- c(ymin, ymax)
  } else {
    ymin <- min(coord_lat)
    ymax <- max(coord_lat)
  }

  p_a <-
    data %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
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
      legend.position = "none"
    ) +
    ggplot2::scale_x_continuous(
      breaks = vec_breaks
    ) +
    ggplot2::labs(
      x = "Age (cal yr BP)",
      y = "Records"
    ) +
    ggplot2::coord_cartesian(
      xlim = c(min(limits), max(limits))
    )

  p_1 <-
    plot_data_distribution_by_numbers(
      data = data,
      var = "age_young_lim",
      coord_long = coord_long,
      coord_lat = coord_lat,
      limits = c(min(limits), max(limits)),
      start_from = "min",
      caption_label = FALSE,
      legend_position = "bottom",
      ...
    ) +
    ggplot2::labs(
      title = "The youngest age"
    )

  p_2 <-
    plot_data_distribution_by_numbers(
      data = data,
      var = "age_old_lim",
      coord_long = coord_long,
      coord_lat = coord_lat,
      limits = c(min(limits), max(limits)),
      start_from = "min",
      caption_label = FALSE,
      legend_position = "bottom",
      ...
    ) +
    ggplot2::labs(
      title = "The oldest age"
    )

  p_3 <-
    plot_data_distribution_by_numbers(
      data = data,
      var = "age_lenght_lim",
      coord_long = coord_long,
      coord_lat = coord_lat,
      limits = c(min(limits), max(limits)),
      start_from = "min",
      caption_label = FALSE,
      legend_position = "bottom",
      ...
    ) +
    ggplot2::labs(
      title = "The total lenght of record"
    )

  global_fig <-
    ggpubr::ggarrange(
      p_1 +
        ggplot2::theme(
          legend.position = "none"
        ),
      p_2 +
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          legend.position = "none"
        ),
      p_3 +
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          legend.position = "none"
        ),
      nrow = 1,
      widths = c(0.37, 0.3, 0.3),
      align = "h",
      labels = "AUTO",
      legend = "none"
    )

  p_legend <-
    cowplot::get_plot_component(p_3, "guide-box-bottom", return_all = TRUE)

  cowplot::plot_grid(
    cowplot::plot_grid(
      global_fig,
      p_legend,
      ncol = 1,
      rel_heights = c(1, 0.2)
    ),
    p_a,
    ncol = 1,
    rel_heights = c(1, 1),
    labels = c("", "D")
  ) %>%
    return()
}
