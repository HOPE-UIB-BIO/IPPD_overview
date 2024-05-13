plot_data_distribution_by_age <- function(data,
                                          coord_long,
                                          coord_lat,
                                          bin_size = 10e3,
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
      breaks = seq(
        from = 0,
        to = max(limits),
        by = bin_size
      )
    ) +
    ggplot2::labs(
      title = "Age distribution of records",
      x = "Age (cal yr BP)",
      y = "Records"
    ) +
    ggplot2::coord_cartesian(
      xlim = c(min(limits), max(limits))
    )

  p_1 <-
    plot_data_distribution_by_numbers(
      data = data_w,
      var = "age_young",
      bin_size = bin_size,
      coord_long = coord_long,
      coord_lat = coord_lat,
      limits = c(min(limits), max(limits)),
      start_from = "min",
      caption_label = FALSE,
      ...
    ) +
    ggplot2::labs(
      title = "Distribution of the youngest level age",
      subtitle = paste("Limit by", min(limits), "years")
    )

  p_2 <-
    plot_data_distribution_by_numbers(
      data = data_w,
      var = "age_old",
      bin_size = bin_size,
      coord_long = coord_long,
      coord_lat = coord_lat,
      limits = c(min(limits), max(limits)),
      start_from = "min",
      caption_label = FALSE,
      ...
    ) +
    ggplot2::labs(
      title = "Distribution of the oldest level age",
      subtitle = paste("Limit by", max(limits), "years")
    )

  p_3 <-
    plot_data_distribution_by_numbers(
      data = data_w,
      var = "age_lenght",
      bin_size = bin_size,
      coord_long = coord_long,
      coord_lat = coord_lat,
      limits = c(min(limits), max(limits)),
      start_from = "min",
      caption_label = FALSE,
      ...
    ) +
    ggplot2::labs(
      title = "Distribution of the total lenght of record",
      subtitle = paste("Limit by", max(limits), "years")
    )

  global_fig <-
    cowplot::plot_grid(
      p_1,
      p_2 +
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank()
        ),
      p_3 +
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank()
        ),
      nrow = 1,
      rel_widths = c(0.37, 0.3, 0.3),
      align = "h",
      labels = "AUTO"
    )

  cowplot::plot_grid(
    global_fig, p_a,
    nrow = 2,
    rel_heights = c(0.5, 0.5),
    labels = c("", "D")
  ) %>%
    return()
}
