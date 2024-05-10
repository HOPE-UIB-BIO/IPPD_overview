plot_data_distribution_by_var <- function(data,
                                          var,
                                          custom_pallete,
                                          pallete_set = "Set1",
                                          point_alpha_outer = 0.3,
                                          point_size = 1,
                                          legend_n_col = 3,
                                          legend_position = "bottom",
                                          ...) {
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

  p_spatial_by_var <-
    plot_data_distribution_spatial(
      data = data,
      var = var,
      ...
    ) +
    ggplot2::geom_point(
      size = point_size,
      shape = 1,
      alpha = point_alpha_outer
    ) +
    ggplot2::scale_color_manual(values = custom_pallete) +
    ggplot2::scale_fill_manual(values = custom_pallete) +
    ggplot2::theme(
      legend.position = legend_position
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

  return(p_spatial_by_var)
}
