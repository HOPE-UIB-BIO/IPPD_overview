plot_data_distribution_by_var <- function(data,
                                          var,
                                          custom_palette,
                                          palette_set = "Set1",
                                          point_alpha_outer = 0.3,
                                          point_size = 1,
                                          legend_n_col = 3,
                                          legend_position = "bottom",
                                          ...) {
  if (
    missing(custom_palette)
  ) {
    custom_palette <-
      make_custom_palette(data, var, palette_set)
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
    ggplot2::scale_color_manual(values = custom_palette) +
    ggplot2::scale_fill_manual(values = custom_palette) +
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
