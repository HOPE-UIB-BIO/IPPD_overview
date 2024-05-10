plot_data_distribution_by_numbers <- function(
    data,
    var,
    bin_size = 5,
    point_alpha_outer = 0.3,
    legend_n_col = 3,
    ...) {
  vec_breaks <-
    get_binned(
      data_source = data,
      var = var,
      bin_size = bin_size,
      mode = "breaks"
    )

  data_binned <-
    get_binned(
      data_source = data,
      var = var,
      bin_size = bin_size,
      mode = "data"
    )

  p_binned <-
    plot_data_distribution_spatial(
      data = data_binned,
      var = var,
      var_as_factor = FALSE,
      ...
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        size = get(var),
        col = get(var)
      ),
      shape = 1,
      alpha = point_alpha_outer
    ) +
    ggplot2::scale_color_viridis_c(
      breaks = vec_breaks,
      labels = vec_breaks,
      direction = -1,
      option = "plasma"
    ) +
    ggplot2::scale_size_continuous(
      breaks = vec_breaks,
      labels = vec_breaks
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(
          shape = 19,
          alpha = 1
        ),
        ncol = legend_n_col
      ),
      size = ggplot2::guide_legend()
    )

  return(p_binned)
}
