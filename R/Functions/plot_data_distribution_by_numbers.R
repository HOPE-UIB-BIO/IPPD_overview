plot_data_distribution_by_numbers <- function(
    data,
    var,
    vec_breaks,
    custom_palette,
    bin_size = 5,
    point_alpha_outer = 0.3,
    legend_n_col = 3,
    limits = c(-Inf, Inf),
    start_from = c(0, "min"),
    ...) {
  start_from <- match.arg(start_from)

  if (
    missing(vec_breaks)
  ) {
    vec_breaks <-
      get_binned(
        data_source = data,
        var = var,
        bin_size = bin_size,
        limits = limits,
        start_from = start_from,
        mode = "breaks"
      )
  }

  p_spatial <-
    plot_data_distribution_spatial(
      data = data,
      var = var,
      var_as_factor = FALSE,
      ...
    )

  p_binned <-
    p_spatial +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        size = get(var),
        col = get(var)
      ),
      shape = 1,
      alpha = point_alpha_outer
    )

  if (
    missing(custom_palette)
  ) {
    p_color <-
      p_binned +
      ggplot2::scale_color_viridis_c(
        breaks = vec_breaks,
        labels = vec_breaks,
        direction = -1,
        option = "plasma"
      )
  } else {
    p_color <-
      p_binned +
      ggplot2::scale_color_gradientn(
        colours = custom_palette,
        breaks = vec_breaks,
        labels = vec_breaks,
        guide = "legend"
      )
  }

  p_size <-
    p_color +
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

  return(p_size)
}
