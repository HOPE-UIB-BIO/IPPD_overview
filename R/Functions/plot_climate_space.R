plot_climate_space <- function(data_source_records,
                               data_source_total,
                               var_x,
                               var_y,
                               var_x_name,
                               var_y_name,
                               custom_palette,
                               default_point_color = "black",
                               point_size = 1,
                               point_alpha_inner = 1,
                               text_size = 10,
                               line_size = 0.1,
                               legend_position = "none",
                               legend_n_col = 3) {
  data_records_sub <-
    data_source_records %>%
    dplyr::filter(var_name %in% c(var_x, var_y)) %>%
    tidyr::pivot_wider(
      names_from = var_name,
      values_from = value,
      values_fn = list,
      id_expand = TRUE
    ) %>%
    tidyr::unnest(cols = dplyr::all_of(c(var_x, var_y)))

  data_total_sub <-
    data_source_total %>%
    dplyr::filter(var_name %in% c(var_x, var_y)) %>%
    tidyr::pivot_wider(
      names_from = var_name,
      values_from = value,
      values_fn = list,
      id_expand = TRUE
    ) %>%
    tidyr::unnest(cols = dplyr::all_of(c(var_x, var_y)))


  p_clim_space <-
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = get(var_x),
        y = get(var_y)
      )
    ) +
    ggplot2::geom_point(
      data = data_total_sub,
      size = point_size,
      alpha = point_alpha_inner,
      col = default_point_color,
    ) +
    ggplot2::geom_point(
      data = data_records_sub,
      mapping = ggplot2::aes(
        col = region
      ),
      size = point_size,
      alpha = point_alpha_inner
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(size = text_size),
      plot.title = ggplot2::element_text(size = text_size),
      line = ggplot2::element_line(linewidth = line_size),
      legend.title = ggplot2::element_blank(),
      legend.position = legend_position
    ) +
    ggplot2::labs(
      x = var_x_name,
      y = var_y_name
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

  return(p_clim_space)
}