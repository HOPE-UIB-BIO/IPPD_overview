plot_data_barplot <- function(
    data,
    var_x,
    var_fill,
    custom_palette,
    palette_set = "Set1",
    bar_default_color = "grey30",
    bar_alpha = 1,
    text_size = 10,
    line_size = 0.1,
    x_label_angle = 0,
    x_label_hjust = 0.5,
    x_label_vjust = 0.5,
    y_axis_limits = NULL,
    plot_number_of_records = TRUE,
    legend_n_col = 3,
    legend_position = "bottom",
    caption_label = TRUE) {
  if (
    missing(var_fill)
  ) {
    var_fill <- var_x
  }

  if (
    missing(custom_palette)
  ) {
    var_list <-
      data %>%
      dplyr::distinct(get(var_fill)) %>%
      purrr::pluck(1)

    var_list_length <- length(var_list)

    get_palette_set <-
      grDevices::colorRampPalette(
        RColorBrewer::brewer.pal(
          min(
            max(var_list_length, 3),
            8
          ),
          palette_set
        )
      )

    custom_palette <- get_palette_set(var_list_length)

    names(custom_palette) <- var_list
  }

  data_summed <-
    data %>%
    dplyr::group_by(get(var_x), get(var_fill)) %>%
    dplyr::summarise(
      .groups = "drop",
      N = dplyr::n()
    ) %>%
    rlang::set_names(
      nm = c(var_x, var_fill, "N")
    )

  if (
    var_x == var_fill
  ) {
    data_summed <-
      data_summed[-2]
  }

  p_barplot <-
    data_summed %>%
    ggplot2::ggplot(
      ggplot2::aes(
        y = N,
        x = get(var_x),
        label = N,
        fill = get(var_fill)
      )
    ) +
    ggplot2::geom_bar(
      stat = "identity",
      color = bar_default_color,
      linewidth = line_size,
      alpha = bar_alpha
    ) +
    ggplot2::scale_color_manual(values = custom_palette) +
    ggplot2::scale_fill_manual(values = custom_palette) +
    ggplot2::theme(
      text = ggplot2::element_text(
        size = text_size
      ),
      plot.title = ggplot2::element_text(
        size = text_size
      ),
      axis.text.x = ggplot2::element_text(
        size = text_size,
        angle = x_label_angle,
        hjust = x_label_hjust, 
        vjust = x_label_vjust
      ),
      line = ggplot2::element_line(
        linewidth = line_size
      ),
      legend.title = ggplot2::element_blank(),
      legend.position = legend_position
    ) +
    ggplot2::labs(
      y = "Number of records",
      x = var_x
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        override.aes = list(
          alpha = 1
        ),
        ncol = legend_n_col
      )
    )

  if (
    isTRUE(plot_number_of_records)
  ) {
    p_barplot <-
      p_barplot +
      ggplot2::geom_text(
        data = data_summed,
        ggplot2::aes(
          y = N,
          x = get(var_x),
          label = N
        ),
        size = text_size / (ggplot2::.pt),
        vjust = 0,
        nudge_y = 0
      )
  }

  if (
    !is.null(y_axis_limits)
  ) {
    p_barplot <-
      p_barplot +
      ggplot2::coord_cartesian(
        ylim = c(y_axis_limits)
      )
  }

  if (
    isTRUE(caption_label)
  ) {
    p_barplot <-
      p_barplot +
      ggplot2::labs(
        caption = paste0(
          "Distribution of the records (N = ", nrow(data), ")"
        )
      )
  }

  return(p_barplot)
}
