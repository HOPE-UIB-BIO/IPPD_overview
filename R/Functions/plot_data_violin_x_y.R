plot_data_violin_x_y <- function(
    data,
    var_x,
    var_y,
    custom_pallete,
    pallete_set = "Set1",
    default_color = "grey30",
    default_box_fill = "white",
    line_alpha_violin = 1,
    line_alpha_box = 1,
    text_size = 10,
    line_size = 0.1,
    legend_n_col = 3,
    legend_position = "bottom",
    caption_label = TRUE) {
  if (
    missing(custom_pallete)
  ) {
    var_list <-
      data %>%
      dplyr::distinct(get(var_x)) %>%
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

  p_barplot <-
    data %>%
    ggplot2::ggplot(
      ggplot2::aes(
        y = get(var_y),
        x = get(var_x),
        fill = get(var_x)
      )
    ) +
    ggplot2::geom_violin(
      color = default_color,
      linewidth = line_size,
      alpha = line_alpha_violin
    ) +
    ggplot2::geom_boxplot(
      fill = default_box_fill,
      color = default_color,
      linewidth = line_size,
      alpha = line_alpha_box,
      width = 0.15,
      outlier.shape = NA
    ) +
    ggplot2::scale_color_manual(values = custom_pallete) +
    ggplot2::scale_fill_manual(values = custom_pallete) +
    ggplot2::theme(
      text = ggplot2::element_text(size = text_size),
      plot.title = ggplot2::element_text(size = text_size),
      axis.text.x = ggplot2::element_text(size = text_size, angle = 45, hjust = 1, vjust = 1),
      line = ggplot2::element_line(linewidth = line_size),
      legend.title = ggplot2::element_blank(),
      legend.position = legend_position
    ) +
    ggplot2::labs(
      x = var_x,
      y = var_y
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
