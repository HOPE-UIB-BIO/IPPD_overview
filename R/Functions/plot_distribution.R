plot_distribution <- function(
    data_source,
    var,
    var_x_name,
    default_color = "black",
    text_size = 10,
    line_size = 0.1) {
  data_source %>%
    dplyr::filter(var_name %in% c(var)) %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = value
      )
    ) +
    ggplot2::geom_density(
      fill = default_color,
      alpha = 0.5
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(size = text_size),
      plot.title = ggplot2::element_text(size = text_size),
      line = ggplot2::element_line(linewidth = line_size),
      legend.title = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = var_x_name
    )
}
