make_custom_palette <- function(data, var, palette, ...) {
  var_list <-
    data %>%
    dplyr::distinct(get(var)) %>%
    purrr::pluck(1)

  var_list_length <- length(var_list)

  if (
    missing(palette)
  ) {
    palette <-
      RColorBrewer::brewer.pal(
        min(
          max(var_list_length, 3),
          8
        ),
        ...
      )
  }

  get_palette_set <-
    grDevices::colorRampPalette(palette)

  custom_pallete <- get_palette_set(var_list_length)

  names(custom_pallete) <- var_list

  return(custom_pallete)
}
