get_binned <- function(
    data_source,
    var,
    bin_size = 5,
    start_from = c(0, "min"),
    limits = c(-Inf, Inf),
    mode = c("data", "breaks")) {
  mode <- match.arg(mode)
  start_from <- match.arg(start_from)

  val_limits <-
    data_source %>%
    dplyr::select(
      dplyr::any_of(var)
    ) %>%
    purrr::pluck(1) %>%
    range()

  if (
    start_from == "min"
  ) {
    sel_min <-
      c(min(limits), min(val_limits), 0) %>%
      max()
  } else {
    sel_min <- 0
  }

  sel_max <-
    c(max(limits), max(val_limits)) %>%
    min()

  if (
    mode == "breaks"
  ) {
    custom_breaks <-
      seq(
        from = sel_min,
        to = ceiling(sel_max / bin_size) * bin_size,
        by = bin_size
      )
    return(custom_breaks)
  }

  if (
    mode == "data"
  ) {
    data_binned <-
      data_source %>%
      dplyr::filter(is.na(get(var)) == FALSE) %>%
      dplyr::mutate(
        !!var := ifelse(get(var) > sel_max, sel_max, get(var)),
        !!var := ifelse(get(var) < sel_min, sel_min, get(var))
      ) %>%
      dplyr::mutate(
        !!var := floor(get(var) / bin_size) * bin_size
      )
    return(data_binned)
  }
}
