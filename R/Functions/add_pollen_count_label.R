add_pollen_count_label <- function(data_source) {
  data_source %>%
    dplyr::mutate(
      pollen_percentage_label = dplyr::case_when(
        .default = pollen_percentage,
        pollen_percentage == "raw count" ~ "Raw count",
        pollen_percentage == "percentage count" ~ "Percentage count",
        pollen_percentage == "percentage digitised" ~ "Percentage digitised",
        pollen_percentage == "unknown %" ~ "Percentage unknown",
        pollen_percentage == "other" ~ "Other"
      )
    ) %>%
    dplyr::mutate(
      pollen_percentage_label = forcats::fct_relevel(
        pollen_percentage_label,
        c(
          "Raw count",
          "Percentage count",
          "Percentage digitised",
          "Percentage unknown",
          "Other"
        )
      )
    ) %>%
    return()
}
