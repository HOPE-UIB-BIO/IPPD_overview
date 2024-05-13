add_region_label <- function(data_source) {
  data_source %>%
    dplyr::mutate(
      region_label = dplyr::case_when(
        .default = region,
        region == "Australia_above_500m" ~ "Australia above 500m",
        region == "New_Zealand_and_Islands_So" ~ "New Zealand and Islands South",
        region == "Australia_tropical" ~ "Australia tropical",
        region == "New_Guinea_highland" ~ "New Guinea highland",
        region == "Australia_Tasmania" ~ "Australia Tasmania",
        region == "Australia_east_coast" ~ "Australia east coast",
        region == "Australia_south_coast" ~ "Australia south coast",
        region == "New_Guinea_lowland" ~ "New Guinea lowland",
        region == "Australia_arid" ~ "Australia arid",
        region == "West_Pacific" ~ "West Pacific",
        region == "Maritime_SE_Asia" ~ "Maritime SE Asia",
        region == "New_Caledonia" ~ "New Caledonia",
        region == "New_Zealand_North" ~ "New Zealand North",
        region == "Australia_SW" ~ "Australia SW"
      )
    ) %>%
    dplyr::mutate(
      region_label = as.factor(region_label)
    ) %>%
    return()
}