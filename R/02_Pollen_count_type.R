#----------------------------------------------------------#
#
#
#                     IPPD overview
#
#              Spatial distribution of count types
#
#
#                       O. Mottl
#                         2024
#
#----------------------------------------------------------#

# Create summary of spatial distribution of various types of pollen counts

#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

verbose <- FALSE

#----------------------------------------------------------#
# 2. Load data  -----
#----------------------------------------------------------#

data_ippd <-
  readr::read_rds(
    paste0(current_dir, "/Data/Input/ippd_data_public-2021-12-15.rds")
  ) %>%
  purrr::pluck("data")

if (
  isTRUE(verbose)
) {
  dplyr::glimpse(data_ippd)
}


#----------------------------------------------------------#
# 3. Create figure -----
#----------------------------------------------------------#

data_pollen_count <-
  data_ippd %>%
  add_pollen_count_label()

pal_pollen_count <-
  make_custom_palette(
    data = data_pollen_count,
    var = "pollen_percentage_label",
    palette = sort(PrettyCols::PrettyColsPalettes[["Light"]][[1]])
  )

p_count_map <-
  plot_data_distribution_by_var(
    data = data_pollen_count,
    var = "pollen_percentage_label",
    point_alpha_outer = 0.5,
    custom_palette = pal_pollen_count,
    coord_long = c(long_min, long_max), # [Config]
    coord_lat = c(lat_min, lat_max), # [Config]
    point_size = point_size, # [Config]
    text_size = text_size, # [Config]
    line_size = line_size, # [Config]
    map_color_fill = map_color_fill, # [Config]
    map_color_border = map_color_border, # [Config]"
    caption_label = FALSE
  )

p_count_bar <-
  plot_data_barplot(
    data = data_pollen_count,
    var_x = "pollen_percentage_label",
    y_axis_limits = c(0, 140),
    x_label_angle = 90,
    x_label_hjust = 1,
    x_label_vjust = 0.5,
    custom_palette = pal_pollen_count,
    text_size = text_size, # [Config]
    line_size = line_size, # [Config]
    bar_default_color = gray_dark, # [Config]
    legend_position = "none",
    caption_label = FALSE
  ) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank()
  )

p_count_bar_long <-
  get_binned(
    data_source = data_pollen_count,
    var = "long",
    bin_size = 10,
    start_from = "min",
  ) %>%
  plot_data_barplot(
    data = .,
    var_x = "long",
    var_fill = "pollen_percentage_label",
    custom_palette = pal_pollen_count,
    text_size = text_size, # [Config]
    line_size = line_size, # [Config]
    bar_default_color = gray_dark, # [Config]
    legend_position = "none",
    x_label_angle = 0,
    plot_number_of_records = FALSE,
    caption_label = FALSE
  ) +
  ggplot2::labs(
    x = "Longitude"
  )

p_count_bar_lat <-
  data_pollen_count %>%
  dplyr::mutate(
    lat_pos = lat * (-1)
  ) %>%
  get_binned(
    data_source = .,
    var = "lat_pos",
    bin_size = 5,
    start_from = "min",
  ) %>%
  dplyr::mutate(
    lat = lat_pos * (-1)
  ) %>%
  plot_data_barplot(
    data = .,
    var_x = "lat",
    var_fill = "pollen_percentage_label",
    custom_palette = pal_pollen_count,
    text_size = text_size, # [Config]
    line_size = line_size, # [Config]
    bar_default_color = gray_dark, # [Config]
    legend_position = "none",
    x_label_angle = 0,
    plot_number_of_records = FALSE,
    caption_label = FALSE
  ) +
  ggplot2::labs(
    x = "Latitude"
  )

p_count_bar_merge <-
  cowplot::plot_grid(
    p_count_bar,
    cowplot::plot_grid(
      p_count_bar_long,
      p_count_bar_lat,
      labels = c("B", "C"),
      nrow = 1
    ),
    labels = c("A", ""),
    ncol = 1,
    rel_heights = c(1, 0.5)
  )

#----------------------------------------------------------#
# 4. Save -----
#----------------------------------------------------------#

purrr::walk(
  .x = c("pdf", "png"),
  .f = ~ ggplot2::ggsave(
    filename = paste0(
      current_dir, "/Outputs/Figures/Figure_02.", .x
    ),
    plot = p_count_map,
    width = image_width, # [Config]
    height = image_height, # [Config]
    units = image_units, # [Config]
    dpi = image_dpi # [Config]
  )
)

purrr::walk(
  .x = c("pdf", "png"),
  .f = ~ ggplot2::ggsave(
    filename = paste0(
      current_dir, "/Outputs/Figures/Figure_A02.", .x
    ),
    plot = p_count_bar_merge,
    width = image_width, # [Config]
    height = image_height, # [Config]
    units = image_units, # [Config]
    dpi = image_dpi # [Config]
  )
)
