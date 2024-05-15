#----------------------------------------------------------#
#
#
#                     IPPD overview
#
#              Spatial distribution of regions
#
#
#                       O. Mottl
#                         2024
#
#----------------------------------------------------------#

# Create summary of spatial distribution of regions

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
  )

if (
  isTRUE(verbose)
) {
  dplyr::glimpse(data_ippd)
}


#----------------------------------------------------------#
# 3. Create figure -----
#----------------------------------------------------------#

data_region <-
  data_ippd %>%
  add_region_label()

pal_region <-
  make_custom_palette(
    data = data_region,
    var = "region_label",
    palette = sort(PrettyCols::PrettyColsPalettes[["Rainbow"]][[1]])
  )

p_region_map <-
  plot_data_distribution_by_var(
    data = data_region,
    var = "region_label",
    coord_long = c(long_min, long_max), # [Config]
    coord_lat = c(lat_min, lat_max), # [Config]
    point_size = point_size, # [Config]
    text_size = text_size, # [Config]
    line_size = line_size, # [Config]
    map_color_fill = map_color_fill, # [Config]
    map_color_border = map_color_border, # [Config]
    custom_palette = pal_region,
    caption_label = TRUE
  )

p_region_bar <-
  plot_data_barplot(
    data = data_region,
    var_x = "region_label",
    y_axis_limits = c(0, 45),
    x_label_angle = 90,
    x_label_hjust = 1,
    x_label_vjust = 0.5,
    text_size = text_size, # [Config]
    line_size = line_size, # [Config]
    bar_default_color = gray_dark, # [Config]
    legend_position = "none",
    custom_palette = pal_region,
    caption_label = FALSE
  ) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank()
  )

p_region_bar_long <-
  get_binned(
    data_source = data_region,
    var = "long",
    bin_size = 10,
    start_from = "min",
  ) %>%
  plot_data_barplot(
    data = .,
    var_x = "long",
    var_fill = "region_label",
    text_size = text_size, # [Config]
    line_size = line_size, # [Config]
    bar_default_color = gray_dark, # [Config]
    legend_position = "none",
    custom_palette = pal_region,
    x_label_angle = 0,
    plot_number_of_records = FALSE,
    caption_label = FALSE
  ) +
  ggplot2::labs(
    x = "Longitude"
  )

p_region_bar_lat <-
  data_region %>%
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
    var_fill = "region_label",
    text_size = text_size, # [Config]
    line_size = line_size, # [Config]
    bar_default_color = gray_dark, # [Config]
    legend_position = "none",
    custom_palette = pal_region,
    x_label_angle = 0,
    plot_number_of_records = FALSE,
    caption_label = FALSE
  ) +
  ggplot2::labs(
    x = "Latitude"
  )

p_region_bar_merge <-
  cowplot::plot_grid(
    p_region_bar,
    cowplot::plot_grid(
      p_region_bar_long,
      p_region_bar_lat,
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
      current_dir, "/Outputs/Figures/Figure_01.", .x
    ),
    plot = p_region_map,
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
      current_dir, "/Outputs/Figures/Figure_A01.", .x
    ),
    plot = p_region_bar_merge,
    width = image_width, # [Config]
    height = image_height, # [Config]
    units = image_units, # [Config]
    dpi = image_dpi # [Config]
  )
)
