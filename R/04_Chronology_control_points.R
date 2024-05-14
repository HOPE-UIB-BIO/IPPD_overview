#----------------------------------------------------------#
#
#
#                     IPPD overview
#
#       Spatial distribution of chronology control points
#
#
#                       O. Mottl
#                         2024
#
#----------------------------------------------------------#

# Create summary of spatial distribution of number of chronology control points

#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

bin_value <- 5

verbose <- FALSE


#----------------------------------------------------------#
# 2. Load data  -----
#----------------------------------------------------------#

ippd_data_public <-
  readr::read_rds(
    paste0(current_dir, "/Data/Input/ippd_data_public-2021-12-15.rds")
  ) %>%
  purrr::pluck("data")

if (
  isTRUE(verbose)
) {
  dplyr::glimpse(ippd_data_public)
}


#----------------------------------------------------------#
# 3. Create figure -----
#----------------------------------------------------------#

data_chron_control <-
  ippd_data_public %>%
  tidyr::drop_na(n_chron_control) %>%
  dplyr::mutate(n_chron_control_binned = n_chron_control) %>%
  get_binned(
    data_source = .,
    var = "n_chron_control_binned",
    bin_size = bin_value,
    mode = "data"
  ) %>%
  dplyr::mutate(
    n_chron_control_char = as.character(n_chron_control_binned)
  )

pal_chron_control <-
  data_chron_control %>%
  dplyr::arrange(n_chron_control) %>%
  make_custom_palette(
    data = .,
    var = "n_chron_control_binned",
    palette = rev(PrettyCols::PrettyColsPalettes[["Blues"]][[1]])
  )

p_chron_con_count_map <-
  plot_data_distribution_by_numbers(
    data = data_chron_control,
    var = "n_chron_control",
    bin_size = bin_value,
    custom_palette = pal_chron_control,
    point_alpha_outer = 0.5,
    coord_long = c(long_min, long_max), # [Config]
    coord_lat = c(lat_min, lat_max), # [Config]
    point_size = point_size, # [Config]
    text_size = text_size, # [Config]
    line_size = line_size, # [Config]
    map_color_fill = map_color_fill, # [Config]
    map_color_border = map_color_border, # [Config]
    caption_label = TRUE,
    legend_n_col = 5
  )

p_chron_con_count_bar <-
  plot_data_barplot(
    data = data_chron_control,
    var_x = "n_chron_control_binned",
    var_fill = "n_chron_control_char",
    custom_palette = pal_chron_control,
    text_size = text_size, # [Config]
    line_size = line_size, # [Config]
    bar_default_color = gray_dark, # [Config]
    legend_position = "none",
    caption_label = FALSE
  ) +
  ggplot2::labs(
    x = "Number of chronology control points"
  )


#----------------------------------------------------------#
# 4. Save -----
#----------------------------------------------------------#

purrr::walk(
  .x = c("pdf", "png"),
  .f = ~ ggplot2::ggsave(
    filename = paste0(
      current_dir, "/Outputs/Figures/Figure_05.", .x
    ),
    plot = p_chron_con_count_map,
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
      current_dir, "/Outputs/Figures/Figure_A04.", .x
    ),
    plot = p_chron_con_count_bar,
    width = image_width, # [Config]
    height = image_height, # [Config]
    units = image_units, # [Config]
    dpi = image_dpi # [Config]
  )
)
