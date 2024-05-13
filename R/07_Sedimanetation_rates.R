#----------------------------------------------------------#
#
#
#                     IPPD overview
#
#         Spatial distribution of sedimantation rates
#
#
#                       O. Mottl
#                         2024
#
#----------------------------------------------------------#

# Create summary of spatial distribution of sedimanetiation rates

#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

bin_value <- 75

#----------------------------------------------------------#
# 2. Load data  -----
#----------------------------------------------------------#

ippd_data_public <-
  readr::read_rds(
    paste0(current_dir, "/Data/Input/ippd_data_public-2021-12-15.rds")
  ) %>%
  purrr::pluck("data")

dplyr::glimpse(ippd_data_public)


#----------------------------------------------------------#
# 3. Estimate sedimatation rates -----
#----------------------------------------------------------#

data_sedimentation <-
  ippd_data_public %>%
  add_region_label() %>%
  dplyr::mutate(
    sedimentation_rate = purrr::map2_dbl(
      .x = depth_range,
      .y = age_range,
      .f = ~ {
        total_depth <-
          (.x[2] - .x[1]) %>%
          abs()

        total_age <-
          (.y[2] - .y[1]) %>%
          abs()

        (total_age / total_depth) %>%
          return()
      }
    )
  )

data_sedimentation_binned <-
  data_sedimentation %>%
  tidyr::drop_na(sedimentation_rate) %>%
  dplyr::mutate(sedimentation_rate_binned = sedimentation_rate) %>%
  get_binned(
    data_source = .,
    var = "sedimentation_rate_binned",
    bin_size = bin_value,
    mode = "data"
  ) %>%
  dplyr::mutate(
    sedimentation_rate_char = as.character(sedimentation_rate_binned)
  )

pal_region <-
  make_custom_palette(
    data = data_sedimentation_binned,
    var = "region_label",
    palette = sort(
      PrettyCols::PrettyColsPalettes[["Rainbow"]][[1]]
    )
  )

pal_sedimentation <-
  data_sedimentation_binned %>%
  dplyr::arrange(sedimentation_rate) %>%
  make_custom_palette(
    data = .,
    var = "sedimentation_rate_binned",
    palette = rev(PrettyCols::PrettyColsPalettes[["Teals"]][[1]])
  )


#----------------------------------------------------------#
# 3. Create figure -----
#----------------------------------------------------------#

p_sedimentation_map <-
  plot_data_distribution_by_numbers(
    data = data_sedimentation_binned,
    var = "sedimentation_rate",
    bin_size = bin_value,
    legend_n_col = 5,
    custom_palette = pal_sedimentation,
    point_alpha_outer = 0.5,
    coord_long = c(long_min, long_max), # [Config]
    coord_lat = c(lat_min, lat_max), # [Config]
    point_size = point_size, # [Config]
    text_size = text_size, # [Config]
    line_size = line_size, # [Config]
    map_color_fill = map_color_fill, # [Config]
    map_color_border = map_color_border, # [Config]
    caption_label = TRUE
  )

p_sedimentation_violin <-
  data_sedimentation %>%
  tidyr::drop_na(sedimentation_rate) %>%
  plot_data_violin_x_y(
    data = .,
    var_y = "sedimentation_rate",
    var_x = "region_label",
    custom_pallete = pal_region,
    text_size = text_size, # [Config]
    line_size = line_size, # [Config]
    default_color = gray_dark, # [Config]
    default_box_fill = gray_light, # [Config]
    legend_position = "none",
    caption_label = FALSE
  ) +
  ggplot2::labs(
    x = "",
    y = "Sedimentation rate (years / 1cm)"
  )

p_sedimentation_main <-
  cowplot::plot_grid(
    p_sedimentation_map,
    p_sedimentation_violin,
    nrow = 2,
    labels = "AUTO"
  )

p_sedimentation_bar <-
  plot_data_barplot(
    data = data_sedimentation_binned,
    var_x = "sedimentation_rate_binned",
    var_fill = "sedimentation_rate_char",
    custom_pallete = pal_sedimentation,
    text_size = text_size, # [Config]
    line_size = line_size, # [Config]
    bar_default_color = gray_dark, # [Config]
    legend_position = "none",
    caption_label = FALSE
  ) +
  ggplot2::labs(
    x = "Sedimentation rate (years / 1cm)"
  )


#----------------------------------------------------------#
# 4. Save -----
#----------------------------------------------------------#

purrr::walk(
  .x = c("pdf", "png"),
  .f = ~ ggplot2::ggsave(
    filename = paste0(
      current_dir, "/Outputs/Figures/Figure_08.", .x
    ),
    plot = p_sedimentation_main,
    width = image_width, # [Config]
    height = image_height * 1.25, # [Config]
    units = image_units, # [Config]
    dpi = image_dpi # [Config]
  )
)

purrr::walk(
  .x = c("pdf", "png"),
  .f = ~ ggplot2::ggsave(
    filename = paste0(
      current_dir, "/Outputs/Figures/Figure_A06.", .x
    ),
    plot = p_sedimentation_bar,
    width = image_width, # [Config]
    height = image_height, # [Config]
    units = image_units, # [Config]
    dpi = image_dpi # [Config]
  )
)
