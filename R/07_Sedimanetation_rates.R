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

data_sedimentation$depth_range %>%
  unlist() %>%
  summary()

data_sedimentation$age_range %>%
  unlist() %>%
  summary()

data_sedimentation$sedimentation_rate %>%
  summary()

is.na(data_sedimentation$sedimentation_rate) %>% which()


data_sedimentation$age_range[110]


#----------------------------------------------------------#
# 3. Create figure -----
#----------------------------------------------------------#

p_sedimentation_map <-
  data_sedimentation %>%
  tidyr::drop_na(sedimentation_rate) %>%
  plot_data_distribution_by_numbers(
    data = .,
    var = "sedimentation_rate",
    bin_size = 50,
    legend_n_col = 10,
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
    var_x = "region",
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

p_sedimentation <-
  cowplot::plot_grid(
    p_sedimentation_map,
    p_sedimentation_violin,
    nrow = 2,
    labels = "AUTO"
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
    plot = p_sedimentation,
    width = image_width, # [Config]
    height = image_height * 1.25, # [Config]
    units = image_units, # [Config]
    dpi = image_dpi # [Config]
  )
)
