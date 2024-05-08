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
# 3. Create figure -----
#----------------------------------------------------------#

p_region <-
  plot_data_distribution_spatial(
    data = ippd_data_public,
    var = "region",
    coord_long = c(long_min, long_max), # [Config]
    coord_lat = c(lat_min, lat_max), # [Config]
    point_size = point_size, # [Config]
    text_size = text_size, # [Config]
    line_size = line_size, # [Config]
    map_color_fill = map_color_fill, # [Config]
    map_color_border = map_color_border, # [Config]"
    caption_label = TRUE
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
    plot = p_region,
    width = image_width, # [Config]
    height = image_height, # [Config]
    units = image_units, # [Config]
    dpi = image_dpi # [Config]
  )
)