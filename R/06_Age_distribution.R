#----------------------------------------------------------#
#
#
#                     IPPD overview
#
#              Distribution of records by age
#
#
#                       O. Mottl
#                         2024
#
#----------------------------------------------------------#

# Create summary of distribution of records by age

#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

verbose <- FALSE

sel_limits <- c(0, 50e3)

bin_value <- 10e3

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

ippd_data_public %>%
  dplyr::pull("age_range") %>%
  unlist() %>%
  sort()

data_age <-
  ippd_data_public %>%
  dplyr::mutate(
    age_young = purrr::map_dbl(
      .x = age_range,
      .f = ~ min(.x)
    ),
    age_old = purrr::map_dbl(
      .x = age_range,
      .f = ~ max(.x)
    ),
    age_mid = c(age_young + age_old) / 2,
    age_lenght = abs(age_young - age_old)
  )

data_age  %>% 
  dplyr::select(age_young, age_old, age_mid, age_lenght) %>% 
  dplyr::glimpse()

tidyr::drop_na(n_sample_counts) %>%
  dplyr::mutate(n_sample_counts_binned = n_sample_counts) %>%
  get_binned(
    data_source = .,
    var = "n_sample_counts_binned",
    bin_size = bin_value,
    mode = "data"
  ) %>%
  dplyr::mutate(
    n_sample_counts_char = as.character(n_sample_counts_binned)
  )


p_age_distr <-
  plot_data_distribution_by_age(
    data = ippd_data_public,
    bin_size = bin_value,
    limits = sel_limits,
    coord_long = c(long_min, long_max), # [Config]
    coord_lat = c(lat_min, lat_max), # [Config]
    point_size = point_size, # [Config]
    text_size = text_size, # [Config]
    line_size = line_size, # [Config]
    map_color_fill = map_color_fill, # [Config]
    map_color_border = map_color_border # [Config]
  )


#----------------------------------------------------------#
# 4. Save -----
#----------------------------------------------------------#

purrr::walk(
  .x = c("pdf", "png"),
  .f = ~ ggplot2::ggsave(
    filename = paste0(
      current_dir, "/Outputs/Figures/Figure_07.", .x
    ),
    plot = p_age_distr,
    width = image_width, # [Config]
    height = image_height * 1.25, # [Config]
    units = image_units, # [Config]
    dpi = image_dpi # [Config]
  )
)
