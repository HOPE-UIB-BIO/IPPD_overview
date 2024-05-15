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

limit_values <- function(x, limit_vec) {
  x[x < limit_vec[1]] <- limit_vec[1]
  x[x > limit_vec[2]] <- limit_vec[2]
  return(x)
}

data_age <-
  data_ippd %>%
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
  ) %>%
  tidyr::drop_na(age_young, age_old, age_mid, age_lenght) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = c(age_young, age_old, age_mid, age_lenght),
      .fns = ~ limit_values(.x, sel_limits),
      .names = "{.col}_lim"
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = c(age_young_lim, age_old_lim, age_mid_lim, age_lenght_lim),
      .fns = ~ floor(.x / bin_value) * bin_value,
      .names = "{.col}_binned"
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = c(
        age_young_lim_binned, age_old_lim_binned,
        age_mid_lim_binned, age_lenght_lim_binned
      ),
      .fns = ~ as.character(.x),
      .names = "{.col}_char"
    )
  )

vec_age_bin <-
  data_age %>%
  dplyr::select(
    age_young_lim_binned,
    age_old_lim_binned,
    age_mid_lim_binned,
    age_lenght_lim_binned
  ) %>%
  unlist() %>%
  unique() %>%
  sort() %>%
  as.character()

pal_age <-
  grDevices::colorRampPalette(
    rev(PrettyCols::PrettyColsPalettes[["Tangerines"]][[1]])
  )(length(vec_age_bin)) %>%
  rlang::set_names(vec_age_bin)

p_age_distr <-
  plot_data_distribution_by_age(
    data = data_age,
    bin_size = bin_value,
    limits = sel_limits,
    vec_breaks = as.numeric(vec_age_bin),
    custom_palette = pal_age,
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
    height = image_height, # [Config]
    units = image_units, # [Config]
    dpi = image_dpi # [Config]
  )
)
