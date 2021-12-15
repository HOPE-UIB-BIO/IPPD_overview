#----------------------------------------------------------#
#
#
#                   HOPE master repo 
#
#                         Figures
#                 
#
#   O. Mottl, S. Flantua, K. Bhatta, V. Felde, A. Seddon 
#                         2021
#
#----------------------------------------------------------#

# Create output figures

#----------------------------------------------------------#
# 1. Set up run -----
#----------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here("R/00_Config_file.R"))


#----------------------------------------------------------#
# 2. Load data  -----
#----------------------------------------------------------#

ippd_data_public <- 
  readr::read_rds(
    paste0(current_dir, "/Data/Input/ippd_data_public-2021-12-15.rds")) %>% 
  purrr::pluck("data")

dplyr::glimpse(ippd_data_public)

#----------------------------------------------------------#
# 4. Sedimentation rate -----
#----------------------------------------------------------#

data_for_figures  <-
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
        
        (total_age/total_depth) %>% 
          return()
          
      }))


#----------------------------------------------------------#
# 5. Figures -----
#----------------------------------------------------------#


(p_region <- 
   plot.data.distribution.by.var(
     data = data_for_figures,
     var = "region",
     title = "Geographical region",
     coord_long = c(long_min, long_max), #[Config]
     coord_lat = c(lat_min, lat_max) #[Config]
   ))

(p_dep_env <- 
    plot.data.distribution.by.var(
      data = data_for_figures,
      var = "depositionalenvironment",
      title = "Depositional environment",
      coord_long = c(long_min, long_max), #[Config]
      coord_lat = c(lat_min, lat_max) #[Config]
    ))


(p_percentage <-
    plot.data.distribution.by.var(
      data = data_for_figures,
      var = "pollen_percentage",
      title = "Is pollen in percentage?",
      coord_long = c(long_min, long_max), #[Config]
      coord_lat = c(lat_min, lat_max) #[Config]
    ))

(p_source <- 
    plot.data.distribution.by.var(
      data = data_for_figures,
      var = "source_of_data",
      title = "What is the source of data?",
      coord_long = c(long_min, long_max), #[Config]
      coord_lat = c(lat_min, lat_max) #[Config]
    ))

(p_n_levels <-
    plot.data.distribution.by.numbers(
      data = data_for_figures,
      var = "n_sample_counts",
      title = "Number of levels", 
      bin_size = 50,
      coord_long = c(long_min, long_max), #[Config]
      coord_lat = c(lat_min, lat_max) #[Config]
    ))

(p_sedimentation <-
    plot.data.distribution.by.numbers(
      data = data_for_figures,
      var = "sedimentation_rate",
      title = "Sedimentation rate [years/1cm]", 
      bin_size = 50,
      coord_long = c(long_min, long_max), #[Config]
      coord_lat = c(lat_min, lat_max) #[Config]
    ))

(p_n_chron <-
    plot.data.distribution.by.numbers(
      data = data_for_figures,
      var = "n_chron_control",
      title = "Number of chron.control points",
      bin_size = 5,
      coord_long = c(long_min, long_max), #[Config]
      coord_lat = c(lat_min, lat_max) #[Config]
    ))


(p_age <-
    plot.data.distribution.by.age(
      data = data_for_figures,
      coord_long = c(long_min, long_max), #[Config]
      coord_lat = c(lat_min, lat_max), #[Config]
      break_size = 10e3,
      limits = c(0, 50e3)
    ))

#----------------------------------------------------------#
# 5. Save -----
#----------------------------------------------------------#

c(
  "p_region",
  "p_dep_env",
  "p_percentage",
  "p_source",
  "p_n_levels",
  "p_sedimentation",
  "p_n_chron",
  "p_age"
) %>% 
  purrr::walk(
    .x = ., 
    .f = ~ ggplot2::ggsave(
      filename = paste0(
        current_dir, "/Outputs/Figures/Distribution/", .x, ".pdf"),
      plot = get(.x),
      width = image_width, #[Config]
      height = image_height, #[Config]
      units = image_units, #[Config]
      dpi = image_dpi)) #[Config]

