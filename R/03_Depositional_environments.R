#----------------------------------------------------------#
#
#
#                     IPPD overview
#
#         Distribution of depositional environment
#
#
#                       O. Mottl
#                         2024
#
#----------------------------------------------------------#

# Create summary of  distribution of various types of depositional environments

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
# 3. Harmonise environemnts -----
#----------------------------------------------------------#

vec_target_depenv <-
  c(
    "Coastal",
    "Fluvial",
    "Glacial",
    "Lacustrine",
    "Marine",
    "Terrestrial",
    "Wetlands",
    "Unknown"
  )

# Get Neotoma depositional environments
data_neotoma_depenv <- get_neotoma_depenv()

# check the Neotoma depositional environments harmisation for reference
if (
  isTRUE(verbose)
) {
  View(data_neotoma_depenv)
}

data_ippd_depenv_harmonised <-
  data_ippd %>%
  dplyr::mutate(
    depenv_harmonised = dplyr::case_when(
      .default = depositionalenvironment,
      depositionalenvironment %in% c(
        "Valley Mire",
        "Swamp",
        "Mire",
        "terrestrial, mire, bog,\tblanket bog",
        "terrestrial, mire, bog",
        "terrestrial, mire (i.e. peatland, >30cm peat)",
        "terrestrial, mire,swamp (forested wetland or peatland)",
        "terrestrial, mire, fen",
        "Fen"
      ) ~ "Wetlands",
      depositionalenvironment %in% c(
        "Cirque Lake",
        "Explosion Crater Lake",
        "Glacial Origin Lake",
        "Wind Origin Lake",
        "Interdunal Lake",
        "Natural Lake (Origin Unknown)",
        "Lava Flow Dammed Lake",
        "Drained Lake",
        "Natural Lake",
        "lacustrine, volcanic lake",
        "lacustrine, natural open-water, tectonic lake",
        "lacustrine, natural open-water, glacial origin",
        "lacustrine, drained lake",
        "Spring Mound",
        "Small Hollow",
        "Solution Origin Lake",
        "lacustrine",
        "lacustrine, natural open-water"
      ) ~ "Lacustrine",
      depositionalenvironment %in% c(
        "Archaeological", "terrestrial, soil, buried soil",
        "terrestrial, cave sediments",
        "terrestrial"
      ) ~ "Terrestrial",
      depositionalenvironment %in% c(
        "Fluvial", "Moraine Dammed Lake",
        "fluvial"
      ) ~ "Fluvial",
      depositionalenvironment %in% c(
        "Lacustrine Beach",
        "coastal, estuarine",
        "lacustrine, playa",
        "coastal"
      ) ~ "Coastal",
      depositionalenvironment == "marine" ~ "Marine",
      is.na(depositionalenvironment) ~ "Unknown"
    )
  ) %>%
  dplyr::mutate(
    depenv_harmonised = forcats::fct_relevel(
      depenv_harmonised,
      vec_target_depenv
    )
  )

data_ippd_depenv_harmonised_overview <-
  data_ippd_depenv_harmonised %>%
  dplyr::distinct(depositionalenvironment, depenv_harmonised)

assertthat::assert_that(
  data_ippd_depenv_harmonised_overview %>%
    dplyr::filter(!depenv_harmonised %in% vec_target_depenv) %>%
    nrow() == 0,
  msg = "Some depositional environments are not harmonised"
)

pal_dep_env <-
  make_custom_palette(
    data = data_ippd_depenv_harmonised,
    var = "depenv_harmonised",
    palette = sort(PrettyCols::PrettyColsPalettes[["Neon"]][[1]])
  )


#----------------------------------------------------------#
# 4. Create figures -----
#----------------------------------------------------------#

p_dep_env_map <-
  plot_data_distribution_by_var(
    data = data_ippd_depenv_harmonised,
    var = "depenv_harmonised",
    point_alpha_outer = 0.5,
    custom_palette = pal_dep_env,
    coord_long = c(long_min, long_max), # [Config]
    coord_lat = c(lat_min, lat_max), # [Config]
    point_size = point_size, # [Config]
    text_size = text_size, # [Config]
    line_size = line_size, # [Config]
    map_color_fill = map_color_fill, # [Config]
    map_color_border = map_color_border, # [Config]"
    caption_label = FALSE
  )

p_dep_env_bar <-
  plot_data_barplot(
    data = data_ippd_depenv_harmonised,
    var_x = "depenv_harmonised",
    y_axis_limits = c(0, 90),
    x_label_angle = 90,
    x_label_hjust = 1,
    x_label_vjust = 0.5,
    custom_palette = pal_dep_env,
    text_size = text_size, # [Config]
    line_size = line_size, # [Config]
    bar_default_color = gray_dark, # [Config]
    legend_position = "none",
    caption_label = FALSE
  ) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank()
  )

p_dep_env_main <-
  cowplot::plot_grid(
    p_dep_env_map,
    p_dep_env_bar,
    labels = "AUTO",
    ncol = 1,
    rel_heights = c(1, 0.5)
  )

p_dep_env_bar_full <-
  plot_data_barplot(
    data = data_ippd_depenv_harmonised,
    var_x = "depositionalenvironment",
    var_fill = "depenv_harmonised",
    y_axis_limits = c(0, 50),
    x_label_angle = 90,
    x_label_hjust = 1,
    x_label_vjust = 0.5,
    custom_palette = pal_dep_env,
    text_size = text_size, # [Config]
    line_size = line_size, # [Config]
    bar_default_color = gray_dark, # [Config]
    legend_position = "none",
    caption_label = FALSE
  ) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank()
  )

p_dep_env_bar_long <-
  get_binned(
    data_source = data_ippd_depenv_harmonised,
    var = "long",
    bin_size = 10,
    start_from = "min",
  ) %>%
  plot_data_barplot(
    data = .,
    var_x = "long",
    var_fill = "depenv_harmonised",
    custom_palette = pal_dep_env,
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

p_dep_env_bar_lat <-
  data_ippd_depenv_harmonised %>%
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
    var_fill = "depenv_harmonised",
    custom_palette = pal_dep_env,
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

p_dep_env_bar_merge <-
  cowplot::plot_grid(
    p_dep_env_bar_full,
    cowplot::plot_grid(
      p_dep_env_bar_long,
      p_dep_env_bar_lat,
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
      current_dir, "/Outputs/Figures/Figure_04.", .x
    ),
    plot = p_dep_env_main,
    width = image_width, # [Config]
    height = image_height * 1.5, # [Config]
    units = image_units, # [Config]
    dpi = image_dpi # [Config]
  )
)

data_ippd_depenv_harmonised_overview %>%
  dplyr::arrange(depenv_harmonised, depositionalenvironment) %>%
  rlang::set_names(
    nm = c(
      "Original depositional environment",
      "Depositional environment - broader grouping"
    )
  ) %>%
  readr::write_csv(
    x = .,
    file = paste0(current_dir, "/Outputs/Tables/Table_S1.csv")
  )

purrr::walk(
  .x = c("pdf", "png"),
  .f = ~ ggplot2::ggsave(
    filename = paste0(
      current_dir, "/Outputs/Figures/Figure_A03.", .x
    ),
    plot = p_dep_env_bar_merge,
    width = image_width, # [Config]
    height = image_height * 1.2, # [Config]
    units = image_units, # [Config]
    dpi = image_dpi # [Config]
  )
)
