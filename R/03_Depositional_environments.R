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
# 3. Harmonise environemnts -----
#----------------------------------------------------------#

vec_target_depenv <-
  c(
    "Coastal",
    "Drained Lake",
    "Fluvial",
    "Glacial",
    "Lacustrine",
    "Marine",
    "Small Hollow",
    "Spring Mound",
    "Terrestrial",
    "Wetlands",
    "Unknown"
  )

# Get Neotoma depositional environments
data_neotoma_depenv <- get_neotoma_depenv()

# check the Neotoma depositional environments harmisation for reference
View(data_neotoma_depenv)

data_ippd_depenv_harmonised <-
  ippd_data_public %>%
  dplyr::mutate(
    depenv_harmonised = dplyr::case_when(
      .default = depositionalenvironment,
      depositionalenvironment == "Valley Mire" ~ "Wetlands",
      depositionalenvironment == "Cirque Lake" ~ "Lacustrine",
      depositionalenvironment == "Explosion Crater Lake" ~ "Lacustrine",
      depositionalenvironment == "Glacial Origin Lake" ~ "Lacustrine",
      depositionalenvironment == "Swamp" ~ "Wetlands",
      depositionalenvironment == "Mire" ~ "Wetlands",
      depositionalenvironment == "Wind Origin Lake" ~ "Lacustrine",
      depositionalenvironment == "Glacial" ~ "Glacial",
      depositionalenvironment == "Interdunal Lake" ~ "Lacustrine",
      depositionalenvironment == "Natural Lake (Origin Unknown)" ~ "Lacustrine",
      depositionalenvironment == "Lava Flow Dammed Lake" ~ "Lacustrine",
      depositionalenvironment == "Archaeological" ~ "Terrestrial",
      depositionalenvironment == "Fen" ~ "Wetlands",
      depositionalenvironment == "Drained Lake" ~ "Drained Lake",
      depositionalenvironment == "Fluvial" ~ "Fluvial",
      depositionalenvironment == "Solution Origin Lake" ~ "Lacustrine",
      depositionalenvironment == "Moraine Dammed Lake" ~ "Fluvial",
      depositionalenvironment == "Lacustrine Beach" ~ "Coastal",
      depositionalenvironment == "Natural Lake" ~ "Lacustrine",
      depositionalenvironment == "lacustrine, volcanic lake" ~ "Lacustrine",
      depositionalenvironment == "lacustrine, natural open-water, tectonic lake" ~ "Lacustrine",
      depositionalenvironment == "terrestrial, mire, bog,\tblanket bog" ~ "Wetlands",
      depositionalenvironment == "terrestrial, mire, bog" ~ "Wetlands",
      depositionalenvironment == "terrestrial, mire (i.e. peatland, >30cm peat)" ~ "Wetlands",
      depositionalenvironment == "lacustrine, natural open-water" ~ "Lacustrine",
      depositionalenvironment == "coastal" ~ "Coastal",
      depositionalenvironment == "terrestrial, soil, buried soil" ~ "Terrestrial",
      depositionalenvironment == "terrestrial, mire,swamp (forested wetland or peatland)" ~ "Wetlands",
      depositionalenvironment == "lacustrine" ~ "Lacustrine",
      depositionalenvironment == "lacustrine, natural open-water, glacial origin" ~ "Lacustrine",
      depositionalenvironment == "terrestrial, mire, fen" ~ "Wetlands",
      depositionalenvironment == "coastal, estuarine" ~ "Coastal",
      depositionalenvironment == "terrestrial" ~ "Terrestrial",
      depositionalenvironment == "marine" ~ "Marine",
      depositionalenvironment == "terrestrial, cave sediments" ~ "Terrestrial",
      depositionalenvironment == "fluvial" ~ "Fluvial",
      depositionalenvironment == "lacustrine, playa" ~ "Coastal",
      depositionalenvironment == "lacustrine, drained lake" ~ "Drained Lake",
      is.na(depositionalenvironment) ~ "Unknown"
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

#----------------------------------------------------------#
# 4. Create figures -----
#----------------------------------------------------------#

p_dep_env_a <-
  plot_data_distribution_by_var(
    data = data_ippd_depenv_harmonised,
    var = "depenv_harmonised",
    coord_long = c(long_min, long_max), # [Config]
    coord_lat = c(lat_min, lat_max), # [Config]
    point_size = point_size, # [Config]
    text_size = text_size, # [Config]
    line_size = line_size, # [Config]
    map_color_fill = map_color_fill, # [Config]
    map_color_border = map_color_border, # [Config]"
    caption_label = TRUE
  )

p_dep_env_b <-
  plot_data_barplot(
    data = data_ippd_depenv_harmonised,
    var = "depenv_harmonised",
    text_size = text_size, # [Config]
    line_size = line_size, # [Config]
    bar_default_color = gray_dark, # [Config]
    legend_position = "none",
    caption_label = FALSE
  ) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank()
  )

p_dep_env <-
  cowplot::plot_grid(
    p_dep_env_a,
    p_dep_env_b,
    labels = "AUTO",
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
    plot = p_dep_env,
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
