#----------------------------------------------------------#
#
#
#                     IPPD overview
#
#             Record position in climate space
#
#
#                       O. Mottl
#                         2024
#
#----------------------------------------------------------#

# Plot records in a climate space

#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

vec_var <-
  c("MAP", "MAT", "MTWA", "MTCO") %>%
  rlang::set_names()

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

data_clim_records <-
  readr::read_csv(
    paste0(
      current_dir,
      "/Data/Input/IPPD_climate_records.csv"
    ),
    show_col_types = FALSE
  )

if (
  isTRUE(verbose)
) {
  dplyr::glimpse(data_clim_records)
}

data_clim_total <-
  readr::read_csv(
    paste0(
      current_dir,
      "/Data/Input/IPPD_climate_total.csv"
    ),
    show_col_types = FALSE
  )


#----------------------------------------------------------#
# 2. Data wrangling -----
#----------------------------------------------------------#

data_unit_table <-
  tibble::tibble(
    var_name = vec_var,
    var_units = c("m yr-1", "deg C", "deg C", "deg C")
  )

data_clim_total_long <-
  data_clim_total %>%
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "var_name",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    value = dplyr::case_when(
      .default = value,
      var_name == "MAP" ~ value / 1e3
    )
  )

data_clim_records_work <-
  data_clim_records %>%
  dplyr::mutate(
    value = dplyr::case_when(
      .default = value,
      var_name == "MAP" ~ value / 1e3
    )
  )


pal_region <-
  make_custom_palette(
    data = data_clim_records_work,
    var = "region",
    palette = sort(
      PrettyCols::PrettyColsPalettes[["Rainbow"]][[1]]
    )
  )

data_dummy_table <-
  tidyr::expand_grid(
    var_a = vec_var,
    var_b = vec_var
  ) %>%
  dplyr::left_join(
    data_unit_table,
    by = c("var_a" = "var_name"),
  ) %>%
  dplyr::left_join(
    data_unit_table,
    by = c("var_b" = "var_name"),
    suffix = c("_a", "_b")
  ) %>%
  dplyr::mutate(
    name = stringr::str_c(var_a, "_", var_b)
  )

#----------------------------------------------------------#
# 3. Make figures -----
#----------------------------------------------------------#

p_list_clim <-
  purrr::pmap(
    .progress = TRUE,
    .l = list(
      data_dummy_table$var_a,
      data_dummy_table$var_b,
      data_dummy_table$var_units_a,
      data_dummy_table$var_units_b
    ),
    .f = ~ plot_climate_space(
      data_source_records = data_clim_records_work,
      data_source_total = data_clim_total_long,
      var_x = ..1,
      var_y = ..2,
      var_x_name = paste0(..1, " (", ..3, ")"),
      var_y_name = paste0(..2, " (", ..4, ")"),
      custom_palette = pal_region,
      default_point_color = gray_light, # [Config]
      point_size = 1, # [Config]
      text_size = text_size, # [Config]
      line_size = line_size, # [Config]
    )
  ) %>%
  rlang::set_names(nm = data_dummy_table$name)

p_density <-
  purrr::map2(
    .x = data_unit_table$var_name,
    .y = data_unit_table$var_units,
    .f = ~ plot_distribution(
      data_source = data_clim_total_long,
      var = .x,
      var_x_name = paste0(.x, " (", .y, ")"),
      default_color = gray_light, # [Config]
      text_size = text_size, # [Config]
      line_size = line_size # [Config]
    )
  )

plot_empty <-
  ggplot2::ggplot() +
  ggplot2::theme_void()

p_list_clim[c(1, 6, 11, 16)] <- p_density

p_list_clim[c(5, 9:10, 13:15)] <- list(plot_empty)

p_climate_space <-
  cowplot::plot_grid(
    p_density[["MAP"]],
    p_list_clim[["MAP_MAT"]],
    p_list_clim[["MAP_MTWA"]],
    p_list_clim[["MAP_MTCO"]],
    plot_empty,
    p_density[["MAT"]],
    p_list_clim[["MAT_MTWA"]],
    p_list_clim[["MAT_MTCO"]],
    plot_empty,
    plot_empty,
    p_density[["MTWA"]],
    p_list_clim[["MTWA_MTCO"]],
    plot_empty,
    plot_empty,
    plot_empty,
    p_density[["MTCO"]],
    ncol = 4,
    nrow = 4
  )

#----------------------------------------------------------#
# 4. Save -----
#----------------------------------------------------------#

purrr::walk(
  .x = c("pdf", "png"),
  .f = ~ ggplot2::ggsave(
    filename = paste0(
      current_dir, "/Outputs/Figures/Figure_03.", .x
    ),
    plot = p_climate_space,
    width = image_width, # [Config]
    height = image_height, # [Config]
    units = image_units, # [Config]
    dpi = image_dpi # [Config]
  )
)
