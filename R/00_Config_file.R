#----------------------------------------------------------#
#
#
#                     IPPD overview
#
#                       Config file
#
#
#                      Ondrej Mottl
#                         2024
#
#----------------------------------------------------------#
# Configuration script with the variables that should be consistent throughout
#   the whole repo. It loads packages, defines important variables,
#   authorises the user, and saves config file.

# Set the current environment
current_env <- environment()

# set seed
set.seed(1234)


#----------------------------------------------------------#
# 1. Load packages -----
#----------------------------------------------------------#

if (
  isFALSE(
    exists("already_synch", envir = current_env)
  )
) {
  already_synch <- FALSE
}

if (
  isFALSE(already_synch)
) {
  library(here)
  # Synchronise the package versions
  renv::restore(
    lockfile = here::here("renv/library_list.lock")
  )
  already_synch <- TRUE

  # Save snapshot of package versions
  # renv::snapshot(lockfile =  "renv/library_list.lock")  # do only for update
}

# define packages
package_list <-
  c(
    "assertthat",
    "cowplot",
    "ggpubr",
    "here",
    "httpgd",
    "janitor",
    "jsonlite",
    "knitr",
    "languageserver",
    "maps",
    "RColorBrewer",
    "renv",
    "remotes",
    "roxygen2",
    "tidyverse",
    "usethis",
    "utils"
  )

# load all packages
sapply(package_list, library, character.only = TRUE)


#----------------------------------------------------------#
# 2. Define space -----
#----------------------------------------------------------#

current_date <- Sys.Date()

# project directory is set up by 'here' package, Adjust if needed
current_dir <- here::here()


#----------------------------------------------------------#
# 3. Load functions -----
#----------------------------------------------------------#

# get vector of general functions
fun_list <-
  list.files(
    path = "R/Functions/",
    pattern = "*.R",
    recursive = TRUE
  )

# source them
sapply(
  paste0("R/Functions/", fun_list, sep = ""),
  source
)


#----------------------------------------------------------#
# 4. Define variables -----
#----------------------------------------------------------#

# geographical boudaries
long_min <- 90
long_max <- 180
lat_min <- -50
lat_max <- 10

#----------------------------------------------------------#
# 5. Graphical options -----
#----------------------------------------------------------#

# define general
text_size <- 12
line_size <- 0.1
point_size <- 4

# define output sizes
image_width <- 18
image_height <- 18
image_units <- "cm"
image_dpi <- 300


# define pallets

# define common color
gray_light <- "gray80"
gray_dark <- "gray30"
accend_blue <- "#00AFBB"
accend_red <- "#FF0033"

map_color_fill <- "gray90"
map_color_border <- NA


# set ggplot output
ggplot2::theme_set(
  ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(
        size = text_size,
        colour = gray_dark
      ),
      line = ggplot2::element_line(
        size = line_size,
        colour = gray_dark
      ),
      axis.title = ggplot2::element_text(
        size = text_size
      ),
      axis.text = ggplot2::element_text(
        size = text_size
      ),
      legend.text = ggplot2::element_text(
        size = text_size
      ),
      legend.title = ggplot2::element_text(
        size = text_size
      ),
      plot.title = ggplot2::element_text(
        size = text_size
      ),
      plot.subtitle = ggplot2::element_text(
        size = text_size
      ),
      plot.caption = ggplot2::element_text(
        size = text_size
      )
    )
)
