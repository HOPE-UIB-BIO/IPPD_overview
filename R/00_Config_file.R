#----------------------------------------------------------#
#
#
#                       Project name
#
#                       Config file
#                 
#
#                         Names 
#                         Year
#
#----------------------------------------------------------#
# Configuration script with the variables that should be consistent throughout 
#   the whole repo. It loads packages, defines important variables, 
#   authorises the user, and saves config file.


#----------------------------------------------------------#
# 1. Load packages -----
#----------------------------------------------------------#

if(!exists("update_repo_packages")){
  update_repo_packages <- TRUE
}

if(update_repo_packages == TRUE){
  
  # # install fossilpol from github
  # if (!exists("already_installed_fossilpol")){
  #   already_installed_fossilpol <- FALSE
  # }
  # 
  # if(already_installed_fossilpol == FALSE){
  #   devtools::install_github("HOPE-UIB-BIO/fossilpol",
  #                            quiet = FALSE,
  #                            upgrade = FALSE)
  #   already_installed_fossilpol <- TRUE
  # }
  
  if (!exists("already_synch")){
    already_synch <- FALSE
  }
  
  if(already_synch == FALSE){
    library(here)
    # synchronise the package versions
    renv::restore(lockfile = here::here( "renv/library_list.lock"))
    already_synch <- TRUE
    
    # save snapshot of package versions
    # renv::snapshot(lockfile =  "renv/library_list.lock")  # do only for update
  }
}

# define packages
package_list <- 
  c(
    "assertthat",
    "devtools",
    "ggpubr",
    "here",
    "maps",
    "RColorBrewer",
    "renv",       
    "roxygen2",   
    "tidyverse",  
    "usethis"    
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
    recursive = TRUE) 

# source them
sapply(
  paste0("R/functions/", fun_list, sep = ""),
  source)


#----------------------------------------------------------#
# 4. Authorise the user -----
#----------------------------------------------------------#

# if applicable

#----------------------------------------------------------#
# 5. Define variables -----
#----------------------------------------------------------#

# geographical boudaries
long_min <- 90
long_max <- 180
lat_min <- -50
lat_max <- 25

#----------------------------------------------------------#
# 6. Graphical options -----
#----------------------------------------------------------#

# define general
text_size = 10
line_size = 0.1
point_size = 3

# define output sizes
image_width <- 45
image_height <- 35
image_units <- "cm"
image_dpi <- 600


# define pallets

# define common color
gray_light <- "gray80"
gray_dark <- "gray30"
accend_blue <-  "#00AFBB"
accend_red <- "#FF0033"

map_color_fill <- "gray90"
map_color_border <- NA


#set ggplot output
ggplot2::theme_set(
  ggplot2::theme_classic()+
    ggplot2::theme(
      text = ggplot2::element_text(
        size = text_size,
        colour = gray_dark),
      line = ggplot2::element_line(
        size = line_size,
        colour = gray_dark)))


#----------------------------------------------------------#
# 7. Save current config setting -----
#----------------------------------------------------------#

current_setting <- .extract.config.data()
