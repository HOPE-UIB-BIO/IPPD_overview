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

# Script to prepare all necessary components of environment to run the Project.
#   Needs to be run only once


#----------------------------------------------------------#
# Step 0: Install {renv} for package management -----
#----------------------------------------------------------#

if (
  "renv" %in% utils::installed.packages()
) {
  library(renv)
} else {
  # install package
  utils::install.packages("renv")

  # load the package
  library(renv)
}


#----------------------------------------------------------#
# Step 1: Deactivate 'renv' project -----
#----------------------------------------------------------#

# NOTE: The R may ask the User to restart the session (R).
#   After that, continue with the next step

renv::activate()


#----------------------------------------------------------#
# Step 2: Install {here} for file navigation -----
#----------------------------------------------------------#

if (
  "here" %in% utils::installed.packages()
) {
  library(here)
} else {
  # install package
  utils::install.packages("here")

  # load the package
  library(here)
}


#----------------------------------------------------------#
# Step 3: Synchronize package versions with the project -----
#----------------------------------------------------------#

# If there is no lock file present make a new snapshot
if (
  isTRUE("library_list.lock" %in% list.files(here::here("renv")))
) {
  cat("The project already has a lockfile. Restoring packages", "\n")

  renv::restore(
    lockfile = here::here("renv/library_list.lock")
  )

  cat("Set up completed. You can continute to run the project", "\n")

  cat("Do NOT run the rest of this script", "\n")
} else {
  cat("The project seems to be new (no lockfile)", "\n")

  cat("Continue with this script", "\n")
}


#----------------------------------------------------------#
# Step 4: Install packages to the project -----
#----------------------------------------------------------#

# install all packages in the lst from CRAN
sapply(
  c(
    "assertthat",
    "cowplot",
    "ggpubr",
    "here",
    "httpgd",
    "httr2",
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
    "usethis"
  ),
  utils::install.packages,
  character.only = TRUE
)


#----------------------------------------------------------#
# Step 5: Save versions of packages -----
#----------------------------------------------------------#

renv::snapshot(
  lockfile = here::here("renv/library_list.lock")
)

cat("Set up completed. You can continute to run the project", "\n")
