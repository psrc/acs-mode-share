# Packages ----------------------------------------------------------------

# Packages for Interactive Web application
library(shiny)
library(shinydashboard)
library(bs4Dash)
library(shinycssloaders)

# Packages for Data Cleaning/Processing
library(tidyverse)

# Packages for Chart Creation
library(psrcplot)
library(echarts4r)

# Packages for Table Creation
library(DT)

# Package for Excel Data Creation
library(openxlsx)

# Run Modules Files ---------------------------------------------------------------------------
module_files <- list.files('modules', full.names = TRUE)
sapply(module_files, source)
source("functions.R")

# Page Information --------------------------------------------------------
left_panel_info <- read_csv("data/left_panel_information.csv", show_col_types = FALSE)
page_text <- read_csv("data/page_text.csv", show_col_types = FALSE)
source_info <- read_csv("data/source_information.csv", show_col_types = FALSE)

# Colors ---------------------------------------------------------------
load_clr <- "#91268F"

# Data via RDS files ------------------------------------------------------
commute_data <- readRDS("data/commute_data.rds") 

# Values for Drop-Downs ---------------------------------------------------
travel_modes_list <- as.character(unique(commute_data$variable))
race_list <- as.character(unique(commute_data |> select("geography") |> filter(!(geography %in% c("King County", "Kitsap County", "Pierce County", "Snohomish County", "Region", "White alone"))) |> pull()))

# Data Download Table List ------------------------------------------------------
download_table_list <- list("Sources" = source_info, "Mode to Work" = commute_data)
  
