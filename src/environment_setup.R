# Description

# Setup environment
# Developed by Evaldas Jankauskas, jankauskas.ev@gmail.com

# Load Packages
libs <- c('data.table', 'RODBC', 'Hmisc', 'here','magrittr', 'config',
          'readxl', 'openxlsx', 'sf', 'dplyr', 'rgdal', 'rgeos', 'igraph', 
          'FNN', 'fastshap', 'hereR', 'geosphere', 'ggplot2')
invisible(lapply(libs, library, character.only = TRUE))

# Load user functions
source('./src/custom-functions.R')

# Load configuration file
str_config <- 'config_var' # <<< #### Set configuration here ####
config <- config::get(file = './src/config.yml')
config_var <- config::get(file = './src/config.yml',
                          config = str_config)

# Set directories
str_output_r <- './data/processed/'
str_output_plots <- './output/plots/'
str_input_r <- './data/input/'

# Population filename
str_pop_filename <- paste0('processing_population-by-',config$grid_size,'-grid.xlsx')

str_est_filename <-  paste0('modelling_total-and-store-level-market-share-estimation-with-different-alpha-and-beta-values-',config$grid_size,'-grid-size.xlsx')
str_distance_filename <- paste0('processing_distances-between-stores-and-grids-',config$grid_size,'-grid-size-',config$distance_threshold_km,'km-catchment-area.csv')
