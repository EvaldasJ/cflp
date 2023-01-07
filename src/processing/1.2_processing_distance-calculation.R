#################################################
#################################################
##### 1.2_processing_distance-calculation.R #####
#################################################
#################################################

# Code for calculating distance between stores and population grids

# # Code below is necessary in case this step is run separately
# # Setup environment
# source("./src/environment_setup.R")
# 
# # Load population data
# dt_pop <- readxl::read_excel(path = paste0(str_output_r, 
#                                            str_pop_filename)) %>%
#   data.table()
# 
# # Load stores data
# dt_stores <- readxl::read_excel(path = paste0(str_output_r, 
#                                               config$stores_filename)) %>%
#   data.table()

# Calculate haversine distance between stores and population grids
dt_dist <- geosphere::distm(x = as.matrix(dt_pop[, .(longitude_x, latitude_y)]), 
                            y = as.matrix(dt_stores[, .(longitude_x,latitude_y)]),
                            fun = distHaversine) %>%
  data.table() %>%
  setnames(., old = colnames(.), new = dt_stores[, ]$store_no)
dt_dist$grid_id <- dt_pop$grid_id
dt_dist <- melt.data.table(data = dt_dist, 
                           id.vars = 'grid_id', 
                           variable.name = 'store_no', 
                           value.name = 'haversine_distance_m')

# Remove observations with distance greater threshold value (refer to configuration file config.yaml)
dt_dist <- dt_dist[haversine_distance_m < (config$distance_threshold_km*1000)]
dt_dist$store_no %<>% as.character()

# Save data to .csv
fwrite(dt_dist, 
       file = paste0(str_output_r, str_distance_filename))
