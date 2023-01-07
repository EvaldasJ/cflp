################################################################
################################################################
##### 1.1_processing_data-cleaning-coordinate-conversion.R #####
################################################################
################################################################

# Code for converting population grid coordinates LKS94 to WGS84
# and store coordinates from WGS84 to LKS94

# Setup environment
source("./src/environment_setup.R")

# Offset is used to get the center of the grid
x_offset <- config$x_offset
y_offset <- config$y_offset

# Load population data
dt_pop <- readxl::read_excel(path = list.files(str_input_r, 
                                               pattern = paste0('population-by-grid-',config$grid_size), 
                                               full.names = T)) %>% 
  dplyr::select(c('grid_id','grid_size','x_coord_lks94','y_coord_lks94','population', 'mean_age')) %>%
  data.table()

# Adjust coordinates to match grid center
dt_pop[, `:=`(x_coord_lks94 = x_coord_lks94 + x_offset,
              y_coord_lks94 = y_coord_lks94 + y_offset)]

# Convert population coordinates from LKS94 to WGS84
dt_pop$latitude_y <- uFuncGridToGeo(x = dt_pop$x_coord_lks94, 
                                    y = dt_pop$y_coord_lks94)[1]
dt_pop$longitude_x <- uFuncGridToGeo(x = dt_pop$x_coord_lks94, 
                                     y = dt_pop$y_coord_lks94)[2]

# Export to .xlsx
openxlsx::write.xlsx(dt_pop, 
                     file = paste0(str_output_r, str_pop_filename))

# Load stores data
dt_stores <- readxl::read_excel(path = list.files("./data/raw", 
                                                  pattern = 'store-coordinates', 
                                                  full.names = T)) %>% 
  data.table()
names(dt_stores) %<>% tolower()

# Check if there are stores with no location information and remove such stores
dt_stores[is.na(latitude_y) | is.na(longitude_x)]
dt_stores <- dt_stores[!is.na(latitude_y) | !is.na(longitude_x)]

# Remove stores that has been closed before specified date (refer to configuration file config.yaml)
dt_stores[format(as.Date(closing_date), '%Y%m%d') <= format(as.Date(config$last_closing_date),'%Y%m%d')]
dt_stores <- dt_stores[!(format(as.Date(closing_date), '%Y%m%d') <= format(as.Date(config$last_closing_date),'%Y%m%d'))]

# Identify chain
dt_stores[, `:=`(chain = ifelse(tolower(chain_format) %like% 'aibe', 'AIBE',
                                ifelse(tolower(chain_format) %like% 'iki', 'IKI',
                                       ifelse(tolower(chain_format) %like% 'lidl','LIDL',
                                              ifelse(tolower(chain_format) %like% 'maxima','MAXIMA',
                                                     ifelse(tolower(chain_format) %like% 'norfa','NORFA',
                                                            ifelse(tolower(chain_format) %like% 'rimi','RIMI', 'OTHER')))))))]

# Number of stores by chain
table(dt_stores$chain)

# Convert store coordinates from WGS84 to LKS94
dt_stores$x_coord_lks94 <- uFuncGeoToGrid(lat = dt_stores$latitude_y , 
                                          lon = dt_stores$longitude_x)[1]
dt_stores$y_coord_lks94 <- uFuncGeoToGrid(lat = dt_stores$latitude_y , 
                                          lon = dt_stores$longitude_x)[2]
# Convert sales area to numeric
dt_stores$sales_area_m2 %<>% uFuncToNumeric()

# Export to .xlsx
openxlsx::write.xlsx(dt_stores, 
                     file = paste0(str_output_r, config$stores_filename))
