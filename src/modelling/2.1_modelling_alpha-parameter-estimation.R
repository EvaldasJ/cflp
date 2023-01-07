######################################################
######################################################
##### 2.1_modelling_alpha-parameter-estimation.R #####
######################################################
######################################################

# Code for estimating alpha parameters

# # Code below is necessary in case this step is run separately
# # Setup environment
# source("./src/environment_setup.R")
# 
# # Stores data
# dt_stores <- readxl::read_excel(path = paste0(str_output_r,
#                                               config$stores_filename)) %>%
#   data.table()

# Sales data
dt_sales <- readxl::read_excel(path = paste0(str_input_r,
                                             config$sales_filename)) %>%
  data.table()
dt_sales[, `:=`(ms_act = sales / sum(sales))]

####
#### Estimate alpha parameter
####

# Calculate total sales area (total attractiveness)
if (config$remove_other_stores == TRUE) {
  
  dt_alpha <- dt_stores[chain != 'OTHER', .(total_sales_area_m2 = sum(sales_area_m2)), 
                        by = .(chain)]
  
} else {
  
  dt_alpha <- dt_stores[, .(total_sales_area_m2 = sum(sales_area_m2)),
                        by = .(chain)]
  
}
  
# Add sales data
dt_alpha$sales <- dt_sales$sales[match(dt_alpha$chain, dt_sales$chain)]

# Calculate average chain and total efficiency using average and median
dt_alpha[, `:=`(sales_per_m2 = sales / total_sales_area_m2)][, `:=`(sales_per_m2_avg = mean(sales_per_m2),
                                                                    sales_per_m2_med = median(sales_per_m2))]
# Calculate chain attractiveness parameter alpha based on efficiency
dt_alpha[, `:=`(alpha_est_avg = log(sales_per_m2, sales_per_m2_avg),
                alpha_est_med = log(sales_per_m2, sales_per_m2_med))]

# Export data
openxlsx::write.xlsx(dt_alpha, 
                     file = paste0(str_output_r, config$alpha_filename))
