###########################################################
###########################################################
##### 2.1_modelling_customer-choice-rule-validation.R #####
###########################################################
###########################################################

# Code for validating customer choice rules
# - Binary choice model (deterministic)
# - Partially binary choice rule
# - Proportional (Huff gravity) choice rule (probabilistic)

# Code below is necessary in case this step is run separately
# Setup environment
source("./src/environment_setup.R")

# Population data
dt_pop <- readxl::read_excel(path = paste0(str_output_r, str_pop_filename)) %>%
  data.table()

# Stores data
dt_stores <- readxl::read_excel(path = paste0(str_output_r,
                                              config$stores_filename)) %>%
  data.table()

# Sales data
dt_sales <- readxl::read_excel(path = paste0(str_input_r,
                                             config$sales_filename)) %>%
  data.table()

# Alpha parameters
dt_alpha <- readxl::read_excel(path = paste0(str_output_r,
                                             config$alpha_filename)) %>%
  data.table()

# Calculate actual market share
dt_sales[, `:=`(ms_act = sales / sum(sales))]

# Run models with different beta values
# 1: no distance decay
# 2: default
# 3: based on Huff (1964)

vct_betas <- c(1,2,3)

lst_chain_ms_final <- list()
lst_chain_errors_final <- list()
lst_store_ms_final <- list()
lst_store_errors_final <- list()
lst_store_normality_final <- list()

for (b in 2:length(vct_betas)) {
  
  print(paste0('Beta value: ', vct_betas[[b]]))
  
  lst_chain_ms <- list()
  lst_chain_errors <- list()
  lst_store_ms <- list()
  lst_store_errors <- list()
  lst_store_normality <- list()
  
  # Run loop for different levels of catchment area
  vct_catchment <- c(seq(from = 100, to = 10, by = -5), 5, 3, 2)
  
  for (c in 1:length(vct_catchment)) {
    
    print(vct_catchment[[c]])
    
    # Distance data
    dt_dist <- fread(file = paste0(str_output_r, str_distance_filename))
    
    # Remove observations with distance greater than set catchment threshold
    dt_dist <- dt_dist[haversine_distance_m < vct_catchment[[c]]*1000]
    
    # Add sales area and chain name to distance data
    dt_dist$sales_area_m2 <- dt_stores$sales_area_m2[match(dt_dist$store_no, dt_stores$store_no)]
    dt_dist$chain <- dt_stores$chain[match(dt_dist$store_no, dt_stores$store_no)]
    
    ###################################
    #### !!! CONFIDENTIAL DATA !!! ####
    ###################################
    
    # Check if store level data should be modelled (refer to configuration file config.yaml)
    if (config$evaluate_ms_on_store_level == TRUE) {
      
      dt_store_ms <- readxl::read_excel(path = paste0(str_input_r,config$stores_ms_filename)) %>% 
        data.table()  
      
    }
    
    ###################################
    #### /\/\/\/\/\/\/\/\/\/\/\/\/ ####
    ###################################
    
    ####
    #### Evaluate customer choice rules
    ####
    
    # Evaluate customer choice rules by estimating market shares using each model.
    # First, calculate market shares with default attractiveness parameter alpha.
    # Then, use different alpha parameter values, reallocate demand points and recalculate market shares.
    
    # Remove other stores
    if (config$remove_other_stores == TRUE) {
      
      dt_dist <- dt_dist[!(store_no %in% dt_stores[chain=="OTHER"]$store_no)]
      
    }
    
    #### Run loop to evaluate each set of alpha parameters
    
    vct_runs <- c('Default','Average','Median')
    
    lst_ms <- list()
    lst_error <- list()
    
    lst_ms_store <- list()
    lst_error_store <- list()
    lst_normality <- list()
    
    for (k in 1:length(vct_runs)) {
      
      print(paste0('Run: ', vct_runs[[k]]))
      print(Sys.time())
      
      # Set beta parameter
      # Beta parameter (or distance decay component) indicates how willing customers 
      # are to travel longer distances to the stores.
      dt_dist$beta <- vct_betas[[b]]
      
      # Set alpha parameter
      # Alpha parameter (or attractiveness enhancement parameter)
      
      if (k == 1) {
        
        dt_dist$alpha <- 1
        
      } else if (k == 2) {
        
        dt_dist$alpha <- dt_alpha$alpha_est_avg[match(dt_dist$chain, dt_alpha$chain)]
        
      } else {
        
        dt_dist$alpha <- dt_alpha$alpha_est_med[match(dt_dist$chain, dt_alpha$chain)]
        
      }
      
      # Calculate Huff probabilities
      dt_huff <- uFuncHuff(facility = dt_dist$store_no,
                           facility_attractiveness = dt_dist$sales_area_m2,
                           demand_point = dt_dist$grid_id,
                           distance = dt_dist$haversine_distance_m,
                           alpha = dt_dist$alpha,
                           beta = dt_dist$beta)
      
      # Add chain information
      dt_huff$chain <- dt_stores$chain[match(dt_huff$facility, dt_stores$store_no)]
      
      # Add population
      dt_huff$population <- dt_pop$population[match(dt_huff$demand_point, dt_pop$grid_id)]
      
      ####
      #### 1. Binary choice model
      ####
      
      # Select facility with the highest probability for each demand point
      dt_bin <- setDT(dt_huff)[, .SD[which.max(huff_probability)], by = .(demand_point)]
      
      # Calculate market share
      dt_bin_ms <- dt_bin[, .(demand = sum(population)), 
                          by = .(chain)]
      dt_bin_ms[, `:=`(market_share = demand / sum(dt_bin_ms$demand))]
      
      ####
      #### 2. Partially binary choice model
      ####
      
      # Select facilities with the highest attractiveness for each demand point and for each chain
      dt_part_bin <- setDT(dt_huff)[, .SD[which.max(huff)], by = .(demand_point, chain)]
      
      # Recalculate huff probabilities
      setDT(dt_part_bin)[, huff_probability := prop.table(huff), .(demand_point)]
      
      # Allocate demand to facilities
      dt_part_bin[, `:=`(demand = huff_probability * population)]
      
      # Calculate market share
      dt_part_bin_ms <- dt_part_bin[, .(demand = sum(demand)), 
                                    by = .(chain)]
      dt_part_bin_ms[, `:=`(market_share = demand / sum(dt_part_bin_ms$demand))]
      
      ####
      #### 3. Proportional (Huff) choice rule
      ####
      
      # Allocate demand to facilities
      dt_huff[, `:=`(demand = huff_probability * population)]
      
      # Calculate market share
      dt_huff_ms <- dt_huff[, .(demand = sum(demand)), 
                            by = .(chain)]
      dt_huff_ms[, `:=`(market_share = demand / sum(dt_huff_ms$demand))]
      
      ####
      #### Market share on a chain level
      ####
      
      # Merge market share information
      lst_ms[[k]] <- merge(x = dt_bin_ms[, .(chain, binary_ms = market_share)],
                           y = dt_part_bin_ms[, .(chain, part_binary_ms = market_share)],
                           by = 'chain')
      
      lst_ms[[k]] <- merge(x = lst_ms[[k]],
                           y = dt_huff_ms[, .(chain, huff_ms = market_share)],
                           by = 'chain')
      
      # Add actual market share
      lst_ms[[k]]$actual_ms <- dt_sales$ms_act[match(lst_ms[[k]]$chain, dt_sales$chain)]
      lst_ms[[k]]$run <- vct_runs[[k]]
      
      # Calculate errors
      lst_error[[k]] <- data.table(mae_bin = sum(abs(lst_ms[[k]]$binary_ms - lst_ms[[k]]$actual_ms))/nrow(lst_ms[[k]]),
                                   mae_partbin = sum(abs(lst_ms[[k]]$part_binary_ms - lst_ms[[k]]$actual_ms))/nrow(lst_ms[[k]]),
                                   mae_huff = sum(abs(lst_ms[[k]]$huff_ms - lst_ms[[k]]$actual_ms))/nrow(lst_ms[[k]]),
                                   mpe_bin = sum((lst_ms[[k]]$binary_ms - lst_ms[[k]]$actual_ms)/lst_ms[[k]]$actual_ms)/nrow(lst_ms[[k]]),
                                   mpe_partbin = sum((lst_ms[[k]]$part_binary_ms - lst_ms[[k]]$actual_ms)/lst_ms[[k]]$actual_ms)/nrow(lst_ms[[k]]),
                                   mpe_huff = sum((lst_ms[[k]]$huff_ms - lst_ms[[k]]$actual_ms)/lst_ms[[k]]$actual_ms)/nrow(lst_ms[[k]]),
                                   mape_bin = sum(abs((lst_ms[[k]]$binary_ms - lst_ms[[k]]$actual_ms)/lst_ms[[k]]$actual_ms))/nrow(lst_ms[[k]]),
                                   mape_partbin = sum(abs((lst_ms[[k]]$part_binary_ms - lst_ms[[k]]$actual_ms)/lst_ms[[k]]$actual_ms))/nrow(lst_ms[[k]]),
                                   mape_huff = sum(abs((lst_ms[[k]]$huff_ms - lst_ms[[k]]$actual_ms)/lst_ms[[k]]$actual_ms))/nrow(lst_ms[[k]]),
                                   mse_bin = sum((lst_ms[[k]]$binary_ms - lst_ms[[k]]$actual_ms)^2)/nrow(lst_ms[[k]]),
                                   mse_partbin = sum((lst_ms[[k]]$part_binary_ms - lst_ms[[k]]$actual_ms)^2)/nrow(lst_ms[[k]]),
                                   mse_huff = sum((lst_ms[[k]]$huff_ms - lst_ms[[k]]$actual_ms)^2)/nrow(lst_ms[[k]]),
                                   rmse_bin = sqrt(sum((lst_ms[[k]]$binary_ms - lst_ms[[k]]$actual_ms)^2)/nrow(lst_ms[[k]])),
                                   rmse_partbin = sqrt(sum((lst_ms[[k]]$part_binary_ms - lst_ms[[k]]$actual_ms)^2)/nrow(lst_ms[[k]])),
                                   rmse_huff = sqrt(sum((lst_ms[[k]]$huff_ms - lst_ms[[k]]$actual_ms)^2)/nrow(lst_ms[[k]])))
      lst_error[[k]] <- melt.data.table(lst_error[[k]])
      lst_error[[k]]$variable %<>% as.character()
      lst_error[[k]] <- tidyr::separate(data = lst_error[[k]], col = 'variable', sep = '_', into = c('error','model'))
      lst_error[[k]]$run <- vct_runs[[k]]
      
      if (config$evaluate_ms_on_store_level == TRUE) {
        
        ####
        #### Market share on a store level
        ####
        
        # Binary choice rule
        dt_bin_store <- dt_bin[chain=="IKI", .(demand = sum(population)), by = .(facility)]
        dt_bin_store[, `:=`(bin_ms = demand / sum(demand))]
        
        # Partially binary choice rule
        dt_part_bin_store <- dt_part_bin[chain=="IKI", .(demand = sum(demand)), by = .(facility)]
        dt_part_bin_store[, `:=`(part_bin_ms = demand / sum(demand))]
        
        # Proportional (Huff) choice rul
        dt_huff_store <- dt_huff[chain=="IKI", .(demand = sum(demand)), by = .(facility)]
        dt_huff_store[, `:=`(huff_ms = demand / sum(demand))]
        
        lst_ms_store[[k]] <- merge(x = dt_store_ms,
                                   y = dt_bin_store[, .(store_no = facility, binary_ms = bin_ms)],
                                   by = 'store_no',
                                   all.x = T)
        lst_ms_store[[k]][is.na(binary_ms)]$binary_ms <- 0
        
        lst_ms_store[[k]] <- merge(x = lst_ms_store[[k]],
                                   y = dt_part_bin_store[, .(store_no = facility, part_binary_ms = part_bin_ms)],
                                   by = 'store_no',
                                   all.x = T)
        lst_ms_store[[k]][is.na(part_binary_ms)]$part_binary_ms <- 0
        
        lst_ms_store[[k]] <- merge(x = lst_ms_store[[k]],
                                   y = dt_huff_store[, .(store_no = facility, huff_ms)],
                                   by = 'store_no',
                                   all.x = T)  
        lst_ms_store[[k]]$run <- vct_runs[[k]]
        
        # Calculate errors
        lst_ms_store[[k]][, `:=`(abs_error_bin = binary_ms - actual_ms,
                                 abs_error_part_bin = part_binary_ms - actual_ms,
                                 abs_error_huff = huff_ms - actual_ms,
                                 pr_error_bin = binary_ms / actual_ms - 1,
                                 pr_error_part_bin = part_binary_ms / actual_ms - 1,
                                 pr_error_huff = huff_ms / actual_ms - 1)]
        
        # Order by market share
        lst_ms_store[[k]] <- lst_ms_store[[k]][order(-actual_ms)]
        
        # Normality test
        lst_normality[[k]] <- data.table(model = c('binary','partially_binary','huff'),
                                         shapiro_statistic = c(shapiro.test(lst_ms_store[[k]]$pr_error_bin)$statistic,
                                                               shapiro.test(lst_ms_store[[k]]$pr_error_part_bin)$statistic,
                                                               shapiro.test(lst_ms_store[[k]]$pr_error_huff)$statistic),
                                         p_value = c(shapiro.test(lst_ms_store[[k]]$pr_error_bin)$p.value,
                                                     shapiro.test(lst_ms_store[[k]]$pr_error_part_bin)$p.value,
                                                     shapiro.test(lst_ms_store[[k]]$pr_error_huff)$p.value))
        lst_normality[[k]]$run <- vct_runs[[k]]
        
        # Calculate errors
        lst_error_store[[k]] <- data.table(mae_bin = sum(abs(lst_ms_store[[k]]$binary_ms - lst_ms_store[[k]]$actual_ms))/nrow(lst_ms_store[[k]]),
                                           mae_partbin = sum(abs(lst_ms_store[[k]]$part_binary_ms - lst_ms_store[[k]]$actual_ms))/nrow(lst_ms_store[[k]]),
                                           mae_huff = sum(abs(lst_ms_store[[k]]$huff_ms - lst_ms_store[[k]]$actual_ms))/nrow(lst_ms_store[[k]]),
                                           mpe_bin = sum((lst_ms_store[[k]]$binary_ms - lst_ms_store[[k]]$actual_ms)/lst_ms_store[[k]]$actual_ms)/nrow(lst_ms_store[[k]]),
                                           mpe_partbin = sum((lst_ms_store[[k]]$part_binary_ms - lst_ms_store[[k]]$actual_ms)/lst_ms_store[[k]]$actual_ms)/nrow(lst_ms_store[[k]]),
                                           mpe_huff = sum((lst_ms_store[[k]]$huff_ms - lst_ms_store[[k]]$actual_ms)/lst_ms_store[[k]]$actual_ms)/nrow(lst_ms_store[[k]]),
                                           mape_bin = sum(abs((lst_ms_store[[k]]$binary_ms - lst_ms_store[[k]]$actual_ms)/lst_ms_store[[k]]$actual_ms))/nrow(lst_ms_store[[k]]),
                                           mape_partbin = sum(abs((lst_ms_store[[k]]$part_binary_ms - lst_ms_store[[k]]$actual_ms)/lst_ms_store[[k]]$actual_ms))/nrow(lst_ms_store[[k]]),
                                           mape_huff = sum(abs((lst_ms_store[[k]]$huff_ms - lst_ms_store[[k]]$actual_ms)/lst_ms_store[[k]]$actual_ms))/nrow(lst_ms_store[[k]]),
                                           mse_bin = sum((lst_ms_store[[k]]$binary_ms - lst_ms_store[[k]]$actual_ms)^2)/nrow(lst_ms_store[[k]]),
                                           mse_partbin = sum((lst_ms_store[[k]]$part_binary_ms - lst_ms_store[[k]]$actual_ms)^2)/nrow(lst_ms_store[[k]]),
                                           mse_huff = sum((lst_ms_store[[k]]$huff_ms - lst_ms_store[[k]]$actual_ms)^2)/nrow(lst_ms_store[[k]]),
                                           rmse_bin = sqrt(sum((lst_ms_store[[k]]$binary_ms - lst_ms_store[[k]]$actual_ms)^2)/nrow(lst_ms_store[[k]])),
                                           rmse_partbin = sqrt(sum((lst_ms_store[[k]]$part_binary_ms - lst_ms_store[[k]]$actual_ms)^2)/nrow(lst_ms_store[[k]])),
                                           rmse_huff = sqrt(sum((lst_ms_store[[k]]$huff_ms - lst_ms_store[[k]]$actual_ms)^2)/nrow(lst_ms_store[[k]])))
        lst_error_store[[k]] <- melt.data.table(lst_error_store[[k]])
        lst_error_store[[k]]$variable %<>% as.character()
        lst_error_store[[k]] <- tidyr::separate(data = lst_error_store[[k]], col = 'variable', sep = '_', into = c('error','model'))
        lst_error_store[[k]]$run <- vct_runs[[k]]
        
      }
      
    }
    
    # Create data tables
    lst_chain_ms[[c]] <- rbindlist(lst_ms)
    lst_chain_ms[[c]]$catchment <- vct_catchment[[c]]
    lst_chain_errors[[c]] <- rbindlist(lst_error)
    lst_chain_errors[[c]]$catchment <- vct_catchment[[c]]
    lst_store_ms[[c]] <- rbindlist(lst_ms_store)
    lst_store_ms[[c]]$catchment <- vct_catchment[[c]]
    lst_store_errors[[c]] <- rbindlist(lst_error_store)
    lst_store_errors[[c]]$catchment <- vct_catchment[[c]]
    lst_store_normality[[c]] <- rbindlist(lst_normality)
    lst_store_normality[[c]]$catchment <- vct_catchment[[c]]
    rm(dt_dist)
    gc(reset = T)
    
  }
  
  # Add to lists
  lst_chain_ms_final[[b]] <- rbindlist(lst_chain_ms)
  lst_chain_ms_final[[b]]$beta <- vct_betas[[b]]
  lst_chain_errors_final[[b]] <- rbindlist(lst_chain_errors)
  lst_chain_errors_final[[b]]$beta <- vct_betas[[b]]
  lst_store_ms_final[[b]] <- rbindlist(lst_store_ms)
  lst_store_ms_final[[b]]$beta <- vct_betas[[b]]
  lst_store_errors_final[[b]] <- rbindlist(lst_store_errors)
  lst_store_errors_final[[b]]$beta <- vct_betas[[b]]
  lst_store_normality_final[[b]] <- rbindlist(lst_store_normality)
  lst_store_normality_final[[b]]$beta <- vct_betas[[b]]
  
}

dt_chain_ms_final <- rbindlist(lst_chain_ms_final)
dt_chain_errors_final <- rbindlist(lst_chain_errors_final)
dt_store_ms_final <- rbindlist(lst_store_ms_final)
dt_store_errors_final <- rbindlist(lst_store_errors_final)
dt_store_normality_final <- rbindlist(lst_store_normality_final)

lst_export <- list(chain_ms = dt_chain_ms_final, 
                   chain_ms_errors = dt_chain_errors_final, 
                   store_ms = dt_store_ms_final,
                   store_ms_errors = dt_store_errors_final,
                   store_ms_errors_normality = dt_store_normality_final)

openxlsx::write.xlsx(lst_export, 
                     file = paste0(str_output_r, str_est_filename))
