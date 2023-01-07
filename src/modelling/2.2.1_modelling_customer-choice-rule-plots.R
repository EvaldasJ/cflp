########################################################
########################################################
##### 2.2.1_modelling_customer-choice-rule-plots.R #####
########################################################
########################################################

# Code for generating market share error plots for customer choice rules

# Setup environment
source("./src/environment_setup.R")

####
#### User input
####

# Set error to plot
str_error <- 'RMSE'
vct_catchment <- 3

# Upload chain level data
dt_chain_err <- readxl::read_excel(path = paste0(str_output_r, str_est_filename), sheet = 'chain_ms_errors') %>% 
    data.table()
names(dt_chain_err) <- Cs(error, model, value, alpha, catchment, beta)
dt_chain_err$error %<>% toupper()
dt_chain_err[, `:=`(model = ifelse(model == 'bin', 'Binary', 
                                   ifelse(model == 'partbin', 'Partially binary','Proportional (Huff)')))]
dt_chain_err[, `:=`(group = ifelse(alpha == 'Default', paste0('Alpha 1, Beta ',beta), paste0('Alpha EST., Beta ',beta)))]
dt_chain_err$model <- factor(dt_chain_err$model, levels = c('Binary','Partially binary', 'Proportional (Huff)'))

# Upload store level data
dt_stores_err <- readxl::read_excel(path = paste0(str_output_r, str_est_filename), sheet = 'store_ms_errors') %>% 
    data.table()
names(dt_stores_err) <- Cs(error, model, value, alpha, catchment, beta)
dt_stores_err$error %<>% toupper()
dt_stores_err[, `:=`(model = ifelse(model == 'bin', 'Binary', 
                                    ifelse(model == 'partbin', 'Partially binary','Proportional (Huff)')))]
dt_stores_err[, `:=`(group = ifelse(alpha == 'Default', paste0('Alpha 1, Beta ',beta), paste0('Alpha EST., Beta ',beta)))]
dt_stores_err$model <- factor(dt_chain_err$model, levels = c('Binary','Partially binary', 'Proportional (Huff)'))

if (config$use_average_alpha == TRUE) {
    
    dt_chain_err <- dt_chain_err[alpha %in% c('Default','Average')]
    dt_stores_err <- dt_stores_err[alpha %in% c('Default','Average')]
}

# Upload predicted and observed market shares
dt_stores_detailed <- readxl::read_excel(path = paste0(str_output_r, str_est_filename), sheet = 'store_ms') %>% 
    data.table()

# Chain-level errors
ggplot(data = dt_chain_err[error==str_error], aes(x = catchment, 
                                               y = value, 
                                               group = group,
                                               shape = group)) + 
    xlab('Catchment distance, km.') +
    ylab(paste0(str_error)) +
    geom_line() + 
    geom_point(size = 1) +
    facet_wrap(facets = vars(model)) +
    scale_x_continuous(breaks = seq(0, 100, by = 10)) +
    scale_y_continuous(breaks = round(seq(min(dt_chain_err[error==str_error]$value), 
                                          max(dt_chain_err[error==str_error]$value), 
                                          by = 0.01), 3)) +
    ggthemes::theme_clean(base_size = 10) +
    theme(legend.title = element_blank(), 
          legend.position = 'bottom', 
          legend.background = element_rect(color = NA),
          legend.text = element_text(size=8))
    # ggthemes::theme_calc()
    # theme_bw()
    # theme_minimal()
    # theme_publish()

# Store-level errors
ggplot(data = dt_stores_err[error==str_error], aes(x = catchment, 
                                                   y = value, 
                                                   group = group,
                                                   shape = group)) + 
    xlab('Catchment distance, km.') +
    ylab(paste0(str_error)) +
    geom_line() + 
    geom_point(size = 2) +
    facet_wrap(facets = vars(model)) +
    scale_y_continuous(breaks = round(seq(min(dt_stores_err[error==str_error]$value), 
                                          max(dt_stores_err[error==str_error]$value), 
                                          by = 0.001), 5)) +
    scale_x_continuous(breaks = seq(0, 100, by = 10)) +
    ggthemes::theme_clean(base_size = 10) +
    theme(legend.title = element_blank(), 
          legend.position = 'bottom', 
          legend.background = element_rect(color = NA),
          legend.text = element_text(size=8))

####
#### Error distribution
####

dt_stores_detailed <- dt_stores_detailed[catchment == vct_catchment & run == "Average" & beta == 1]

# # Generate q-plots to check for normality of error distribution
# ggpubr::ggqqplot(dt_stores_detailed$abs_error_bin, 
#                  title = paste0('Absolute error Q-plot: binary with'))

# Predicted versus observed market share
ggplot(data = dt_stores_detailed, aes(x = dt_stores_detailed$actual_ms, 
                                      y = dt_stores_detailed$huff_ms)) + 
    xlab('Observed market share') +
    ylab('Predicted market share') +
    geom_point(shape = 1, size = 2) +
    ggthemes::theme_clean() +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    geom_smooth(method = 'lm', se = FALSE, color = 'black', size = 0.5)
    #ggtitle('Observed versus predicted market share')

# Percentage error
ggplot(data = dt_stores_detailed, aes(x = seq(1, nrow(dt_stores_detailed), by = 1), y = dt_stores_detailed$pr_error_huff)) + 
    xlab('Store index') +
    ylab('Percentage error') +
    geom_point(shape = 1, size = 2) +
    ggthemes::theme_clean() +
    geom_hline(yintercept = 0)
    # ggtitle('Market share percentage error')

