################################################################
# BACKEND FOR PRIOR SAMPLING                                   #
################################################################

prior_sampling_seed = reactive(input$prior_seed)

sample_prior_values = eventReactive(input$submit_prior_sampling, {
  set.seed(prior_sampling_seed())
  sample_prior(N = input$prior_sample_bigN, 
               p = input$num_dimensions, 
               alpha01 = prior_elicitation_sigma_values()$alpha01,
               alpha02 = prior_elicitation_sigma_values()$alpha02,
               mu0 = prior_elicitation_mu_values()$mu0,
               lambda0 = prior_elicitation_mu_values()$lambda0)
})

sample_prior_values_cleaned = eventReactive(input$submit_prior_sampling, {
  sample_prior_data_cleaning(N = input$prior_sample_bigN, 
                             p = input$num_dimensions, 
                             mu_matrix = sample_prior_values()$mu_matrix, 
                             sigma_ii_matrix = sample_prior_values()$sigma_ii,
                             correlation_matrix = sample_prior_values()$correlation_matrix)
})

prior_sample_display_head = eventReactive(input$submit_prior_sampling, {
  head(sample_prior_values_cleaned(), 10)
})

# may not be displayed anymore?
output$prior_sample_sample = renderPrint({
  prior_sample_display_head()
  #head(sample_prior_values_cleaned(), 10)
})

# previously: sample_prior_content_values
sample_prior_effective_range = eventReactive(input$submit_sample_prior, {
  set.seed(prior_sampling_seed())
  # WARNING: the xi right now is not operational.
  prior_eff_range = true_prior_density(p = input$num_dimensions, 
                                       alpha01 = prior_elicitation_sigma_values()$alpha01,
                                       alpha02 = prior_elicitation_sigma_values()$alpha02,
                                       mu0 = prior_elicitation_mu_values()$mu0,
                                       lambda0 = prior_elicitation_mu_values()$lambda0)
  find_effective_range(p = input$num_dimensions, 
                       m = input$prior_sample_m, 
                       x_vector_matrix = prior_eff_range$x_vector, 
                       y_vector_matrix = prior_eff_range$y_vector,
                       quantile_val = c(input$prior_sample_small_quantile,
                                        input$prior_sample_large_quantile))
})

# mostly for display
sample_prior_delta_values = reactive({
  
  x_effective_range = sample_prior_effective_range()$x_range
  x_effective_range_title = c()
  x_grid_title = c()
  for(i in 1:ncol(x_effective_range)){
    newtitle1 = paste("Effective Range", i, sep = " ")
    newtitle2 = paste("Grid", i, sep = " ")
    x_effective_range_title = c(x_effective_range_title, newtitle1)
    x_grid_title = c(x_grid_title, newtitle2)
  }
  #y_effective_range = sample_prior_effective_range()$y_range
  effective_range_grid = sample_prior_effective_range()$grid
  
  names(x_effective_range) = x_effective_range_title
  names(effective_range_grid) = effective_range_grid
  
  data.frame("delta" =  sample_prior_effective_range()$delta,
             "effective_range_bounds" = x_effective_range,
             "effective_range_grid" = effective_range_grid)
})

output$prior_sample_delta = renderPrint({
  
  x_effective_range_title = c()
  x_grid_title = c()
  
  x_effective_range = as.data.frame(sample_prior_effective_range()$x_range)
  #y_effective_range = sample_prior_effective_range()$y_range
  effective_range_grid = as.data.frame(sample_prior_effective_range()$grid)
  
  for(i in 1:ncol(x_effective_range)){
    newtitle1 = paste("Effective Range", i, sep = " ")
    newtitle2 = paste("Grid", i, sep = " ")
    x_effective_range_title = c(x_effective_range_title, newtitle1)
    x_grid_title = c(x_grid_title, newtitle2)
  }
  names(x_effective_range) = x_effective_range_title
  names(effective_range_grid) = x_grid_title
  
  list("delta" =  sample_prior_effective_range()$delta,
       "effective_range_bounds" = x_effective_range)#,
       #"effective_range_grid" = effective_range_grid)
})

################################################################
# DOWNLOADING THE DATA                                         #
################################################################

output$download_prior_sample = downloadHandler(
  filename = "prior_sample.csv",
  content = function(file) {
    write.csv(sample_prior_values_cleaned(), file, row.names = FALSE)
  }
)

# BELOW HAS CHANGED/BEEN REMOVED -> NEEDED TO REFORMAT ACCORDING TO
# MIKE'S DESIRES.

# Mu ###########################################################

download_sample_prior_mu = reactive({
  # cleaning the data by changing the column names.
  data = as.data.frame(sample_prior_values()$mu_matrix)
  for(i in 1:ncol(data)){
    colnames(data)[i] = paste("mu", i, sep = " ")
  }
  data
})

output$download_prior_sample_mu = downloadHandler(
  filename = "prior_sample_mu.csv",
  content = function(file) {
    write.csv(download_sample_prior_mu(), file, row.names = FALSE)
  }
)

# Sigma ########################################################

output$download_prior_sample_sigma = downloadHandler(
  filename = "prior_sample_sigma.csv",
  content = function(file) {
    # Note: difficult to change the column names for this particular case.
    # todo: try to edit it later...
    write.csv(sample_prior_values()$sigma_matrix, file, row.names = FALSE)
  }
)

# Correlation Matrix ############################################

output$download_prior_sample_correlation = downloadHandler(
  filename = "prior_sample_correlation.csv",
  content = function(file) {
    # TODO: change the names 
    write.csv(sample_prior_values()$correlation_matrix, file, row.names = FALSE)
  }
)


################################################################
# OLD!!                                                        #
################################################################

# old histogram - kept here in case it is needed later.
# below is the histogram - newest version is a line plot.
#sample_prior_hist(mu_prior = sample_prior_values()$mu_matrix, 
#                  col_num = input$prior_sample_col, 
#                  delta = input$prior_sample_delta, 
#                  min_xlim = input$prior_sample_xlim_min, 
#                  max_xlim = input$prior_sample_xlim_max,
#                  smooth_num = input$prior_sample_smoother, 
#                  colour_choice = input$prior_sample_colour,
#                  lty_type = as.numeric(input$prior_sample_lty), 
#                  transparency = input$prior_sample_transparency)

#prior_sample_histogram_DOWNLOAD = function(){
#  content_density_plot(density = sample_prior_content_values()$prior_density, 
#                       col_num = input$prior_sample_col, 
#                       grid = sample_prior_content_values()$plotting_grid, 
#                       min_xlim = input$prior_sample_xlim_min, 
#                       max_xlim = input$prior_sample_xlim_max,
#                       smooth_num = input$prior_sample_smoother,
#                       colour_choice = input$prior_sample_colour,
#                       lty_type = as.numeric(input$prior_sample_lty), 
#                       transparency = input$prior_sample_transparency)
#}

#output$prior_sample_histogram = renderPlot({
#  prior_sample_histogram_DOWNLOAD()
#})