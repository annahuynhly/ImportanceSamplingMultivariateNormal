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

sample_prior_values_cleaned_round = eventReactive(input$submit_prior_sampling, {
  round(sample_prior_values_cleaned(), 4)
})

prior_sample_display_head = eventReactive(input$submit_prior_sampling, {
  head(sample_prior_values_cleaned(), 10)
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
# CODE FOR TABLE!!!CCV                                         #
################################################################

# use df as: sample_prior_values_cleaned()

prior_display_cols <- reactiveValues()   
prior_display_cols$showing <- 1:5    

observeEvent(input$submit_prior_sampling, {
  showCols(prior_proxy, 1:5, reset = FALSE) #show the first five cols (because the colums are now all hidden)
})

#show the next five columns 
observeEvent(input$prior_next_five, {
  #stop when the last column is displayed
  if(prior_display_cols$showing[[length(prior_display_cols$showing)]] < length(sample_prior_values_cleaned_round())) {
    hideCols(prior_proxy, prior_display_cols$showing, reset = FALSE) #hide displayed cols
    prior_display_cols$showing = prior_display_cols$showing + 5
    showCols(prior_proxy, prior_display_cols$showing, reset = FALSE) #show the next five 
  } 
})

#similar mechanism but reversed to show the previous cols
observeEvent(input$prior_prev_five, {
  #stop when the first column is displayed
  if(prior_display_cols$showing[[1]] > 1) {
    hideCols(prior_proxy, prior_display_cols$showing, reset = FALSE) #hide displayed cols
    prior_display_cols$showing <- prior_display_cols$showing - 5
    showCols(prior_proxy, prior_display_cols$showing, reset = FALSE) #show previous five
  } 
})

output$prior_sample_table = renderDT(
  sample_prior_values_cleaned_round(),
  options = list(
    columnDefs = list(list(visible = FALSE, targets = 1:length(sample_prior_values_cleaned_round()))), #hide all columns
    scrollX = TRUE)  #for when many columns are visible
)

prior_proxy = dataTableProxy('prior_sample_table')

# OLD: this was when the head was provided.
#output$prior_sample_sample = renderPrint({
#  prior_sample_display_head()
#})

################################################################
# DOWNLOADING THE DATA                                         #
################################################################

output$download_prior_sample = downloadHandler(
  filename = "prior_sample.csv",
  content = function(file) {
    write.csv(sample_prior_values_cleaned(), file, row.names = FALSE)
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