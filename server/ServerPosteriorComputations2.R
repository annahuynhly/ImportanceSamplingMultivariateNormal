################################################################
# CODE FOR THE TABLE OF WEIGHTS, MUS, AND SIGMAS               #
################################################################

post_display_cols = reactiveValues()   
post_display_cols$showing <- 1:5    

observeEvent(input$submit_sample_post, {
  showCols(post_proxy, 1:5, reset = FALSE) #show the first five cols (because the colums are now all hidden)
})

#show the next five columns 
observeEvent(input$post_next_five, {
  #stop when the last column is displayed
  if(post_display_cols$showing[[length(post_display_cols$showing)]] < length(sample_post_values_reformatted_round())) {
    hideCols(post_proxy, post_display_cols$showing, reset = FALSE) #hide displayed cols
    post_display_cols$showing = post_display_cols$showing + 5
    showCols(post_proxy, post_display_cols$showing, reset = FALSE) #show the next five 
  } 
})

#similar mechanism but reversed to show the previous cols
observeEvent(input$post_prev_five, {
  #stop when the first column is displayed
  if(post_display_cols$showing[[1]] > 1) {
    hideCols(post_proxy, post_display_cols$showing, reset = FALSE) #hide displayed cols
    post_display_cols$showing = post_display_cols$showing - 5
    showCols(post_proxy, post_display_cols$showing, reset = FALSE) #show previous five
  } 
})

output$post_display_table = renderDT(
  sample_post_values_reformatted_round(),
  options = list(
    #hide all columns
    columnDefs = list(list(visible = FALSE, targets = 1:length(sample_post_values_reformatted_round()))), 
    scrollX = TRUE)  #for when many columns are visible
)

post_proxy = dataTableProxy('post_display_table')

################################################################
# CODE FOR COMPUTING THE EFFECTIVE RANGE                       #
################################################################

true_prior_effective_range = eventReactive(input$submit_prior_eff_range, {
  elicit_prior_effective_range(p = post_sample_p_val(), 
                               m = input$prior_eff_range_m, 
                               alpha01 = prior_elicitation_sigma_values()$alpha01,
                               alpha02 = prior_elicitation_sigma_values()$alpha02,
                               mu0 = prior_elicitation_mu_values()$mu0, 
                               lambda0 = prior_elicitation_mu_values()$lambda0, 
                               x_low = -10,
                               quantile_val = c(input$prior_eff_range_small_quantile, 
                                                input$prior_eff_range_large_quantile))
})

# mostly for display
prior_delta_values = reactive({
  
  x_effective_range_title = c()
  delta_title = c()
  x_effective_range_matrix = c()
  for(i in 1:post_sample_p_val()){
    newtitle1 = paste("Effective Range", i, sep = " ")
    newtitle2 = paste("Delta", i, sep = " ")
    x_effective_range_title = c(x_effective_range_title, newtitle1)
    delta_title = c(delta_title, newtitle2)
    grid = true_prior_effective_range()$grid[[i]]
    min_val = grid[1]
    max_val = grid[length(grid)]
    x_effective_range_matrix = cbind(x_effective_range_matrix, 
                                     c(min_val, max_val))
  }
  x_effective_range_matrix = as.data.frame(x_effective_range_matrix)
  names(x_effective_range_matrix) = x_effective_range_title
  rownames(x_effective_range_matrix) = c("Min", "Max")
  
  delta_data = as.data.frame(t(true_prior_effective_range()$delta))
  names(delta_data) = delta_title
  
  list("Delta" = delta_data,
       "Effective_Range" = x_effective_range_matrix)
})

xlim_comparison_vals = reactive({
  x_effective_range_list = list()
  for(i in 1:input$num_dimensions){
    grid = true_prior_effective_range()$grid[[i]]
    min_val = grid[1]
    max_val = grid[length(grid)]
    x_effective_range_list[[i]] = c(min_val, max_val)
  }
  x_effective_range_list
})

output$prior_eff_range_delta_table1 = renderTable({
  prior_delta_values()$Delta
})

output$prior_eff_range_delta_table2 = renderTable({
  prior_delta_values()$Effective_Range
})