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
                               x_low = -10)#,
                               #quantile_val = c(input$prior_eff_range_small_quantile, 
                               #                  input$prior_eff_range_large_quantile))
})

prior_delta_values = reactive({
  index = input$comparison_mu_col
  grid = true_prior_effective_range()$grid[[index]]
  
  min_val = grid[1]
  max_val = grid[length(grid)]
  
  delta = true_prior_effective_range()$delta[index]
  datatable = as.data.frame(t(c(delta, min_val, max_val)))
  names(datatable) = c("Delta", "Min", "Max")
  datatable
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
  prior_delta_values()
})
