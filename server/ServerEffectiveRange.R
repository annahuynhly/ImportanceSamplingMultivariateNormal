
true_prior_effective_range = eventReactive(input$submit_prior_eff_range, {
  elicit_prior_effective_range(p = input$num_dimensions, 
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
  x_effective_range_matrix = c()
  for(i in 1:input$num_dimensions){
    newtitle1 = paste("Effective Range", i, sep = " ")
    x_effective_range_title = c(x_effective_range_title, newtitle1)
    grid = true_prior_effective_range()$grid[[i]]
    min_val = grid[1]
    max_val = grid[length(grid)]
    x_effective_range_matrix = cbind(x_effective_range_matrix, 
                                     c(min_val, max_val))
  }
  x_effective_range_matrix = as.data.frame(x_effective_range_matrix)
  names(x_effective_range_matrix) = x_effective_range_title
  x_effective_range_matrix
  
  list("Delta" = true_prior_effective_range()$delta,
       "Effective Range" = x_effective_range_matrix)
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

output$prior_eff_range_delta = renderPrint({
  #xlim_comparison_vals()
  prior_delta_values()
})
