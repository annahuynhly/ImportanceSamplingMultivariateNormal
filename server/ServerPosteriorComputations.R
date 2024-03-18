################################################################
# BACKEND FOR POSTERIOR COMPUTATIONS                           #
################################################################

post_integ_seed = reactive(input$post_seed)

choose_file_Y_type = reactive({
  if(input$post_input_type == "csv"){
    read.csv(input$sample_post_Y$datapath, header = TRUE)
  } else if (input$post_input_type == "txt"){
    read.csv(input$sample_post_Y_txt$datapath, sep = "\t")
  } else if (input$post_input_type == "default"){
    test_sample_data()
  }
})

input_Y_values = reactive({
  #as.matrix(test_sample_data())
  tryCatch(
    {
      df = choose_file_Y_type()
      #df = read.csv(input$sample_post_Y$datapath, header = TRUE)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  df = df[,grepl("Y", colnames(df))]
  #df = df %>% select(contains("Y")) # doesn't seem like select works anymore
  as.matrix(df)
  #Y_values = as.matrix(df)
  #Y_values = Y_values %>% select(contains("Y"))
})

post_sample_p_val = reactive({
  # not to be confused with p-values, the evidence against the null.
  if(input$post_comp_use == 1){ # use from the prior elicitation
    input$num_dimensions
  } else if (input$post_comp_use == 2){
    input$num_dimensions_post
  }
})

post_sample_values = reactive({
  set.seed(post_integ_seed())
  if(input$post_comp_use == 1){ # default option - use from the prior elicitation
    sample_post_computations(N = input$post_bigN,
                             Y = input_Y_values(), 
                             p = post_sample_p_val(),
                             mu0 = prior_elicitation_mu_values()$mu0,
                             lambda0 = prior_elicitation_mu_values()$lambda0
    )
  } else if (input$post_comp_use == 2){ # use what is manually inserted
    sample_post_computations(N = input$post_bigN, 
                             Y = input_Y_values(), 
                             p = post_sample_p_val(),
                             mu0 = create_necessary_vector(input$mu0_post),
                             lambda0 = create_necessary_vector(input$lambda0_post)
    )
  }
})

post_sample_sigma_ii = reactive({
  sample_sigma_ii(N = input$post_bigN, 
                  p = post_sample_p_val(), 
                  alpha01 = prior_elicitation_sigma_values()$alpha01,
                  alpha02 = prior_elicitation_sigma_values()$alpha02)
})

post_sample_weights = reactive({
  if(input$post_comp_use == 1){ # use from the prior elicitation
    weights(N = input$post_bigN, 
            p = post_sample_p_val(), 
            mu = post_sample_values()$mu_xi, 
            xi = post_sample_values()$xi, 
            mu0 = prior_elicitation_mu_values()$mu0,
            lambda0 = prior_elicitation_mu_values()$lambda0,
            sigma_ii = post_sample_sigma_ii(), #sample_prior_values()$sigma_ii,
            alpha01 = prior_elicitation_sigma_values()$alpha01,
            alpha02 = prior_elicitation_sigma_values()$alpha02)
  } else if (input$post_comp_use == 2){
    weights(N = input$post_bigN, 
            p = post_sample_p_val(), 
            mu = post_sample_values()$mu_xi, 
            xi = post_sample_values()$xi, 
            mu0 = create_necessary_vector(input$mu0_post),
            lambda0 = create_necessary_vector(input$lambda0_post),
            sigma_ii = post_sample_sigma_ii(), #sample_prior_values()$sigma_ii,
            alpha01 = prior_elicitation_sigma_values()$alpha01,
            alpha02 = prior_elicitation_sigma_values()$alpha02)
  }
})

sample_post_values_reformatted = reactive({
  sample_post_reformat(N = input$post_bigN, 
                       p = post_sample_p_val(), 
                       post_mu = post_sample_values()$mu_xi,
                       post_xi = post_sample_values()$xi, 
                       weights = post_sample_weights())
})

sample_post_values_reformatted_round = eventReactive(input$submit_sample_post, {
  round(sample_post_values_reformatted(), 4)
})

#######################################
# CODE FOR THE TABLE
#######################################

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

#######################################
# END CODE FOR THE TABLE
#######################################

#######################################
# CODE FOR THE EFFECTIVE RANGE
#######################################

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

#output$prior_eff_range_delta = renderPrint({
#  #xlim_comparison_vals()
#  prior_delta_values()
#})

#######################################
# END CODE FOR THE EFFECTIVE RANGE
#######################################


sample_post_content_values = reactive({
  posterior_content(N = input$post_bigN, 
                    p = post_sample_p_val(), 
                    effective_range = true_prior_effective_range()$grid, #sample_prior_effective_range()$grid, 
                    mu = post_sample_values()$mu_xi, 
                    xi = post_sample_values()$xi, 
                    weights = post_sample_weights())
}) 


true_prior_values = reactive({
  true_prior_comparison(p = post_sample_p_val(),
                        alpha01 = prior_elicitation_sigma_values()$alpha01,
                        alpha02 = prior_elicitation_sigma_values()$alpha02,
                        mu0 = prior_elicitation_mu_values()$mu0,
                        lambda0 = prior_elicitation_mu_values()$lambda0,
                        grid = true_prior_effective_range()$grid) #,sample_prior_effective_range()$grid)
})


rbr_content = reactive({
  relative_belief_ratio(p = post_sample_p_val(), 
                        prior_content = true_prior_values()$prior_matrix, #sample_prior_content_values()$prior_density, 
                        post_content = sample_post_content_values()$post_density)
})

# makes it so the output is delayed until the user submits the data
sample_post_computations_outputs = eventReactive(input$submit_sample_post, {
  list(
    "xi" = post_sample_values()$xi[,,1:5],
    "mu_xi" = head(post_sample_values()$mu_xi, 5)
  )
})

output$sample_post_computation = renderPrint({
  sample_post_computations_outputs()
})

# cleaning the data before being downloaded - changing the column names.

output$post_computation_download = downloadHandler(
  filename = "postsample.csv",
  content = function(file) {
    write.csv(sample_post_values_reformatted(), file, row.names = FALSE)
  }
)


# GRAPHING FUNCTIONS ###########################################

post_sample_posterior_content_graph_DOWNLOAD = function(){
  content_density_plot(density = sample_post_content_values()$post_density, 
                       col_num = input$post_graph_num, 
                       grid = true_prior_values()$midpoint_grid_matrix, 
                       type = "Posterior",
                       min_xlim = input$post_xlim_min, 
                       max_xlim = input$post_xlim_max,
                       smooth_num = input$post_graph_smoother, 
                       colour_choice = input$post_line_col,
                       lty_type = 2, # need to let user modify this
                       transparency = input$post_transparency)
}

post_sample_posterior_content_graph = eventReactive(input$submit_sample_post, {
  post_sample_posterior_content_graph_DOWNLOAD()
})

### THE OUTPUT FOR THE PLOT
output$sample_post_graph = renderPlot({
  post_sample_posterior_content_graph()
  #the_sample_post_graph() # previous before making changes
})

comparisons_content_graph_DOWNLOAD = function(){
  comparison_content_density_plot(prior_density = true_prior_values()$prior_matrix, 
                                  post_density = sample_post_content_values()$post_density,
                                  col_num = input$comparison_mu_col, 
                                  prior_grid = true_prior_values()$midpoint_grid_matrix,
                                  post_grid = true_prior_values()$midpoint_grid_matrix, 
                                  min_xlim = xlim_comparison_vals()[[input$comparison_mu_col]][1],#input$comparison_xlim_min, 
                                  max_xlim = xlim_comparison_vals()[[input$comparison_mu_col]][2],#input$comparison_xlim_max,
                                  smooth_num = c(1, input$comparison_smoother),
                                  colour_choice = c(input$comparison_prior_col, 
                                                    input$comparison_post_col),
                                  lty_type = c(as.numeric(input$comparison_prior_lty), 
                                               as.numeric(input$comparison_post_lty)), 
                                  transparency = input$comparison_transparency)
}

output$sample_priorpost_graph = renderPlot({
  comparisons_content_graph_DOWNLOAD()
  #mu_graph_comparison(grid = rbr_computation_values()$grid, 
  #                    mu_prior = rbr_computation_values()$prior_mu,  
  #                    mu_post = rbr_computation_values()$post_mu, 
  #                    col_num = input$comparison_mu_col,
  #                    min_xlim = input$comparison_xlim_min, 
  #                    max_xlim = input$comparison_xlim_max,
  #                    smooth_num = input$comparison_smoother,
  #                    colour_choice = c(input$comparison_prior_col, 
  #                                      input$comparison_post_col),
  #                    lty_type = c(as.numeric(input$comparison_prior_lty), 
  #                                 as.numeric(input$comparison_post_lty)), 
  #                    transparency = input$comparison_transparency
  #)
})

rbr_content_graph_DOWNLOAD = function(){
  content_density_plot(density = rbr_content()$RBR_modified, 
                       col_num = input$comparison_mu_col, 
                       grid = true_prior_values()$midpoint_grid_matrix, 
                       type = "RBR",
                       min_xlim = xlim_comparison_vals()[[input$comparison_mu_col]][1],#input$comparison_xlim_min, 
                       max_xlim = xlim_comparison_vals()[[input$comparison_mu_col]][2],#input$comparison_xlim_max,
                       smooth_num = input$comparison_smoother,
                       colour_choice = input$comparison_rbr_col,
                       lty_type = as.numeric(input$comparison_rbr_lty), 
                       transparency = input$comparison_transparency)
}

output$sample_rbr_graph = renderPlot({
  rbr_content_graph_DOWNLOAD()
  #rbr_mu_graph(grid = rbr_computation_values()$grid, 
  #             mu = rbr_computation_values()$rbr_mu, 
  #             type = "relative belief ratio", 
  #             smooth_num = input$comparison_smoother,
  #             col_num = input$comparison_mu_col,
  #             colour_choice = input$comparison_rbr_col,
  #             lty_type = as.numeric(input$comparison_rbr_lty),
  #             transparency = input$comparison_transparency)
})

