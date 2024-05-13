################################################################
# CODE FOR COMPUTING POSTERIOR CONTENT                         #
################################################################

sample_post_content_values = reactive({
  # This is based off of the effective range for the prior.
  posterior_content(N = input$post_bigN, 
                    p = post_sample_p_val(), 
                    effective_range = true_prior_effective_range()$grid, #sample_prior_effective_range()$grid, 
                    mu = post_sample_values()$mu_xi, 
                    xi = post_sample_values()$xi, 
                    weights = post_sample_weights()$weights_vector)
}) 

post_manual_grid = reactive({
  grid_size = as.numeric(input$prior_eff_range_m)+1
  lwr = as.numeric(input$post_graph_lwr_bd)
  upp = as.numeric(input$post_graph_upp_bd)
  
  grid = seq(from = lwr, to = upp, length.out = grid_size)
  grids = vector("list", post_sample_p_val())
  grids[] = list(grid)
  
  grids
})

sample_post_content_values_mod = reactive({
  # This is based off of the effective range for the posterior.
  # It seems redundant - but want to save computational time in case the user wants to
  # switch between the two.
  posterior_content(N = input$post_bigN, 
                    p = post_sample_p_val(), 
                    effective_range = post_manual_grid(),
                    mu = post_sample_values()$mu_xi, 
                    xi = post_sample_values()$xi, 
                    weights = post_sample_weights()$weights_vector)
})

#output$testing_post_graph = renderPrint({
  #debugging
#  sample_post_content_values_mod()
#})

true_prior_values = reactive({
  # This is based off of the effective range for the prior.
  true_prior_comparison(p = post_sample_p_val(),
                        alpha01 = prior_elicitation_sigma_values()$alpha01,
                        alpha02 = prior_elicitation_sigma_values()$alpha02,
                        mu0 = prior_elicitation_mu_values()$mu0,
                        lambda0 = prior_elicitation_mu_values()$lambda0,
                        grid = true_prior_effective_range()$grid) #,sample_prior_effective_range()$grid)
})

true_prior_values_mod = reactive({
  # This is based off of the manually inserted range for the posterior.
  true_prior_comparison(p = post_sample_p_val(),
                        alpha01 = prior_elicitation_sigma_values()$alpha01,
                        alpha02 = prior_elicitation_sigma_values()$alpha02,
                        mu0 = prior_elicitation_mu_values()$mu0,
                        lambda0 = prior_elicitation_mu_values()$lambda0,
                        grid = post_manual_grid()) 
})

rbr_content = reactive({
  # This is based off of the effective range for the prior.
  relative_belief_ratio(p = post_sample_p_val(), 
                        prior_content = true_prior_values()$prior_matrix, #sample_prior_content_values()$prior_density, 
                        post_content = sample_post_content_values()$post_density)
})

rbr_content_mod = reactive({
  # This is based off of the manually inserted range for the posterior.
  relative_belief_ratio(p = post_sample_p_val(), 
                        prior_content = true_prior_values_mod()$prior_matrix,  
                        post_content = sample_post_content_values_mod()$post_density)
})

output$sample_post_computation = renderPrint({
  sample_post_computations_outputs()
})

output$imp_computation_download = downloadHandler(
  filename = "importance_sampler.csv",
  content = function(file) {
    write.csv(important_values_reformatted(), file, row.names = FALSE)
  }
)

output$post_computation_download = downloadHandler(
  filename = "sample_post_values.csv",
  content = function(file) {
    write.csv(post_SIR_calculations_reformat(), file, row.names = FALSE)
  }
)



################################################################
# CODE FOR GRAPHING                                            #
################################################################

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

comparisons_content_graph_DOWNLOAD_mod = function(){
  # modified version to make up for the manually insertted posterior
  comparison_content_density_plot(prior_density = true_prior_values_mod()$prior_matrix, 
                                  post_density = sample_post_content_values_mod()$post_density,
                                  col_num = input$comparison_mu_col, 
                                  prior_grid = true_prior_values_mod()$midpoint_grid_matrix,
                                  post_grid = true_prior_values_mod()$midpoint_grid_matrix, 
                                  min_xlim = input$post_graph_lwr_bd, 
                                  max_xlim = input$post_graph_upp_bd,
                                  smooth_num = c(1, input$comparison_smoother),
                                  colour_choice = c(input$comparison_prior_col, 
                                                    input$comparison_post_col),
                                  lty_type = c(as.numeric(input$comparison_prior_lty), 
                                               as.numeric(input$comparison_post_lty)), 
                                  transparency = input$comparison_transparency)
}


output$sample_priorpost_graph = renderPlot({
  if(input$post_interval_use == 1){
    comparisons_content_graph_DOWNLOAD()
  } else if (input$post_interval_use == 2){
    comparisons_content_graph_DOWNLOAD_mod()
  }
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

rbr_content_graph_DOWNLOAD_mod = function(){
  # modified version
  content_density_plot(density = rbr_content_mod()$RBR_modified, 
                       col_num = input$comparison_mu_col, 
                       grid = true_prior_values_mod()$midpoint_grid_matrix, 
                       type = "RBR",
                       min_xlim = input$post_graph_lwr_bd, 
                       max_xlim = input$post_graph_upp_bd,
                       smooth_num = input$comparison_smoother,
                       colour_choice = input$comparison_rbr_col,
                       lty_type = as.numeric(input$comparison_rbr_lty), 
                       transparency = input$comparison_transparency)
}

output$sample_rbr_graph = renderPlot({
  if(input$post_interval_use == 1){
    rbr_content_graph_DOWNLOAD()
  } else if (input$post_interval_use == 2){
    rbr_content_graph_DOWNLOAD_mod()
  }
})
