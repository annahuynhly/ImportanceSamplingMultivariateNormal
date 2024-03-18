################################################################
# CODE FOR COMPUTING POSTERIOR CONTENT                         #
################################################################

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

output$sample_post_computation = renderPrint({
  sample_post_computations_outputs()
})

output$post_computation_download = downloadHandler(
  filename = "postsample.csv",
  content = function(file) {
    write.csv(sample_post_values_reformatted(), file, row.names = FALSE)
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

output$sample_priorpost_graph = renderPlot({
  comparisons_content_graph_DOWNLOAD()
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
})

