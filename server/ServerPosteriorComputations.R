################################################################
# BACKEND FOR POSTERIOR COMPUTATIONS                           #
################################################################

post_integ_seed = reactive(input$post_seed)

input_Y_values = reactive({
  #as.matrix(test_sample_data())
  tryCatch(
    {
      df = read.csv(input$sample_post_Y$datapath, header = TRUE)
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

post_sample_values = reactive({
  set.seed(post_integ_seed())
  if(input$post_comp_use == 1){ # default option - use from the prior elicitation
    sample_post_computations(N = input$post_bigN,
                             Y = input_Y_values(), 
                             p = input$num_dimensions,
                             mu0 = prior_elicitation_mu_values()$mu0,
                             lambda0 = prior_elicitation_mu_values()$lambda0
    )
  } else if (input$post_comp_use == 2){ # use what is manually inserted
    sample_post_computations(N = input$post_bigN, 
                             Y = input_Y_values(), 
                             p = input$num_dimensions_post,
                             mu0 = create_necessary_vector(input$mu0_post),
                             lambda0 = create_necessary_vector(input$lambda0_post)
    )
  }
})

post_sample_weights = reactive({
  if(input$post_comp_use == 1){ # use from the prior elicitation
    weights(N = input$post_bigN, 
            p = input$num_dimensions, 
            mu = post_sample_values()$mu_xi, 
            xi = post_sample_values()$xi, 
            mu0 = prior_elicitation_mu_values()$mu0,
            lambda0 = prior_elicitation_mu_values()$lambda0,
            sigma_ii = sample_prior_values()$sigma_ii,
            alpha01 = prior_elicitation_sigma_values()$alpha01,
            alpha02 = prior_elicitation_sigma_values()$alpha02)
  } else if (input$post_comp_use == 2){
    weights(N = input$post_bigN, 
            p = input$num_dimensions_post, 
            mu = post_sample_values()$mu_xi, 
            xi = post_sample_values()$xi, 
            mu0 = create_necessary_vector(input$mu0_post),
            lambda0 = create_necessary_vector(input$lambda0_post),
            sigma_ii = sample_prior_values()$sigma_ii,
            alpha01 = prior_elicitation_sigma_values()$alpha01,
            alpha02 = prior_elicitation_sigma_values()$alpha02)
  }
})

post_sample_p_val = reactive({
  # not to be confused with p-values, the evidence against the null.
  if(input$post_comp_use == 1){ # use from the prior elicitation
    input$num_dimensions
  } else if (input$post_comp_use == 2){
    input$num_dimensions_post
  }
})

sample_post_content_values = reactive({
  posterior_content(N = input$post_bigN, 
                    p = post_sample_p_val(), 
                    effective_range = sample_prior_content_values()$effective_range, 
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
                        grid = sample_prior_content_values()$plotting_grid)
})

output$debugging_prior = renderPrint({
  true_prior_values()
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
download_post_sample_mu = reactive({
  data = as.data.frame(post_sample_values()$mu_xi)
  for(i in 1:ncol(data)){
    colnames(data)[i] = paste("mu", i, sep = " ")
  }
  data
})

output$postsample_download_mu = downloadHandler(
  filename = "postsample_mu.csv",
  content = function(file) {
    write.csv(download_post_sample_mu(), file, row.names = FALSE)
  }
)

output$postsample_download_xi = downloadHandler(
  filename = "postsample_xi.csv",
  content = function(file) {
    # Note: difficult to change the column names for this particular case.
    # todo: try to edit it later...
    write.csv(post_sample_values()$xi, file, row.names = FALSE)
  }
)

# GRAPHING FUNCTIONS ###########################################

#output$testing_post = renderPrint({
#  sample_post_content_values()$post_density
#})

post_sample_posterior_content_graph_DOWNLOAD = function(){
  content_density_plot(density = sample_post_content_values()$post_density, 
                       col_num = input$post_graph_num, 
                       grid = sample_prior_content_values()$plotting_grid, 
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
                                  prior_grid = sample_prior_content_values()$plotting_grid,
                                  post_grid = sample_prior_content_values()$plotting_grid, 
                                  min_xlim = input$comparison_xlim_min, 
                                  max_xlim = input$comparison_xlim_max,
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
                       grid = sample_prior_content_values()$plotting_grid, 
                       type = "RBR",
                       min_xlim = input$comparison_xlim_min, 
                       max_xlim = input$comparison_xlim_max,
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

