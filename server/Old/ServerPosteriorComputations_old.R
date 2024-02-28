
################################################################
# OLD PLOTS - KEPT IN CASE WE NEED IT LATER                    #
################################################################


# plotting for the relative belief ratio
rbr_computation_values = reactive({
  # note: this requires mandatory inputs from the prior explicitly.
  compute_rbr(gamma = input$virtual_uncertainty, 
              delta = input$comparison_graph_delta, 
              alpha01 = prior_elicitation_sigma_values()$alpha01,
              alpha02 = prior_elicitation_sigma_values()$alpha02, 
              m1 = prior_elicitation_mu_values()$m1, 
              m2 = prior_elicitation_mu_values()$m2,
              mu_post = post_sample_values()$mu_xi,
              min_xlim = input$comparison_xlim_min, 
              max_xlim = input$comparison_xlim_max)
})




# PLOT PREVIOUSLY USED TO SHOW THE POSTERIOR CONTENT. MAY NEED TO MOVE LATER.
the_sample_post_graph = eventReactive(input$submit_sample_post, {
  mu_graph(
    mu = post_sample_values()$mu_xi, 
    type = "Posterior",
    col_num = input$post_graph_num,
    delta = input$post_graph_delta,
    smooth_num = input$post_graph_smoother,
    colour_choice = c(input$post_line_col, input$post_hist_col),
    lty_type = 2,
    transparency = input$post_transparency)
})








## the new download is below

sample_prior_computations_graph_DOWNLOAD = function(){
  if(input$priorsample_use_NEW == "n"){
    hyperpara = sample_hyperparameters(
      gamma = input$virtual_uncertainty_prior, 
      alpha01 = alpha01_prior(),
      alpha02 = alpha02_prior(),
      m1 = m1_prior(),
      m2 = m2_prior()
    )
    alpha01 = alpha01_prior()
    alpha02 = alpha02_prior()
  } else if (input$priorsample_use_NEW == "y"){
    hyperpara = sample_hyperparameters(
      gamma = input$virtual_uncertainty, 
      alpha01 = prior_elicitation_values()$alpha01, 
      alpha02 = prior_elicitation_values()$alpha02, 
      m1 = m1_list(),
      m2 = m2_list()
    )
    alpha01 = prior_elicitation_values()$alpha01
    alpha02 = prior_elicitation_values()$alpha02 
  }
  mu0 = hyperpara$mu0
  lambda0 = hyperpara$lambda0
  
  col = input$prior_graph_num
  x = -10+20*c(0:1000)/1000
  y = dt(x,2*alpha01[col])
  scale = sqrt(alpha02[col]/alpha01[col])*lambda0[col]
  xnew = mu0[col] + scale*x
  ynew = y/scale
  
  if(input$prior_graph_hist == "y"){
    hist(prior_sample_values_NEW(), prob = TRUE,
         xlab = TeX(paste("Value of $\\mu_{", col, "}$")),
         ylab = "Density", 
         main = TeX(paste("Density plot of", r'($\mu$)', col)),
         border = "#ffffff")
    lines(xnew, ynew, lwd = 2, lty = 2)
  } else {
    plot(xnew, ynew, lwd = 2, type="l", 
         xlab = TeX(paste("Value of $\\mu_{", col, "}$")),
         ylab = "Density", 
         main = TeX(paste("Density plot of $\\mu_{", col, "}$"))
    )
  }
  
}

output$plot_prior_mu = downloadHandler(
  filename = "Prior Mu Plot.png",
  content = function(file){
    png(file)
    sample_prior_computations_graph_DOWNLOAD()
    dev.off()
  }
)

sample_post_computations_graph_DOWNLOAD = function(){
  mu_graph(
    mu = post_sample_values()$mu_xi, 
    type = "Posterior",
    col_num = input$post_graph_num,
    delta = input$post_graph_delta,
    smooth_num = input$post_graph_smoother,
    colour_choice = c(input$post_line_col, 
                      input$post_hist_col),
    lty_type = 2,
    transparency = input$post_transparency)
}

output$plot_post_mu = downloadHandler(
  filename = "Post Mu Plot.png",
  content = function(file){
    png(file)
    sample_post_computations_graph_DOWNLOAD()
    dev.off()
  }
)

output$comparison_download_plot = downloadHandler(
  filename = "Comparison Plot.png",
  content = function(file){
    png(file)
    mu_graph_comparison(grid = rbr_computation_values()$grid, 
                        mu_prior = rbr_computation_values()$prior_mu, 
                        mu_post = rbr_computation_values()$post_mu, 
                        col_num = input$comparison_mu_col,
                        smooth_num = input$comparison_smoother,
                        colour_choice = c(input$comparison_prior_col, 
                                          input$comparison_post_col),
                        lty_type = c(as.numeric(input$comparison_prior_lty), 
                                     as.numeric(input$comparison_post_lty)), 
                        transparency = input$comparison_transparency)
    dev.off()
  }
)

output$rbr_download_plot = downloadHandler(
  filename = "RBR Plot.png",
  content = function(file){
    png(file)
    rbr_mu_graph(grid = rbr_computation_values()$grid, 
                 mu = rbr_computation_values()$rbr_mu, 
                 type = "relative belief ratio", 
                 smooth_num = input$comparison_smoother,
                 col_num = input$comparison_mu_col,
                 colour_choice = input$comparison_rbr_col,
                 lty_type = as.numeric(input$comparison_rbr_lty),
                 transparency = input$comparison_transparency)
    dev.off()
  }
)



