################################################################
# QUALITATIVE FACTORS: BACKEND FOR RELATIVE BELIEF INFERENCES  #
################################################################

output$alpha_order_output = renderTable({
  beta_vec = create_beta_list_names(levels = create_necessary_vector(input$qual_num_levels), text = "a")
  df = data.frame(t(beta_vec))
  colnames(df) = 1:length(beta_vec)
  df
})

qual_contrast = reactive({
  if(input$denote_contrasts == "select"){
    col_num = as.numeric(input$qual_alpha_contrast)
    contrast = qual_c_matrix_result()[,col_num] 
  } else if (input$denote_contrasts == "manual"){
    contrast = create_necessary_vector(input$qual_manual_contrast)
  }
  contrast
})

contrast_expression = reactive({
  contrast = qual_contrast()
  beta_vec = create_beta_list_names(levels = create_necessary_vector(input$qual_num_levels))
  dot_product_expression(coefficients = contrast, betas = beta_vec)
})

contrast_expression_latex = reactive({
  expression = dot_product_expression_latex(as.character(contrast_expression()))
  paste("$", expression, "$", sep = "")
})

output$qual_beta_combination = renderUI({
  HTML(paste0("$", contrast_expression_latex(), "$"))
})

observe({
  invalidateLater(500, session)
  session$sendCustomMessage(type = 'mathjax_reprocess', message = list())
})

qual_prior_alpha_contrasts = reactive({
  qual_sample_prior_values()$prior_beta_matrix %*% qual_contrast()
})

qual_post_alpha_contrasts = reactive({
  qual_sample_post_values()$post_beta_matrix %*% qual_contrast()
})

qual_alpha_vals_comp = reactive({
  psi_plot_vals(delta = input$qual_rbr_delta, 
                smoother = c(input$qual_rbr_mprior, input$qual_rbr_mpost), 
                prior_psi = qual_prior_alpha_contrasts(), 
                post_psi = qual_post_alpha_contrasts())
})

qual_rbr_alpha_vals = reactive({
  qual_rbr_psi(prior_psi_dens_smoothed = qual_alpha_vals_comp()$prior_psi_dens_smoothed, 
          post_psi_dens_smoothed = qual_alpha_vals_comp()$post_psi_dens_smoothed, 
          breaks = qual_alpha_vals_comp()$breaks)
})

qual_plausible_region_est = reactive({
  plausible_region_est(prior_psi_mids = qual_alpha_vals_comp()$psi_mids, 
                       RB_psi = qual_rbr_alpha_vals(), 
                       post_psi_dens_smoothed = qual_alpha_vals_comp()$post_psi_dens_smoothed, 
                       delta_psi = input$qual_rbr_delta)
})

# debugging!!!
#output$qualdebugging1234 = renderPrint({
#  qual_alpha_vals_comp()
#})

qual_hypothesis_test = reactive({
  psi_hypothesis_test(psi_0 = as.numeric(input$qual_alpha0), 
                      prior_psi_mids = qual_alpha_vals_comp()$psi_mids, 
                      RB_psi = qual_rbr_alpha_vals(), 
                      post_psi_dens_smoothed = qual_alpha_vals_comp()$post_psi_dens_smoothed, 
                      delta_psi = input$qual_rbr_delta)
})

qual_RBest_value = reactive({
  qual_rbr_alpha_vals()[which.max(qual_rbr_alpha_vals())]
})

output$qual_psi_hypo_test_output = renderPrint({
  list1 = list("Estimate of true value of psi from the relative belief ratio" = qual_RBest_value(),
               "Plausible region" = qual_plausible_region_est()$plaus_interval,
               "Posterior content of the plausible region" = qual_plausible_region_est()$plaus_content)
  if(input$qual_alpha_null == 1){
    list2 = list("The evidence concerning strength H_0 : psi = psi_0" = qual_hypothesis_test()$psi_message,
                 "The strength" = qual_hypothesis_test()$strength_message) 
    list1 = c(list1, list2)
  }
  list1
})

output$qual_psi_hypo_test_output1 = renderPrint({
  cat(qual_RBest_value())
})

output$qual_psi_hypo_test_output2 = renderPrint({
  upper_bd = qual_plausible_region_est()$plaus_interval[2]
  lower_bd = qual_plausible_region_est()$plaus_interval[1]
  cat(paste("(", lower_bd, ", ", upper_bd, ")", sep=""))
})

output$qual_psi_hypo_test_output3 = renderPrint({
  cat(qual_plausible_region_est()$plaus_content)
})

output$qual_psi_hypo_test_output4 = renderPrint({
  cat(qual_hypothesis_test()$psi_message)
})

output$qual_psi_hypo_test_output5 = renderPrint({
  #cat(qual_hypothesis_test()$strength_message)
  cat(qual_hypothesis_test()$strength)
})

################################################################
# PLOTS FOR THE RELATIVE BELIEF RATIO                          #
################################################################

qual_prior_post_plot = function(){
  psi_priorpost_plot(grid = qual_alpha_vals_comp()$psi_mids, 
                     prior_density = qual_alpha_vals_comp()$prior_psi_dens_smoothed, 
                     post_density = qual_alpha_vals_comp()$post_psi_dens_smoothed, 
                     plot_object = contrast_expression_latex(),
                     colour_choice = c(input$qual_comparison_prior_col, 
                                       input$qual_comparison_post_col), 
                     lty_type = c(as.numeric(input$qual_comparison_prior_lty), 
                                  as.numeric(input$qual_comparison_post_lty)),
                     transparency = input$qual_comparison_transparency, 
                     xlim_min = input$qual_psi_plot_xmin, 
                     xlim_max = input$qual_psi_plot_xmax)
}

qual_prior_only_plot = function(){
  psi_cust_plot(grid = qual_alpha_vals_comp()$psi_mids, 
                density = qual_alpha_vals_comp()$prior_psi_dens_smoothed, 
                colour_choice = input$qual_comparison_prior_col, 
                lty_type = as.numeric(input$qual_comparison_prior_lty),  
                transparency = input$qual_comparison_transparency, 
                plot_title = "Prior", 
                plot_object = contrast_expression_latex(),
                xlim_min = input$qual_psi_plot_xmin, 
                xlim_max = input$qual_psi_plot_xmax)
}

qual_post_only_plot = function(){
  psi_cust_plot(grid = qual_alpha_vals_comp()$psi_mids, 
                density = qual_alpha_vals_comp()$post_psi_dens_smoothed, 
                colour_choice = input$qual_comparison_post_col, 
                lty_type = as.numeric(input$qual_comparison_post_lty),  
                transparency = input$qual_comparison_transparency, 
                plot_title = "Posterior", 
                plot_object = contrast_expression_latex(),
                xlim_min = input$qual_psi_plot_xmin, 
                xlim_max = input$qual_psi_plot_xmax)
}

qual_rbr_plot = function(){
  psi_cust_plot(grid = qual_alpha_vals_comp()$psi_mids, 
                density = qual_rbr_alpha_vals(), 
                colour_choice = input$qual_comparison_rbr_col,  
                lty_type = as.numeric(input$qual_comparison_rbr_lty), 
                transparency = input$qual_comparison_transparency, 
                plot_title = "Relative Belief Ratio",
                plot_object = contrast_expression_latex(),
                xlim_min = input$qual_psi_plot_xmin, 
                xlim_max = input$qual_psi_plot_xmax)
}

output$qual_prior_alpha_plot = renderPlot({
  qual_prior_only_plot()
})

output$qual_post_alpha_plot = renderPlot({
  qual_post_only_plot()
})

output$qual_rbr_alpha_plot1 = renderPlot({
  qual_rbr_plot()
})

output$qual_rbr_alpha_plot2 = renderPlot({
  qual_rbr_plot()
})

output$qual_priorpost_alpha_plot = renderPlot({
  qual_prior_post_plot()
})
