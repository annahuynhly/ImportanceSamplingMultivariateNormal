################################################################
# QUALITATIVE FACTORS: BACKEND FOR RELATIVE BELIEF INFERENCES  #
################################################################

qual_prior_alpha_contrasts = reactive({
  col_num = input$qual_column_num_rbr
  C = qual_sufficient_stat_comp_manual()$C
  (t(C) %*% t(qual_sample_prior_values()$prior_beta_matrix))[col_num,]
})

qual_post_alpha_contrasts = reactive({
  col_num = input$qual_column_num_rbr
  C = qual_sufficient_stat_comp_manual()$C
  (t(C) %*% t(qual_sample_post_values()$post_beta_matrix))[col_num,]
})

qual_prior_alpha_vals_comp = reactive({
  alpha_plot_vals(Nmontecarlo = input$qual_prior_sample_bigN, 
                  smoother = input$qual_rbr_mprior, 
                  delta = input$qual_rbr_delta, 
                  alpha_vals = qual_prior_alpha_contrasts())
})

qual_post_alpha_vals_comp = reactive({
  alpha_plot_vals(Nmontecarlo = input$qual_post_sample_bigN, 
                  smoother = input$qual_rbr_mprior, 
                  delta = input$qual_rbr_delta, 
                  alpha_vals = qual_post_alpha_contrasts())
})

qual_rbr_alpha_vals = reactive({
  
  prior_alpha_val = qual_prior_alpha_vals_comp()
  post_alpha_val = qual_post_alpha_vals_comp()
  
  rbr_alpha(prior_alpha_dens_smoothed = prior_alpha_val$alpha_dens_smoothed, 
            prior_alpha_breaks = prior_alpha_val$breaks, 
            post_alpha_dens_smoothed = post_alpha_val$alpha_dens_smoothed, 
            post_alpha_breaks = post_alpha_val$breaks)
})

qual_plausible_region_est = reactive({
  plausible_region_est(prior_psi_mids = qual_rbr_alpha_vals()$RB_mids, 
                       RB_psi = qual_rbr_alpha_vals()$RB_alpha, 
                       post_psi_dens_smoothed = qual_rbr_alpha_vals()$post_alpha_dens_smoothed, 
                       delta_psi = input$qual_rbr_delta)
})

qual_hypothesis_test = reactive({
  psi_hypothesis_test(psi_0 = input$qual_alpha_null, 
                      prior_psi_mids = qual_rbr_alpha_vals()$RB_mids, 
                      RB_psi = qual_rbr_alpha_vals()$RB_alpha, 
                      post_psi_dens_smoothed = qual_rbr_alpha_vals()$post_alpha_dens_smoothed, 
                      delta_psi = input$qual_rbr_delta)
})

qual_RBest_value = reactive({
  rbr_alpha_vals = qual_rbr_alpha_vals()
  rbr_alpha_vals$RB_mids[which.max(rbr_alpha_vals$RB_alpha)]
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

################################################################
# PLOTS FOR THE RELATIVE BELIEF RATIO                          #
################################################################

qual_prior_post_plot = function(){
  rbr_alpha_vals = qual_rbr_alpha_vals()
  psi_priorpost_plot(grid = rbr_alpha_vals$RB_mids, 
                     prior_density = rbr_alpha_vals$prior_alpha_dens_smoothed, 
                     post_density = rbr_alpha_vals$post_alpha_dens_smoothed, 
                     plot_object = "$\\alpha_{0}$",
                     colour_choice = c(input$qual_comparison_prior_col, 
                                       input$qual_comparison_post_col), 
                     lty_type = c(as.numeric(input$qual_comparison_prior_lty), 
                                  as.numeric(input$qual_comparison_post_lty)),
                     transparency = input$qual_comparison_transparency, 
                     xlim_min = input$qual_psi_plot_xmin, 
                     xlim_max = input$qual_psi_plot_xmax)
}

qual_prior_only_plot = function(){
  rbr_alpha_vals = qual_rbr_alpha_vals()
  psi_cust_plot(grid = rbr_alpha_vals$RB_mids, 
                density = rbr_alpha_vals$prior_alpha_dens_smoothed, 
                colour_choice = input$qual_comparison_prior_col, 
                lty_type = as.numeric(input$qual_comparison_prior_lty),  
                transparency = input$qual_comparison_transparency, 
                plot_title = "Prior", plot_object = "$\\alpha_{0}$",
                xlim_min = input$qual_psi_plot_xmin, 
                xlim_max = input$qual_psi_plot_xmax)
}

qual_post_only_plot = function(){
  rbr_alpha_vals = qual_rbr_alpha_vals()
  psi_cust_plot(grid = rbr_alpha_vals$RB_mids, 
                density = rbr_alpha_vals$post_alpha_dens_smoothed, 
                colour_choice = input$qual_comparison_post_col, 
                lty_type = as.numeric(input$qual_comparison_post_lty),  
                transparency = input$qual_comparison_transparency, 
                plot_title = "Posterior", plot_object = "$\\alpha_{0}$",
                xlim_min = input$qual_psi_plot_xmin, 
                xlim_max = input$qual_psi_plot_xmax)
}

qual_rbr_plot = function(){
  rbr_alpha_vals = qual_rbr_alpha_vals()
  psi_cust_plot(grid = rbr_alpha_vals$RB_mids, 
                density = rbr_alpha_vals$RB_alpha, 
                colour_choice = input$qual_comparison_rbr_col,  
                lty_type = as.numeric(input$qual_comparison_rbr_lty), 
                transparency = input$qual_comparison_transparency, 
                plot_title = "Relative Belief Ratio",
                xlim_min = input$qual_psi_plot_xmin, 
                xlim_max = input$qual_psi_plot_xmax)
}

output$qualdebugging1234 = renderPrint({
  "lalala"
})

output$qual_prior_alpha_plot = renderPlot({
  qual_prior_only_plot()
})

output$qual_post_alpha_plot = renderPlot({
  qual_post_only_plot()
})

output$qual_rbr_alpha_plot = renderPlot({
  qual_rbr_plot()
})

output$qual_priorpost_alpha_plot = renderPlot({
  qual_prior_post_plot()
})
