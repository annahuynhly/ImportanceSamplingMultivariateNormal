################################################################
# PLOTTING                                                     #
################################################################

prior_psi_plot_DOWNLOAD = function(){
  psi_cust_plot(grid = prior_psi_values()$prior_psi_mids, 
                density = prior_psi_values()$prior_psi_dens_smoothed, 
                colour_choice = input$comparison_prior_col,
                lty_type = as.numeric(input$comparison_prior_lty), 
                transparency = input$comparison_transparency, 
                plot_title = "Prior Density",
                xlim_min = as.numeric(input$psi_plot_xmin),
                xlim_max = as.numeric(input$psi_plot_xmax))
}

post_psi_plot_DOWNLOAD = function(){
  psi_cust_plot(grid = prior_psi_values()$prior_psi_mids, 
                density = post_psi_values(), 
                colour_choice = input$comparison_post_col,
                lty_type = as.numeric(input$comparison_post_lty), 
                transparency = input$comparison_transparency, 
                plot_title = "Posterior Density",
                xlim_min = as.numeric(input$psi_plot_xmin),
                xlim_max = as.numeric(input$psi_plot_xmax))
}

priorpost_psi_plot_DOWNLOAD = function(){
  psi_priorpost_plot(grid = prior_psi_values()$prior_psi_mids,
                     prior_density = prior_psi_values()$prior_psi_dens_smoothed, 
                     post_density = post_psi_values(), 
                     colour_choice = c(input$comparison_prior_col, 
                                       input$comparison_post_col), 
                     lty_type = c(as.numeric(input$comparison_prior_lty), 
                                  as.numeric(input$comparison_post_lty)),
                     transparency = input$comparison_transparency,
                     xlim_min = as.numeric(input$psi_plot_xmin),
                     xlim_max = as.numeric(input$psi_plot_xmax))
}

rbr_psi_plot_DOWNLOAD = function(){
  psi_cust_plot(grid = prior_psi_values()$prior_psi_mids, 
                density = rbr_psi_values(), 
                colour_choice = input$comparison_rbr_col,
                lty_type = as.numeric(input$comparison_rbr_lty), 
                transparency = input$comparison_transparency, 
                plot_title = "Relative Belief Ratio",
                xlim_min = as.numeric(input$psi_plot_xmin),
                xlim_max = as.numeric(input$psi_plot_xmax))
}

##
prior_psi_plot_DOWNLOAD_alt = function(){
  psi_cust_plot(grid = prior_psi_values_alt()$prior_psi_mids, 
                density = prior_psi_values_alt()$prior_psi_dens_smoothed, 
                colour_choice = input$comparison_prior_col,
                lty_type = as.numeric(input$comparison_prior_lty), 
                transparency = input$comparison_transparency, 
                plot_title = "Prior Density",
                xlim_min = as.numeric(input$psi_plot_xmin),
                xlim_max = as.numeric(input$psi_plot_xmax))
}

post_psi_plot_DOWNLOAD_alt = function(){
  psi_cust_plot(grid = prior_psi_values_alt()$prior_psi_mids, 
                density = post_psi_values_alt(), 
                colour_choice = input$comparison_post_col,
                lty_type = as.numeric(input$comparison_post_lty), 
                transparency = input$comparison_transparency, 
                plot_title = "Posterior Density",
                xlim_min = as.numeric(input$psi_plot_xmin),
                xlim_max = as.numeric(input$psi_plot_xmax))
}

priorpost_psi_plot_DOWNLOAD_alt = function(){
  psi_priorpost_plot(grid = prior_psi_values_alt()$prior_psi_mids,
                     prior_density = prior_psi_values_alt()$prior_psi_dens_smoothed, 
                     post_density = post_psi_values_alt(), 
                     colour_choice = c(input$comparison_prior_col, 
                                       input$comparison_post_col), 
                     lty_type = c(as.numeric(input$comparison_prior_lty), 
                                  as.numeric(input$comparison_post_lty)),
                     transparency = input$comparison_transparency,
                     xlim_min = as.numeric(input$psi_plot_xmin),
                     xlim_max = as.numeric(input$psi_plot_xmax))
}

rbr_psi_plot_DOWNLOAD_alt = function(){
  psi_cust_plot(grid = prior_psi_values_alt()$prior_psi_mids, 
                density = rbr_psi_values_alt(), 
                colour_choice = input$comparison_rbr_col,
                lty_type = as.numeric(input$comparison_rbr_lty), 
                transparency = input$comparison_transparency, 
                plot_title = "Relative Belief Ratio",
                xlim_min = as.numeric(input$psi_plot_xmin),
                xlim_max = as.numeric(input$psi_plot_xmax))
}

##

output$prior_psi_plot = renderPlot({
  if(input$psi_value_type == 1){
    prior_psi_plot_DOWNLOAD()
  } else {
    prior_psi_plot_DOWNLOAD_alt()
  }
})

output$post_psi_plot = renderPlot({
  if(input$psi_value_type == 1){
    post_psi_plot_DOWNLOAD()
  } else {
    post_psi_plot_DOWNLOAD_alt()
  }
})

output$priorpost_psi_plot = renderPlot({
  if(input$psi_value_type == 1){
    priorpost_psi_plot_DOWNLOAD()
  } else {
    priorpost_psi_plot_DOWNLOAD_alt()
  }
})

output$rbr_psi_plot = renderPlot({
  if(input$psi_value_type == 1){
    rbr_psi_plot_DOWNLOAD()
  } else {
    rbr_psi_plot_DOWNLOAD_alt()
  }
})

output$rbr_psi_plot_duplicate = renderPlot({
  if(input$psi_value_type == 1){
    rbr_psi_plot_DOWNLOAD()
  } else {
    rbr_psi_plot_DOWNLOAD_alt()
  }
})

################################################################
# DOWNLOADING INFORMATION                                      #
################################################################

output$imp_computation_download = downloadHandler(
  filename = "importance_sample.csv",
  content = function(file) {
    write.csv(important_values_reformatted(), file, row.names = FALSE)
  }
)

output$post_computation_download = downloadHandler(
  filename = "post_sample.csv",
  content = function(file) {
    write.csv(post_SIR_calculations_reformat(), file, row.names = FALSE)
  }
)

output$download_psi_plots = downloadHandler(
  filename = "Psi Comparison Plots.png",
  content = function(file) {
    png(file, width = 2000, height = 850, units = "px", res = 200)
    
    if(input$psi_value_type == 1){
      if(input$rbr_graph_layout == 1){
        par(mfrow = c(1, 3)) 
        prior_psi_plot_DOWNLOAD()
        post_psi_plot_DOWNLOAD()
        rbr_psi_plot_DOWNLOAD()
      } else if(input$rbr_graph_layout == 2){
        par(mfrow = c(1, 2)) 
        priorpost_psi_plot_DOWNLOAD()
        rbr_psi_plot_DOWNLOAD()
      }
    } else {
      if(input$rbr_graph_layout == 1){
        par(mfrow = c(1, 3)) 
        prior_psi_plot_DOWNLOAD_alt()
        post_psi_plot_DOWNLOAD_alt()
        rbr_psi_plot_DOWNLOAD_alt()
      } else if(input$rbr_graph_layout == 2){
        par(mfrow = c(1, 2)) 
        priorpost_psi_plot_DOWNLOAD_alt()
        rbr_psi_plot_DOWNLOAD_alt()
      }
    }
    dev.off()
  }
)

