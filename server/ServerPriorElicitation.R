################################################################
# BACKEND FOR PRIOR ELICITATION                                #
################################################################

# Prior Elicitation for sigma_{i}^{2} ##########################

const_list = reactive({create_necessary_vector(input$const_s)})
m1_list = reactive({create_necessary_vector(input$m1)})
m2_list = reactive({create_necessary_vector(input$m2)})

prior_elicitation_sigma_values = eventReactive(input$submit_prior_elicit_sigma, {
  elicit_prior_sigma_function(p = input$num_dimensions,
                              gamma = input$virtual_uncertainty,
                              s1 = create_necessary_vector(input$elicit_s1), 
                              s2 = create_necessary_vector(input$elicit_s2), 
                              upper_bd = create_necessary_vector(input$alphaup), 
                              lower_bd = create_necessary_vector(input$alphalow))
})

# This includes the raw results printed out; 
# replaced by a table for now for better formatting.
output$elicit_prior_calculation = renderPrint({
  # Note: this is separate such that it can be re-used in the other algorithm.
  prior_elicitation_sigma_values()
})

# Table
output$prior_elicit_sigma_table = renderTable({
  data.frame(index  = seq(1, as.numeric(input$num_dimensions)),
             alpha01 = prior_elicitation_sigma_values()$alpha01,
             alpha02 = prior_elicitation_sigma_values()$alpha02)
})

# Graphs
elicit_prior_graphs = function(){
  graph_num = input$prior_elicit_sigma_graphnum
  alpha = prior_elicitation_sigma_values()$alpha01[graph_num]
  beta = prior_elicitation_sigma_values()$alpha02[graph_num]
  low = prior_elicitation_sigma_values()$c1[graph_num]
  up = prior_elicitation_sigma_values()$c2[graph_num]
  z0 = prior_elicitation_sigma_values()$z0
  x = low+(up-low)*c(0:1000)/1000
  prob_z = round(pnorm(z0), 4)
  
  if(input$elicit_sigma_graph_type == 1){
    x3 = sqrt(1/x)
    dens3 = 2*(x^(3/2))*dgamma(x,alpha,beta)
    plot(x3,dens3,
         main = TeX(paste("Prior Density of $\\sigma_{", graph_num, "}$")),
         xlab = TeX(paste("$\\sigma_{", graph_num, "}$")),
         ylab = "Prior Density",
         type = "l")
  } else if (input$elicit_sigma_graph_type == 2){
    x3 = z0*sqrt(1/x)
    dens3 = (2/z0)*(x^(3/2))*dgamma(x, alpha, beta)
    plot(x3, dens3,
         main = TeX(paste("Prior Density of $\\sigma_{", graph_num, 
                          '}\\cdot z_{0}$')),
         xlab = TeX(paste('$\\sigma_{', graph_num, 
                          '} \\cdot z_{0}$')),
         ylab = "Prior Density",
         type = "l")
  }
}

output$elicit_prior_graph = renderPlot({
  # clean up the rest later - this is a placeholder. Will turn it into a function to
  # make it easier to read in the future.
  elicit_prior_graphs()
})


# The plot for it to be saved
output$download_prior_elicit_sigma = downloadHandler(
  filename = "Prior Elicit Sigma Plot.png",
  content = function(file){
    png(file)
    elicit_prior_graphs()
    dev.off()
})

# Prior Elicitation for mu_{i} #################################

prior_elicitation_mu_values = eventReactive(input$submit_prior_elicit_mu, {
  elicit_prior_mu_function(p = input$num_dimensions, 
                           gamma = input$virtual_uncertainty, 
                           m1 = m1_list(), 
                           m2 = m2_list(), 
                           s1 = create_necessary_vector(input$elicit_s1), 
                           s2 = create_necessary_vector(input$elicit_s2), 
                           alpha01 = prior_elicitation_sigma_values()$alpha01, 
                           alpha02 = prior_elicitation_sigma_values()$alpha02)
})

# Table
output$prior_elicit_mu_table = renderTable({
  data.frame(index  = seq(1, as.numeric(input$num_dimensions)),
             mu0 = prior_elicitation_mu_values()$mu0,
             sigma0 = prior_elicitation_mu_values()$sigma0,
             lambda0 = prior_elicitation_mu_values()$lambda0,
             mu = prior_elicitation_mu_values()$mu)
})

# Graph
prior_elicit_mu_graph_item = function(){
  
  alpha01 = prior_elicitation_sigma_values()$alpha01 
  alpha02 = prior_elicitation_sigma_values()$alpha02
  lambda0 = prior_elicitation_mu_values()$lambda0
  mu0 = prior_elicitation_mu_values()$mu0
  
  col = input$prior_elicit_mu_graphnum
  x = -10+20*c(0:1000)/1000
  y = dt(x,2*alpha01[col])
  scale = sqrt(alpha02[col]/alpha01[col])*lambda0[col]
  xnew = mu0[col] + scale*x
  ynew = y/scale
  
  plot(xnew, ynew, lwd = 1, type="l", 
       xlab = TeX(paste("Value of $\\mu_{", col, "}$")),
       ylab = "Density", 
       main = TeX(paste("Prior Density of $\\mu_{", col, "}$")))
}

output$prior_elicit_mu_graph = renderPlot({
  prior_elicit_mu_graph_item()
})

# The plot for it to be saved
output$download_prior_elicit_mu_plot = downloadHandler(
  filename = "Prior Elicit Mu Plot.png",
  content = function(file){
    png(file)
    prior_elicit_mu_graph_item()
    dev.off()
  })



