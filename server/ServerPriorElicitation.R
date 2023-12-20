################################################################
# BACKEND FOR PRIOR ELICITATION                                #
################################################################

const_list = reactive({create_necessary_vector(input$const_s)})
m1_list = reactive({create_necessary_vector(input$m1)})
m2_list = reactive({create_necessary_vector(input$m2)})

prior_elicitation_values = eventReactive(input$submit_prior_elicit, {
  elicit_prior(gamma = input$virtual_uncertainty,
               m1 = m1_list(),
               m2 = m2_list(),
               s1 = create_necessary_vector(input$elicit_s1), 
               s2 = create_necessary_vector(input$elicit_s2), 
               alphaup = create_necessary_vector(input$alphaup), 
               alphalow = create_necessary_vector(input$alphalow))
})

output$elicit_prior_calculation = renderPrint({
  # Note: this is separate such that it can be re-used in the other algorithm.
  prior_elicitation_values()
})

elicit_prior_graphs = reactive({
  graph_num = input$prior_elicit_graphnum
  alpha = prior_elicitation_values()$alpha01[graph_num]
  beta = prior_elicitation_values()$alpha02[graph_num]
  low = prior_elicitation_values()$c1[graph_num]
  up = prior_elicitation_values()$c2[graph_num]
  z0 = prior_elicitation_values()$z0
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
                          '}\\cdot z_{t}, t = \\frac{1+\\gamma}{2}$')),
         xlab = TeX(paste('$\\sigma_{', graph_num, 
                          '} \\cdot z_{t}, t = \\frac{1+\\gamma}{2}$')),
         #xlab = TeX(paste(r'($\sigma$)', graph_num, r'(\cdot z_{\frac{1}{\gamma + 2}})')),
         ylab = "Prior Density",
         type = "l")
  }
})

output$elicit_prior_graph = renderPlot({
  # clean up the rest later - this is a placeholder. Will turn it into a function to
  # make it easier to read in the future.
  elicit_prior_graphs()
})

# for downloadng the graph: note that R shiny download handler doesn't work well for
# reactive elements

elicit_prior_graphs_download = function(){
  graph_num = input$prior_elicit_graphnum
  alpha = prior_elicitation_values()$alpha01[graph_num]
  beta = prior_elicitation_values()$alpha02[graph_num]
  low = prior_elicitation_values()$c1[graph_num]
  up = prior_elicitation_values()$c2[graph_num]
  z0 = prior_elicitation_values()$z0
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
                          '}\\cdot z_{t}, t = \\frac{1+\\gamma}{2}$')),
         xlab = TeX(paste('$\\sigma_{', graph_num, 
                          '} \\cdot z_{t}, t = \\frac{1+\\gamma}{2}$')),
         #xlab = TeX(paste(r'($\sigma$)', graph_num, r'(\cdot z_{\frac{1}{\gamma + 2}})')),
         ylab = "Prior Density",
         type = "l")
  }
}

output$download_prior_elicit_sigma = downloadHandler(
  filename = "Prior Elicit Sigma Plot.png",
  content = function(file){
    png(file)
    elicit_prior_graphs_download()
    dev.off()
})
