################################################################
# BACKEND FOR PRIOR ELICITATION                                #
################################################################

const_list = reactive({create_necessary_vector(input$const_s)})
m1_list = reactive({create_necessary_vector(input$m1)})
m2_list = reactive({create_necessary_vector(input$m2)})

alpha01_list = reactive({create_necessary_vector(input$alpha01)})
alpha02_list = reactive({create_necessary_vector(input$alpha02)})
alpha01_list_ver2 = reactive({create_necessary_vector(input$alpha01_ver2)})
alpha02_list_ver2 = reactive({create_necessary_vector(input$alpha02_ver2)})

prior_elicitation_values = reactive({
  prior_elicitation_mu(gamma = input$virtual_uncertainty, 
                       m1 = m1_list(), 
                       m2 = m2_list(), 
                       const = const_list(), 
                       s1 = FALSE, s2 = FALSE)
})

sigma_prior_elicitation_values = reactive({
  elicit_sigma(gamma = input$virtual_uncertainty_sigma, 
               s1 = input$elicit_sigma_s1, 
               s2 = input$elicit_sigma_s2, 
               alphaup = input$alphaup_sigma, 
               alphalow = input$alphalow_sigma)
})

output$sigma_elicit_prior_calculation = renderPrint({
  # Note: this is separate such that it can be re-used in the other algorithm.
  sigma_prior_elicitation_values()
})

output$elicit_prior_computation = renderPrint({
  # Note: this is separate such that it can be re-used in the other algorithm.
  prior_elicitation_values()
})

sigma_elicit_prior_graphs = reactive({
  alpha = sigma_prior_elicitation_values()$alpha
  beta = sigma_prior_elicitation_values()$beta
  low = sigma_prior_elicitation_values()$low
  up = sigma_prior_elicitation_values()$up
  z0 = sigma_prior_elicitation_values()$z0
  x = low+(up-low)*c(0:1000)/1000
  
  if(input$elicit_sigma_graph_type == 1){
    x3 = sqrt(1/x)
    dens3 = 2*(x^(3/2))*dgamma(x,alpha,beta)
    plot(x3,dens3,
         main = "Eliciting for Sigma | Prior Density of Sigma",
         xlab="Sigma",
         ylab="Prior Density",
         type="l")
    
  } else if (input$elicit_sigma_graph_type == 2){
    x3 = z0*sqrt(1/x)
    dens3 = (2/z0)*(x^(3/2))*dgamma(x,alpha,beta)
    plot(x3,dens3,
         main = "Eliciting for Sigma | Prior Density of Sigma * z",
         xlab = "Sigma * z",
         ylab = "Prior Sensity",
         type = "l")
  }
})

output$sigma_elicit_prior_graph = renderPlot({
  # clean up the rest later - this is a placeholder. Will turn it into a function to
  # make it easier to read in the future.
  sigma_elicit_prior_graphs()
})
