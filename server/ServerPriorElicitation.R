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
  elicit_prior(gamma = input$virtual_uncertainty,
               m1 = create_necessary_vector(input$m1),
               m2 = create_necessary_vector(input$m2),
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
  alpha = prior_elicitation_values()$alpha01
  beta = prior_elicitation_values()$alpha02
  low = prior_elicitation_values()$c1
  up = prior_elicitation_values()$c2
  z0 = prior_elicitation_values()$z0
  x = low+(up-low)*c(0:1000)/1000
  
  if(input$elicit_sigma_graph_type == 1){
    x3 = sqrt(1/x)
    dens3 = 2*(x^(3/2))*dgamma(x,alpha,beta)
    plot(x3,dens3,
         main = TeX(paste("Prior Density of ", r'($\sigma$)')),
         xlab=TeX(r'($\sigma$)'),
         ylab="Prior Density",
         type="l")
    
  } else if (input$elicit_sigma_graph_type == 2){
    x3 = z0*sqrt(1/x)
    dens3 = (2/z0)*(x^(3/2))*dgamma(x,alpha,beta)
    plot(x3,dens3,
         main = TeX(paste("Prior Density of ", r'($\sigma$ \cdot z)')),
         xlab = TeX(r'($\sigma$ \cdot z)'),
         ylab = "Prior Sensity",
         type = "l")
  }
})

output$elicit_prior_graph = renderPlot({
  # clean up the rest later - this is a placeholder. Will turn it into a function to
  # make it easier to read in the future.
  elicit_prior_graphs()
})
