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