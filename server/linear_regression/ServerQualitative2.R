################################################################
# QUALITATIVE FACTORS: BACKEND FOR PRIOR & POSTERIOR SAMPLING  #
################################################################

qual_prior_sampling_seed = reactive(input$qual_prior_seed)

qual_post_sampling_seed = reactive(input$qual_post_seed)

qual_sample_prior_values = eventReactive(input$qual_submit_prior_sampling, {
  set.seed(qual_prior_sampling_seed())
  qual_sample_prior(Nprior = input$qual_prior_sample_bigN, 
                    k = prod(create_necessary_vector(input$qual_num_levels)), 
                    alpha01 = qual_prior_sigma()$alpha01, 
                    alpha02 = qual_prior_sigma()$alpha02, 
                    beta0 = qual_prior_elicit_mu_manual()$beta0)
})

qual_sample_post_values = eventReactive(input$qual_submit_post_sampling, {
  set.seed(qual_post_sampling_seed())
  qual_sample_post(Npost = input$qual_post_sample_bigN, 
                   k = prod(create_necessary_vector(input$qual_num_levels)), 
                   n = sum(create_necessary_vector(input$qual_num_n)), 
                   alpha01 = qual_prior_sigma()$alpha01, 
                   alpha02 = qual_prior_sigma()$alpha02,  
                   lambda0 = qual_prior_elicit_mu_manual()$lambda0, 
                   beta0 = qual_prior_elicit_mu_manual()$beta0, 
                   b = qual_sufficient_stat_comp()$b, 
                   s_2 = qual_sufficient_stat_comp()$s_2)
})

# Table
output$qual_prior_sample_table = renderTable({
  data.frame("lol" = x)
  #data.frame("sigma^2" = qual_sample_prior_values())
})
