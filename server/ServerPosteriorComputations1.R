################################################################
# BACKEND FOR POSTERIOR COMPUTATIONS                           #
################################################################

post_integ_seed = reactive(input$post_seed)

choose_file_Y_type = reactive({
  if(input$post_input_type == "csv"){
    read.csv(input$sample_post_Y$datapath, header = TRUE)
  } else if (input$post_input_type == "txt"){
    read.csv(input$sample_post_Y_txt$datapath, sep = "\t")
  } else if (input$post_input_type == "default"){
    test_sample_data()
  }
})

input_Y_values = reactive({
  #as.matrix(test_sample_data())
  tryCatch(
    {
      df = choose_file_Y_type()
      #df = read.csv(input$sample_post_Y$datapath, header = TRUE)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  df = df[,grepl("Y", colnames(df))]
  #df = df %>% select(contains("Y")) # doesn't seem like select works anymore
  as.matrix(df)
  #Y_values = as.matrix(df)
  #Y_values = Y_values %>% select(contains("Y"))
})

post_sample_p_val = reactive({
  # not to be confused with p-values, the evidence against the null.
  if(input$post_comp_use == 1){ # use from the prior elicitation
    input$num_dimensions
  } else if (input$post_comp_use == 2){
    input$num_dimensions_post
  }
})

post_Y_metrics = reactive({
  Y_metrics(Y = input_Y_values(), p = post_sample_p_val())
})

post_sample_values = reactive({
  set.seed(post_integ_seed())
  if(input$post_comp_use == 1){ # default option - use from the prior elicitation
    sample_post_computations(N = input$post_bigN,
                             n = post_Y_metrics()$n,
                             Ybar = post_Y_metrics()$Ybar, 
                             S = post_Y_metrics()$S,
                             p = post_sample_p_val(),
                             mu0 = prior_elicitation_mu_values()$mu0,
                             lambda0 = prior_elicitation_mu_values()$lambda0
    )
  } else if (input$post_comp_use == 2){ # use what is manually inserted
    sample_post_computations(N = input$post_bigN, 
                             n = post_Y_metrics()$n,
                             Ybar = post_Y_metrics()$Ybar, 
                             S = post_Y_metrics()$S,
                             p = post_sample_p_val(),
                             mu0 = create_necessary_vector(input$mu0_post),
                             lambda0 = create_necessary_vector(input$lambda0_post)
    )
  }
})

post_sample_weights = reactive({
  if(input$post_comp_use == 1){ # use from the prior elicitation
    k(N = input$post_bigN, 
      p = post_sample_p_val(), 
      mu = post_sample_values()$mu_xi, 
      xi = post_sample_values()$xi, 
      mu0 = prior_elicitation_mu_values()$mu0,
      lambda0 = prior_elicitation_mu_values()$lambda0,
      alpha01 = prior_elicitation_sigma_values()$alpha01,
      alpha02 = prior_elicitation_sigma_values()$alpha02)
  } else if (input$post_comp_use == 2){
    k(N = input$post_bigN, 
      p = post_sample_p_val(), 
      mu = post_sample_values()$mu_xi, 
      xi = post_sample_values()$xi, 
      mu0 = create_necessary_vector(input$mu0_post),
      lambda0 = create_necessary_vector(input$lambda0_post),
      alpha01 = prior_elicitation_sigma_values()$alpha01,
      alpha02 = prior_elicitation_sigma_values()$alpha02)
  }
})

#0utput$testing_post = renderPrint({
#})

sample_post_values_reformatted = reactive({
  sample_post_reformat(N = input$post_bigN, 
                       p = post_sample_p_val(), 
                       post_mu = post_sample_values()$mu_xi,
                       post_xi = post_sample_values()$xi, 
                       weights = post_sample_weights()$weights_vector)
})

sample_post_values_reformatted_round = eventReactive(input$submit_sample_post, {
  round(sample_post_values_reformatted(), 10)
})
