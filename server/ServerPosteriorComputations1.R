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

important_sample_values = reactive({
  set.seed(post_integ_seed())
  if(input$post_comp_use == 1){ # default option - use from the prior elicitation
    importance_sampler_computations(Npostimp = as.numeric(input$post_bigN), 
                                    n = nrow(input_Y_values()),
                                    Ybar = post_Y_metrics()$Ybar, 
                                    S = post_Y_metrics()$S,
                                    p = post_sample_p_val(), 
                                    mu0 = prior_elicitation_mu_values()$mu0,
                                    lambda0 = prior_elicitation_mu_values()$lambda0,
                                    alpha01 = prior_elicitation_sigma_values()$alpha01,
                                    alpha02 = prior_elicitation_sigma_values()$alpha02
    )
  } else if (input$post_comp_use == 2){ # use what is manually inserted
    importance_sampler_computations(Npostimp = as.numeric(input$post_bigN), 
                                    n = nrow(input_Y_values()),
                                    Ybar = post_Y_metrics()$Ybar, 
                                    S = post_Y_metrics()$S,
                                    p = post_sample_p_val(), 
                                    mu0 = create_necessary_vector(input$mu0_post),
                                    lambda0 = create_necessary_vector(input$lambda0_post),
                                    alpha01 = create_necessary_vector(input$alpha01_post),
                                    alpha02 = create_necessary_vector(input$alpha02_post)
    )
  }
})

important_values_reformatted = reactive({
  important_post_reformat(N = input$post_bigN, 
                       p = post_sample_p_val(), 
                       post_mu = important_sample_values()$mu_xi,
                       post_Sigma = important_sample_values()$Sigma, 
                       post_xi = important_sample_values()$xi,
                       weights = important_sample_values()$weights_vector)
})

important_values_reformatted_round = eventReactive(input$submit_imp_sampler, {
  round(important_values_reformatted(), 10)
})

## adding new things (lots of things will need to be revised here...)

post_SIR_calculations = reactive({
  # the user may need to input
  SIR_algorithm(Npostsamp = input$post_sample_N, 
                cum_weights = important_sample_values()$cum_weights, 
                p = post_sample_p_val(), 
                mu_xi = important_sample_values()$mu_xi, 
                xi = important_sample_values()$xi, 
                Sigma = important_sample_values()$Sigma)
})

post_SIR_calculations_reformat = eventReactive(input$submit_sample_post, {
  SIR_sample_reformat(Npostsamp = input$post_sample_N, 
                      p = post_sample_p_val(), 
                      mu_matrix = post_SIR_calculations()$sample_mu_xi, 
                      Sigma_matrices = post_SIR_calculations()$sample_Sigma,
                      xi_matrices = post_SIR_calculations()$sample_xi)
})

output$SIR_algorithm_output = renderPrint(
  post_SIR_calculations_reformat()
)


#####################################
# relative belief ratio

prior_psi_val1 = reactive({
  prior_psi(Nprior = input$prior_sample_bigN, 
            mu_prior = sample_prior_values()$mu_matrix, 
            Sigma_prior = sample_prior_values()$Sigma_mat[], 
            xi_prior = sample_prior_values()$xi_mat,
            col_num = as.numeric(input$plot_compare_col_num))
})

post_psi_val1 = reactive({
  post_psi(Npostimp = input$post_bigN, 
           numcells = input$rbr_numcells,
           imp_mu = important_sample_values()$mu_xi, 
           imp_Sigma = important_sample_values()$Sigma, 
           imp_xi = important_sample_values()$xi, 
           col_num = as.numeric(input$plot_compare_col_num))
})

post_psi_val2 = reactive({
  post_psi_additional(imp_psi_vals = post_psi_val1(), 
                      imp_weights = important_sample_values()$weights_vector,
                      Npostimp = input$post_bigN, 
                      numcells = input$rbr_numcells,
                      breaks = prior_psi_values()$psi_breaks)
})

prior_psi_values = reactive({
  prior_psi_plot_vals(numcells = input$rbr_numcells, 
                      Nprior = input$prior_sample_bigN, 
                      mprior = input$rbr_mprior,
                      prior_psi_vals = prior_psi_val1())
})

post_psi_values = reactive({
  post_psi_plot_vals(Npostimp = input$post_bigN, # may need to make user input 
                     numcells = input$rbr_numcells,
                     mpost = input$rbr_mpost, 
                     post_psi_cdf = post_psi_val2()$post_psi_cdf, 
                     delta_psi = prior_psi_values()$delta_psi)
})

rbr_psi_values = reactive({
  rbr_psi(numcells = input$rbr_numcells, 
          prior_psi_dens_smoothed = prior_psi_values()$prior_psi_dens_smoothed, 
          post_psi_dens_smoothed = post_psi_values())
})

################################################################
# UPLOADING NEW DATA!                                          #
################################################################

prior_psi_vals_uploaded = reactive({
  tryCatch(
    {
      df = read.csv(input$upload_prior_psi_vals$datapath, header = TRUE)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  df$x
})

imp_psi_vals_uploaded = reactive({
  tryCatch(
    {
      df = read.csv(input$upload_imp_psi_vals$datapath, header = TRUE)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  df$x
})

# constructing the alternative values now

prior_psi_values_alt = reactive({
  prior_psi_plot_vals(numcells = input$rbr_numcells, 
                      Nprior = input$prior_sample_bigN, 
                      mprior = input$rbr_mprior,
                      prior_psi_vals = prior_psi_vals_uploaded())
})

post_psi_val2_alt = reactive({
  post_psi_additional(imp_psi_vals = imp_psi_vals_uploaded(), 
                      imp_weights = important_sample_values()$weights_vector,
                      Npostimp = input$post_bigN, 
                      numcells = input$rbr_numcells,
                      breaks = prior_psi_values()$psi_breaks)
})

post_psi_values_alt = reactive({
  post_psi_plot_vals(Npostimp = input$post_bigN, # may need to make user input 
                     numcells = input$rbr_numcells,
                     mpost = input$rbr_mpost, 
                     post_psi_cdf = post_psi_val2_alt()$post_psi_cdf, 
                     delta_psi = prior_psi_values_alt()$delta_psi)
})

rbr_psi_values_alt = reactive({
  rbr_psi(numcells = input$rbr_numcells, 
          prior_psi_dens_smoothed = prior_psi_values_alt()$prior_psi_dens_smoothed, 
          post_psi_dens_smoothed = post_psi_values_alt())
})

# end of plot; inferences are shown below

RBest_value = reactive({
  if(input$psi_value_type == 1){
    prior_psi_values()$prior_psi_mids[which.max(rbr_psi_values())]
  } else {
    prior_psi_values_alt()$prior_psi_mids[which.max(rbr_psi_values_alt())]
  }
})

plausible_region_estimation = reactive({
  if(input$psi_value_type == 1){
    plausible_region_est(prior_psi_mids = prior_psi_values()$prior_psi_mids, 
                        RB_psi = rbr_psi_values(), 
                        post_psi_dens_smoothed = post_psi_values(),
                        delta_psi = prior_psi_values()$delta_psi)
  } else {
    plausible_region_est(prior_psi_mids = prior_psi_values_alt()$prior_psi_mids, 
                         RB_psi = rbr_psi_values_alt(), 
                         post_psi_dens_smoothed = post_psi_values_alt(),
                         delta_psi = prior_psi_values_alt()$delta_psi)
  }
})

psi_hypothesis_testing = reactive({
  if(input$psi_value_type == 1){
    psi_hypothesis_test(psi_0 = input$psi0, 
                      prior_psi_mids = prior_psi_values()$prior_psi_mids, 
                      RB_psi = rbr_psi_values(), 
                      post_psi_dens_smoothed = post_psi_values(),
                      delta_psi = prior_psi_values()$delta_psi)
  } else {
    psi_hypothesis_test(psi_0 = input$psi0, 
                        prior_psi_mids = prior_psi_values_alt()$prior_psi_mids, 
                        RB_psi = rbr_psi_values_alt(), 
                        post_psi_dens_smoothed = post_psi_values_alt(),
                        delta_psi = prior_psi_values_alt()$delta_psi)
  }
})

output$psi_hypo_test_output = renderPrint({
  list("Estimate of true value of psi from the relative belief ratio" = RBest_value(),
       "Plausible region" = plausible_region_estimation()$plaus_interval,
       "Posterior content of the plausible regiion" = plausible_region_estimation()$plaus_content,
       "The evidence concerning strength H_0 : psi = psi_0" = psi_hypothesis_testing()$psi_message,
       "The strength" = psi_hypothesis_testing()$strength_message)
})

