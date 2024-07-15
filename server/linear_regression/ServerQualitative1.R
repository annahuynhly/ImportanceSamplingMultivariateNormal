################################################################
# QUALITATIVE FACTORS: BACKEND FOR PRIOR ELICITATION           #
################################################################

# Part 0: Data Input and Sufficient Statistics Computations #

qual_choose_file_Y_type = reactive({
  if(input$qual_input_type == "csv"){
    read.csv(input$qual_Y_input_csv$datapath, header = TRUE)$Y
  } else if (input$post_input_type == "txt"){
    read.csv(input$qual_Y_input_txt$datapath, sep = "\t")
  } else if (input$post_input_type == "default"){
    sample_Y_qual_data()
  }
})

output$beta_order_output = renderPrint({
  create_beta_list_names(levels = create_necessary_vector(input$qual_num_levels))
})

# getting the sufficient statistic (manual)
qual_sufficient_stat_comp_manual = reactive({
  
  l = create_necessary_vector(input$qual_num_levels) # input the number of levels per factor
  m = length(l) # the number of factors.
  k = prod(l) # letting this denote the possible number of combinations between the crossed factors 
  
  # the size of n_vector should be equal to k (if factors are crossed)
  n_vector = create_necessary_vector(input$qual_num_n)
  n = sum(n_vector)
  
  X = qual_generate_X(n_vector)
  results = qual_Y_metrics(X, qual_choose_file_Y_type(), m, l)
  
  newlist = list("X" = X, "b" = results$b, "s_2" = results$s_2, "C" = results$C)
  return(newlist)
})

qual_sufficient_stat_comp_file_txt = reactive({
  tryCatch(
    {
      df = read.csv(input$qual_initial_input_file_txt$datapath, 
                    header = TRUE,
                    sep = "\t")
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  df
})

qual_sufficient_stat_comp_file_csv = reactive({
  tryCatch(
    {
      df = read.csv(input$qual_initial_input_file_csv$datapath, header = TRUE)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  df
})

# the input if the user uploads a file
# need to see if it's the same for .csv and .txt
qual_sufficient_stat_comp_file = reactive({
  
  if(input$qual_initial_input_type == "csv"){
    data = qual_sufficient_stat_comp_file_csv() 
  } else if (input$qual_initial_input_type == "txt"){
    data = qual_sufficient_stat_comp_file_txt() 
  }
  
  l = data$li[!is.na(data$li)] # input the number of levels per factor
  m = data$m[!is.na(data$m)] # the number of factors.
  k = prod(l) # letting this denote the possible number of combinations between the crossed factors 
  
  # the size of n_vector should be equal to k (if factors are crossed)
  n_vector = data$n[!is.na(data$n)]
  n = sum(n_vector)
  
  X = qual_generate_X(n_vector)
  results = qual_Y_metrics(X, qual_choose_file_Y_type(), m, l)
  
  newlist = list("X" = X, "b" = results$b, "s_2" = results$s_2, "C" = results$C)
  return(newlist)
})

qual_sufficient_stat_comp = reactive({
  if(input$qual_initial_input_type == "manual"){
    qual_sufficient_stat_comp_manual()
  } else if (input$qual_initial_input_type != "manual"){
    qual_sufficient_stat_comp_file()
  }
})

output$qual_X_output = renderPrint({
  qual_sufficient_stat_comp()$X
})

output$qual_sufficient_statistics1 = renderPrint({
  qual_sufficient_stat_comp()[c("b", "s_2")]
})

output$qual_sufficient_statistics2 = renderPrint({
  qual_sufficient_stat_comp()$C
})

# Part 1.1: Elicitation of the Prior (Sigma) #

qual_elicit_sigma_txt = reactive({
  tryCatch(
    {
      df = read.csv(input$qual_elicit_sigma_inputs_txt$datapath, 
                    header = TRUE,
                    sep = "\t")
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  df
})

qual_elicit_sigma_csv = reactive({
  tryCatch(
    {
      df = read.csv(input$qual_elicit_sigma_inputs_csv$datapath, header = TRUE)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  df
})

qual_s1 = reactive({
  if(input$qual_elicit_sigma_input_type == 'manual'){
    input$qual_elicit_s1
  } else if (input$qual_elicit_sigma_input_type == 'txt'){
    data = qual_elicit_sigma_txt()
    data$s1[!is.na(data$s1)]
  } else if (input$qual_elicit_sigma_input_type == 'csv'){
    data = qual_elicit_sigma_csv()
    data$s1[!is.na(data$s1)]
  }
})

qual_s2 = reactive({
  if(input$qual_elicit_sigma_input_type == 'manual'){
    input$qual_elicit_s2
  } else if (input$qual_elicit_sigma_input_type == 'txt'){
    data = qual_elicit_sigma_txt()
    data$s2[!is.na(data$s2)]
  } else if (input$qual_elicit_sigma_input_type == 'csv'){
    data = qual_elicit_sigma_csv()
    data$s2[!is.na(data$s2)]
  }
})

qual_upper_bd = reactive({
  if(input$qual_elicit_sigma_input_type == 'manual'){
    input$qual_alphaup
  } else if (input$qual_elicit_sigma_input_type == 'txt'){
    data = qual_elicit_sigma_txt()
    data$upper_bd[!is.na(data$upper_bd)]
  } else if (input$qual_elicit_sigma_input_type == 'csv'){
    data = qual_elicit_sigma_csv()
    data$upper_bd[!is.na(data$upper_bd)]
  }
})

qual_lower_bd = reactive({
  if(input$qual_elicit_sigma_input_type == 'manual'){
    input$qual_alphalow
  } else if (input$qual_elicit_sigma_input_type == 'txt'){
    data = qual_elicit_sigma_txt()
    data$lower_bd[!is.na(data$lower_bd)]
  } else if (input$qual_elicit_sigma_input_type == 'csv'){
    data = qual_elicit_sigma_csv()
    data$lower_bd[!is.na(data$lower_bd)]
  }
})

qual_prior_sigma = eventReactive(input$qual_submit_prior_elicit_sigma, {
  elicit_prior_sigma_function(p = 1, 
                              gamma = input$qual_virtual_uncertainty, 
                              s1 = qual_s1(), 
                              s2 = qual_s2(), 
                              upper_bd = qual_upper_bd(), 
                              lower_bd = qual_lower_bd())
})

# Table
output$qual_prior_elicit_sigma_table = renderTable({
  data.frame(alpha01 = qual_prior_sigma()$alpha01,
             alpha02 = qual_prior_sigma()$alpha02)
})

# Graph of sigma
qual_elicit_sigma_graph = function(){
  alpha = qual_prior_sigma()$alpha01
  beta = qual_prior_sigma()$alpha02
  low = qual_prior_sigma()$lwbdinvsigma2
  up = qual_prior_sigma()$upbdinvsigma2
  z0 = qual_prior_sigma()$z0
  x = low+(up-low)*c(0:1000)/1000
  prob_z = round(pnorm(z0), 4)
  
  if(input$qual_elicit_sigma_graph_type == 1){
    x3 = sqrt(1/x)
    dens3 = 2*(x^(3/2))*dgamma(x,alpha,beta)
    plot(x3,dens3,
         main = TeX(paste("Prior Density of $\\sigma$")),
         xlab = TeX(paste("Value of $\\sigma$")),
         ylab = "Prior Density",
         type = "l")
  } else if (input$qual_elicit_sigma_graph_type == 2){
    x3 = z0*sqrt(1/x)
    dens3 = (2/z0)*(x^(3/2))*dgamma(x, alpha, beta)
    plot(x3, dens3,
         main = TeX(paste("Prior Density of $\\sigma \\cdot z_{0}$")),
         xlab = TeX(paste("$\\sigma \\cdot z_{0}$")),
         ylab = "Prior Density",
         type = "l")
  }
}

output$qual_elicit_prior_sigma = renderPlot({
  #qual_prior_sigma()
  qual_elicit_sigma_graph()
})

# The plot for it to be saved
output$qual_download_prior_elicit_sigma = downloadHandler(
  filename = "(Qualitative) Prior Elicit Sigma Plot.png",
  content = function(file){
    png(file, width = 1140, height = 770, units = "px", res = 150)
    qual_elicit_sigma_graph()
    dev.off()
  })

# Part 1.2: Elicitation of the Prior (beta_{ji}) #

qual_elicit_mu_txt = reactive({
  tryCatch(
    {
      df = read.csv(input$qual_prior_mu_file_txt$datapath, 
                    header = TRUE,
                    sep = "\t")
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  df
})

qual_elicit_mu_csv = reactive({
  tryCatch(
    {
      df = read.csv(input$qual_prior_mu_file_csv$datapath, header = TRUE)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  df
})

qual_m1 = reactive({
  if(input$qual_prior_mu_input_type == 'manual'){
    create_necessary_vector(input$qual_m1)
  } else if (input$qual_prior_mu_input_type == 'txt'){
    data = qual_elicit_mu_txt()
    data$m1[!is.na(data$m1)]
  } else if (input$qual_prior_mu_input_type == 'csv'){
    data = qual_elicit_mu_csv()
    data$m1[!is.na(data$m1)]
  }
})

qual_m2 = reactive({
  if(input$qual_prior_mu_input_type == 'manual'){
    create_necessary_vector(input$qual_m2)
  } else if (input$qual_prior_mu_input_type == 'txt'){
    data = qual_elicit_mu_txt()
    data$m2[!is.na(data$m2)]
  } else if (input$qual_prior_mu_input_type == 'csv'){
    data = qual_elicit_mu_csv()
    data$m2[!is.na(data$m2)]
  }
})

qual_prior_elicit_mu = eventReactive(input$qual_submit_prior_elicit_mu, {
  elicit_prior_beta0_function(p = 1, 
                              gamma = input$qual_virtual_uncertainty,
                              m1 = qual_m1(), 
                              m2 = qual_m2(), 
                              s1 = qual_s1(), 
                              s2 = qual_s2(), 
                              alpha01 = qual_prior_sigma()$alpha01, 
                              alpha02 = qual_prior_sigma()$alpha02)
})

output$qual_debugging = renderPrint({
  qual_prior_elicit_mu()
})

# Table
output$qual_prior_elicit_mu_table = renderTable({
  k = prod(create_necessary_vector(input$qual_num_levels))
  data.frame(index  = seq(1, k),
             beta0 = qual_prior_elicit_mu()$beta0,
             lambda0 = qual_prior_elicit_mu()$lambda0)
})

# Graph
qual_prior_elicit_mu_graph_item = function(){
  
  alpha01 = qual_prior_sigma()$alpha01 
  alpha02 = qual_prior_sigma()$alpha02
  lambda0 = qual_prior_elicit_mu()$lambda0
  beta0 = qual_prior_elicit_mu()$beta0
  
  col = input$qual_prior_elicit_mu_graphnum
  x = -10+20*c(0:1000)/1000
  y = dt(x,2*alpha01)
  scale = sqrt(alpha02/alpha01)*lambda0
  xnew = beta0[col] + scale*x
  ynew = y/scale
  
  plot(xnew, ynew, lwd = 1, type="l", 
       xlab = TeX(paste("Value of $\\beta_{", col, "}$ (TODO: EDIT)")),
       ylab = "Prior Density", 
       main = TeX(paste("Prior Density of $\\beta_{", col, "}$")))
}

output$qual_prior_elicit_mu_graph = renderPlot({
  # will replace since latex doesnt render properly
  qual_prior_elicit_mu_graph_item() 
})

# The plot for it to be saved
output$download_prior_elicit_mu_plot = downloadHandler(
  filename = "(Qualitative) Prior Elicit Beta Plot.png",
  content = function(file){
    png(file, width = 1140, height = 770, units = "px", res = 150)
    qual_prior_elicit_mu_graph_item() 
    dev.off()
  })


########### below is generic code for debugging

output$qual_debug = renderPrint({
  qual_elicit_sigma_txt()
})


