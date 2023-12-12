################################################################
# BACKEND FOR SAMPLING                                         #
################################################################

alpha01_prior = reactive({create_necessary_vector(input$alpha01_prior)})
alpha02_prior = reactive({create_necessary_vector(input$alpha02_prior)})
alpha01_post = reactive({create_necessary_vector(input$alpha01_post)})
alpha02_post = reactive({create_necessary_vector(input$alpha02_post)})

m1_prior = reactive({create_necessary_vector(input$m1_prior)})
m2_prior = reactive({create_necessary_vector(input$m2_prior)})
m1_post = reactive({create_necessary_vector(input$m1_post)})
m2_post = reactive({create_necessary_vector(input$m2_post)})

# creating test sample data (changes every time for fun!)
test_sample_data = reactive({
  p = 3#5
  mu = rep(0, p) 
  sigma = diag(p) 
  n = 100
  Y = mvrnorm(n = n, mu = mu, Sigma = sigma)
  data = as.data.frame(Y)
  colnames(data) = c("Y1", "Y2", "Y3") #c("Y1", "Y2", "Y3", "Y4", "Y5")
  data
})

# test data for uploading Y_{1i}s
output$sample_post_example_file = downloadHandler(
  filename = "Y_example.csv",
  content = function(file) {
    write.csv(test_sample_data(), file, row.names = FALSE)
  }
)

# PRIOR CASE (MODIFIED) ########################################

prior_sample_values_NEW = eventReactive(input$submit_sample_prior_NEW, {
  if(input$priorsample_use_NEW == "y"){
    hyperpara = sample_hyperparameters(
      gamma = input$virtual_uncertainty, 
      alpha01 = prior_elicitation_values()$alpha01, 
      alpha02 = prior_elicitation_values()$alpha02, 
      m1 = m1_list(),
      m2 = m2_list()
    )
    samples = sample_prior_new(N = input$prior_bigN_NEW, 
                               alpha01 = prior_elicitation_values()$alpha01, 
                               alpha02 = prior_elicitation_values()$alpha02, 
                               mu0 = hyperpara$mu0, 
                               lambda0 = hyperpara$lambda0)
  } else if (input$priorsample_use_NEW == "n"){
    hyperpara = sample_hyperparameters(
      gamma = input$virtual_uncertainty_prior, 
      alpha01 = alpha01_prior(), 
      alpha02 = alpha02_prior(), 
      m1 = m1_prior(),
      m2 = m2_prior()
    )
    # note: may want to make the code more efficient later.
    samples = sample_prior_new(N = input$prior_bigN_NEW, 
                               alpha01 = alpha01_prior(), 
                               alpha02 = alpha02_prior(), 
                               mu0 = hyperpara$mu0, 
                               lambda0 = hyperpara$lambda0)
  }
  samples
})

output$sample_prior_computation_NEW = renderPrint({
  prior_sample_values_NEW()
})


output$sample_prior_computations_graph_NEW = renderPlot({
  if(input$priorsample_use_NEW == "n"){
    hyperpara = sample_hyperparameters(
      gamma = input$virtual_uncertainty_prior, 
      alpha01 = alpha01_prior(),
      alpha02 = alpha02_prior(),
      m1 = m1_prior(),
      m2 = m2_prior()
    )
    alpha01 = alpha01_prior()
    alpha02 = alpha02_prior()
  } else if (input$priorsample_use_NEW == "y"){
    hyperpara = sample_hyperparameters(
      gamma = input$virtual_uncertainty, 
      alpha01 = prior_elicitation_values()$alpha01, 
      alpha02 = prior_elicitation_values()$alpha02, 
      m1 = m1_list(),
      m2 = m2_list()
    )
    alpha01 = prior_elicitation_values()$alpha01
    alpha02 = prior_elicitation_values()$alpha02 
  }
  mu0 = hyperpara$mu0
  lambda0 = hyperpara$lambda0
  
  col = input$prior_graph_num
  x = -10+20*c(0:1000)/1000
  y = dt(x,2*alpha01[col])
  scale = sqrt(alpha02[col]/alpha01[col])*lambda0[col]
  xnew = mu0[col] + scale*x
  ynew = y/scale
  
  if(input$prior_graph_hist == "y"){
    hist(prior_sample_values_NEW(), prob = TRUE,
         xlab = "Values of Mu", ylab = "Density", 
         main = "Density plot of Mu",
         border = "#ffffff")
    lines(xnew, ynew, lwd = 2, lty = 2)
  } else {
    plot(xnew, ynew, lwd = 2, type="l", 
          xlab = "Values of Mu", ylab = "Density", 
          main = "Density plot of Mu")
  }
  
})


# PRIOR CASE ###################################################
# may delete; this doesn't seem to work

prior_sample_values = eventReactive(input$submit_sample_prior, {
  if(input$priorsample_use == "y"){
    sample_multiple_prior(
      n = input$prior_bigN,
      alpha01 = prior_elicitation_values()$alpha01, 
      alpha02 = prior_elicitation_values()$alpha02, 
      mu_0 = prior_elicitation_values()$mu0, 
      sigma_0 = prior_elicitation_values()$sigma0
    )
  } else if (input$priorsample_use == "n"){
    sample_multiple_prior(
      n = input$prior_bigN,
      alpha01 = alpha01_list(), 
      alpha02 = alpha02_list(), 
      mu_0 = input$mu_0, 
      sigma_0 = input$sigma_0
    )
  }
})

prior_sample_computation_summary = eventReactive(input$submit_sample_prior, {
  list(
    "mu" = head(prior_sample_values()$mu, 10),
    "sigma" = prior_sample_values()$sigma[[1]],
    "R" = prior_sample_values()$R[[1]]
  )
})

output$sample_prior_computation = renderPrint({
  prior_sample_computation_summary()
})

# CODE FOR DOWNLOADING PRIOR CASE ##############################
# also may not work at this stage.

download_prior_sample_mu = reactive({
  data = as.data.frame(prior_sample_values()$mu)
  for(i in 1:ncol(data)){
    colnames(data)[i] = paste("mu", i, sep = " ")
  }
  data
})

download_prior_sample_sigma = reactive({
  data = as.data.frame(prior_sample_values()$sigma)
  for(i in 1:ncol(data)){
    colnames(data)[i] = paste("sigma", i, sep = " ")
  }
  data
})

download_prior_sample_R = reactive({
  data = as.data.frame(prior_sample_values()$R)
  n_matrices = length(data)
  for(i in 1:n_matrices){
    data[[i]] = as.data.frame(data[[i]]) # turning into data frame to name the columns
    n_cols = ncol(data[[1]]) # assuming there exists a matrix 
    for(j in 1:n_cols){
      colnames(data[[i]])[j] = paste("Matrix", i, "column", j, sep = " ")
    }
  }
  data
})

# actually downloading the data.
output$priorsample_download_mu = downloadHandler(
  filename = "priorsample_mu.csv",
  content = function(file) {
    write.csv(download_prior_sample_mu(), file, row.names = FALSE)
  }
)

output$priorsample_download_sigma = downloadHandler(
  filename = "priorsample_sigma.csv",
  content = function(file) {
    write.csv(download_prior_sample_sigma(), file, row.names = FALSE)
  }
)

output$priorsample_download_R = downloadHandler(
  filename = "priorsample_R.csv",
  content = function(file) {
    write.csv(download_prior_sample_R(), file, row.names = FALSE)
  }
)

# PRIOR CASE ###################################################

input_Y_values = reactive({
  #as.matrix(test_sample_data())
  tryCatch(
    {
      df = read.csv(input$sample_post_Y$datapath, header = TRUE)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  as.matrix(df)
})

post_sample_values = reactive({
  if(input$postsample_use == 1){ # input values
    sample_post_new(N = input$post_bigN, 
                    Y = input_Y_values(), 
                    gamma = input$virtual_uncertainty_post, 
                    alpha01 = alpha01_post(), 
                    alpha02 = alpha02_post(), 
                    m1 = m1_post(), 
                    m2 = m2_post()
    )
  } else if (input$postsample_use == 2){ # same as elicit
    sample_post_new(N = input$post_bigN, 
                    Y = input_Y_values(), 
                    gamma = input$virtual_uncertainty_post, 
                    alpha01 = prior_elicitation_values()$alpha01, 
                    alpha02 = prior_elicitation_values()$alpha02,
                    m1 = m1_list(), 
                    m2 = m2_list()
    )
  } else if (input$postsample_use == 3){ # same as prior
    sample_post_new(N = input$post_bigN, 
                    Y = input_Y_values(), 
                    gamma = input$virtual_uncertainty_post, 
                    alpha01 = alpha01_prior(),
                    alpha02 = alpha02_prior(),
                    m1 = m1_prior(), 
                    m2 = m2_prior()
    )
  }
})

# previous location for sample post computations.

# plotting for the relative belief ratio
rbr_sample_values = reactive({
  sample_rbr_mu(prior_mu = prior_sample_values()$mu, 
                post_mu = post_sample_values()$mu, 
                delta = input$graph_delta)
})

# makes it so the output is delayed until the user submits the data
sample_post_computations_outputs = eventReactive(input$submit_sample_post, {
  list(
    "xi_mu" = post_sample_values()$xi[,,1:5],
    "mu" = head(post_sample_values()$mu_xi, 5)
  )
})

output$sample_post_computation = renderPrint({
  sample_post_computations_outputs()
})

# cleaning the data before being downloaded - changing the column names.
download_post_sample_mu = reactive({
  data = as.data.frame(post_sample_values()$mu)
  for(i in 1:ncol(data)){
    colnames(data)[i] = paste("mu", i, sep = " ")
  }
  data
})

output$postsample_download_mu = downloadHandler(
  filename = "postsample_mu.csv",
  content = function(file) {
    write.csv(download_post_sample_mu(), file, row.names = FALSE)
  }
)

output$postsample_download_sigma = downloadHandler(
  filename = "postsample_sigma.csv",
  content = function(file) {
    # Note: difficult to change the column names for this particular case.
    write.csv(post_sample_values()$sigma, file, row.names = FALSE)
  }
)
output$postsample_download_k_zeta = downloadHandler(
  filename = "postsample_k_zeta.csv",
  content = function(file) {
    write.csv(post_sample_values()$k_zeta, file, row.names = FALSE)
  }
)


# GRAPHING FUNCTIONS ###########################################

output$sample_prior_graph = renderPlot({
  mu_graph(
    mu = prior_sample_values()$mu, 
    type = "Prior",
    col_num = input$mu_col,
    delta = input$graph_delta,
    colour_choice = c(convert_to_hex(input$prior_colour_hist),
                      convert_to_hex(input$prior_colour_line)),
    lty_type = as.numeric(input$prior_lty_type),
    transparency = input$prior_transparency)
})


output$sample_post_graph = renderPlot({
  mu_graph(
    mu = post_sample_values()$mu, 
    type = "Posterior",
    col_num = input$mu_col,
    delta = input$graph_delta,
    colour_choice = c(convert_to_hex(input$prior_colour_hist),
                      convert_to_hex(input$prior_colour_line)),
    lty_type = as.numeric(input$prior_lty_type),
    transparency = input$prior_transparency)
})

output$sample_rbr_graph = renderPlot({
  rbr_mu_graph(
    mu = rbr_sample_values()$rbr, 
    type = "RBR",
    col_num = input$mu_col,
    grid_vals = rbr_sample_values()$rbr_sequence,
    colour_choice = c(convert_to_hex(input$prior_colour_hist),
                      convert_to_hex(input$prior_colour_line)),
    lty_type = as.numeric(input$prior_lty_type),
    transparency = input$prior_transparency)
})