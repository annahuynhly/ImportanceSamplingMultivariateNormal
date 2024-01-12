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
  mu = rep(2, p)
  #mu = rep(0, p) 
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

observeEvent(input$post_download_info, {
  # Show a modal when the button is pressed
  shinyalert(html = TRUE, text = tagList(
    file_upload_example,
    br(),
    downloadButton(outputId = "sample_post_example_file", label = "Download Sample"),
  ))
})



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
                    gamma = input$virtual_uncertainty_prior, 
                    alpha01 = alpha01_prior(),
                    alpha02 = alpha02_prior(),
                    m1 = m1_prior(), 
                    m2 = m2_prior()
    )
  }
})

# makes it so the output is delayed until the user submits the data
sample_post_computations_outputs = eventReactive(input$submit_sample_post, {
  list(
    "xi" = post_sample_values()$xi[,,1:5],
    "mu_xi" = head(post_sample_values()$mu_xi, 5)
  )
})

output$sample_post_computation = renderPrint({
  sample_post_computations_outputs()
})

# cleaning the data before being downloaded - changing the column names.
download_post_sample_mu = reactive({
  data = as.data.frame(post_sample_values()$mu_xi)
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

output$postsample_download_xi = downloadHandler(
  filename = "postsample_xi.csv",
  content = function(file) {
    # Note: difficult to change the column names for this particular case.
    # todo: try to edit it later...
    write.csv(post_sample_values()$xi, file, row.names = FALSE)
  }
)

# GRAPHING FUNCTIONS ###########################################

# plotting for the relative belief ratio
rbr_sample_values = reactive({
  if(input$postsample_use == 1){ # input values
    # NOTE: may need to fix the first parameter based on other values
    sample_rbr_new(gamma = input$virtual_uncertainty_post, 
                   delta = input$comparison_graph_delta, 
                   alpha01 = alpha01_post(), 
                   alpha02 = alpha02_post(), 
                   m1 = m1_post(), 
                   m2 = m2_post(),
                   mu_post = post_sample_values()$mu_xi)
  } else if (input$postsample_use == 2){ # same as elicit
    sample_rbr_new(gamma = input$virtual_uncertainty_post, 
                   delta = input$comparison_graph_delta, 
                   alpha01 = prior_elicitation_values()$alpha01, 
                   alpha02 = prior_elicitation_values()$alpha02,
                   m1 = m1_list(), 
                   m2 = m2_list(),
                   mu_post = post_sample_values()$mu_xi)
  } else if (input$postsample_use == 3){ # same as prior
    sample_rbr_new(gamma = input$virtual_uncertainty_prior, 
                   delta = input$comparison_graph_delta, 
                   alpha01 = alpha01_prior(),
                   alpha02 = alpha02_prior(),
                   m1 = m1_prior(), 
                   m2 = m2_prior(),
                   mu_post = post_sample_values()$mu_xi)
  }
})

output$sample_priorpost_graph = renderPlot({
  mu_graph_comparison(grid = rbr_sample_values()$grid, 
                      mu_prior = rbr_sample_values()$prior_mu, 
                      mu_post = rbr_sample_values()$post_mu, 
                      col_num = input$comparison_mu_col,
                      smooth_num = input$comparison_smoother,
                      colour_choice = c(input$comparison_prior_col, 
                                        input$comparison_post_col),
                      lty_type = c(as.numeric(input$comparison_prior_lty), 
                                   as.numeric(input$comparison_post_lty)), 
                      transparency = input$comparison_transparency
  )
})

output$sample_rbr_graph = renderPlot({
  rbr_mu_graph(grid = rbr_sample_values()$grid, 
               mu = rbr_sample_values()$rbr_mu, 
               type = "relative belief ratio", 
               smooth_num = input$comparison_smoother,
               col_num = input$comparison_mu_col,
               colour_choice = input$comparison_rbr_col,
               lty_type = as.numeric(input$comparison_rbr_lty),
               transparency = input$comparison_transparency)
})


# rename this..
the_sample_post_graph = eventReactive(input$submit_sample_post, {
  mu_graph(
    mu = post_sample_values()$mu_xi, 
    type = "Posterior",
    col_num = input$post_graph_num,
    delta = input$post_graph_delta,
    smooth_num = input$post_graph_smoother,
    colour_choice = c(input$post_line_col, 
                      input$post_hist_col),
    lty_type = 2,
    transparency = input$post_transparency)
})


###
# this was modified
output$sample_post_graph = renderPlot({
  the_sample_post_graph()
})

## the new download is below

sample_prior_computations_graph_DOWNLOAD = function(){
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
         xlab = TeX(paste("Value of $\\mu_{", col, "}$")),
         ylab = "Density", 
         main = TeX(paste("Density plot of", r'($\mu$)', col)),
         border = "#ffffff")
    lines(xnew, ynew, lwd = 2, lty = 2)
  } else {
    plot(xnew, ynew, lwd = 2, type="l", 
         xlab = TeX(paste("Value of $\\mu_{", col, "}$")),
         ylab = "Density", 
         main = TeX(paste("Density plot of $\\mu_{", col, "}$"))
    )
  }
  
}

output$plot_prior_mu = downloadHandler(
  filename = "Prior Mu Plot.png",
  content = function(file){
    png(file)
    sample_prior_computations_graph_DOWNLOAD()
    dev.off()
  }
)

sample_post_computations_graph_DOWNLOAD = function(){
  mu_graph(
    mu = post_sample_values()$mu_xi, 
    type = "Posterior",
    col_num = input$post_graph_num,
    delta = input$post_graph_delta,
    smooth_num = input$post_graph_smoother,
    colour_choice = c(input$post_line_col, 
                      input$post_hist_col),
    lty_type = 2,
    transparency = input$post_transparency)
}

output$plot_post_mu = downloadHandler(
  filename = "Post Mu Plot.png",
  content = function(file){
    png(file)
    sample_post_computations_graph_DOWNLOAD()
    dev.off()
  }
)



output$comparison_download_plot = downloadHandler(
  filename = "Comparison Plot.png",
  content = function(file){
    png(file)
    mu_graph_comparison(grid = rbr_sample_values()$grid, 
                        mu_prior = rbr_sample_values()$prior_mu, 
                        mu_post = rbr_sample_values()$post_mu, 
                        col_num = input$comparison_mu_col,
                        smooth_num = input$comparison_smoother,
                        colour_choice = c(input$comparison_prior_col, 
                                          input$comparison_post_col),
                        lty_type = c(as.numeric(input$comparison_prior_lty), 
                                     as.numeric(input$comparison_post_lty)), 
                        transparency = input$comparison_transparency)
    dev.off()
  }
)

output$rbr_download_plot = downloadHandler(
  filename = "RBR Plot.png",
  content = function(file){
    png(file)
    rbr_mu_graph(grid = rbr_sample_values()$grid, 
                 mu = rbr_sample_values()$rbr_mu, 
                 type = "relative belief ratio", 
                 smooth_num = input$comparison_smoother,
                 col_num = input$comparison_mu_col,
                 colour_choice = input$comparison_rbr_col,
                 lty_type = as.numeric(input$comparison_rbr_lty),
                 transparency = input$comparison_transparency)
    dev.off()
  }
)



