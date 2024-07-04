################################################################
# MULTINOMIAL REGRESSION                                       #
################################################################

dummy_data = data.frame(s1 = c(1, 0.5, 0.2, 0.5, 1),
                        s2 = c(7, 4, 3, 4, 7),
                        lower_bd = c(0,0,0,0,0),
                        upper_bd = c(50,50,50,50,50),
                        m1 = c(-5, -3, -2, -1, 0),
                        m2 = c(0,1,2,3,5))

# Prior Sigma Sample TXT (OPERATIONAL)
output$prior_sigma_txt_example = downloadHandler(
  filename = "prior_sigma_example.txt",
  content = function(file) {
    write.table(dummy_data, file, sep = "\t", row.names = FALSE, 
                col.names = TRUE)
    #write.csv(dummy_data, file)
  }
)

# Prior Sigma Sample CSV (OPERATIONAL)
output$prior_sigma_csv_example = downloadHandler(
  filename = "prior_sigma_example.csv",
  content = function(file) {
    write.csv(dummy_data, file)
  }
)

# Table
output$prior_sigma_txt_example_table = renderDT(
  dummy_data
)

post_example_seed = reactive(input$post_default_example_seed)

# creating test sample data (changes every time for fun!)
test_sample_data = reactive({
  set.seed(post_example_seed())
  mu = c(-2, -1, 0, 1, 2) 
  sigma = diag(c(2, 1, 0.5, 1, 2))
  R = 1/2 * diag(5) + 1/2 * c(1, 1, 1, 1, 1) %*%  t(c(1, 1, 1, 1, 1))
  sigma_mat = sigma %*% R %*% sigma
  n = 50 # num samples
  Y = mvrnorm(n = n, mu = mu, Sigma = sigma_mat)
  Y = round(Y, 3) # rounding to make it nicer
  data = as.data.frame(Y)
  colnames(data) = c("Y1", "Y2", "Y3", "Y4", "Y5")
  data
})

# Sample data for uploading Y_{1i}s - csv
output$post_computation_input_example_csv = downloadHandler(
  filename = "Y_example.csv",
  content = function(file) {
    write.csv(test_sample_data(), file, row.names = FALSE)
  }
)

# Sample data for uploading Y_{1i}s - txt
output$post_computation_input_example_txt = downloadHandler(
  filename = "Y_example.txt",
  content = function(file) {
    write.table(test_sample_data(), file, sep = "\t", 
                row.names = FALSE, col.names = TRUE)
  }
)

output$post_comp_example_csv_table = renderDT(
  test_sample_data()
)

################################################################
# QUALITATIVE LINEAR REGRESSION                                #
################################################################

# generating a sample for Y 
generate_Y_qual_seed = reactive(input$qual_default_example_seed)

sample_Y_qual_data = reactive({
  set.seed(generate_Y_qual_seed())
  
  l = create_necessary_vector(input$qual_num_levels) # input the number of levels per factor
  m = length(l) # the number of factors.
  k = prod(l) # letting this denote the possible number of combinations between the crossed factors 
  
  # the size of n_vector should be equal to k (if factors are crossed)
  n_vector = create_necessary_vector(input$qual_num_n)
  n = sum(n_vector)
  
  mu = c(2, 4, 6, 8, 10, 12)
  Y = numeric(n)
  for(i in 1:k){
    if(i == 1){
      Y[1:n[i]] = rnorm(n_vector[i], mean = mu[i], sd = 2)
    } else {
      Y[(cumsum(n_vector)[i-1]+1):cumsum(n_vector)[i]] = rnorm(n_vector[i], mean = mu[i], sd = 2)
    }
  }
  Y # y should be a vector
})

output$sample_Y_text_output = renderPrint({
  sample_Y_qual_data()
})

qual_Y_sample_df = reactive({
  data.frame(Y = sample_Y_qual_data())
})

output$qual_Y_example_csv = downloadHandler(
  filename = "qual_Y_example.csv",
  content = function(file) {
    write.csv(qual_Y_sample_df(), file)
  }
)

output$qual_Y_example_txt = downloadHandler(
  # need to fix this
  filename = "qual_Y_example.txt",
  content = function(file) {
    write.table(qual_Y_sample_df(), file, sep = "\t", 
                row.names = FALSE, col.names = TRUE)
  }
)




qual_dummy_data = data.frame(
  m = c(2, NA, NA, NA, NA, NA),
  li = c(2, 3, NA, NA, NA, NA),
  n = c(5, 5, 5, 5, 5, 5),
  s1 = c(1, NA, NA, NA, NA, NA),
  s2 = c(7, NA, NA, NA, NA, NA),
  lower_bd = c(0, NA, NA, NA, NA, NA),
  upper_bd = c(50, NA, NA, NA, NA, NA),
  m1 = c(0, 2, 4, 6, 8, 10),
  m2 = c(4, 6, 8, 10, 12, 14)
)

output$qual_initial_input_example_csv = downloadHandler(
  filename = "qual_csv_example.csv",
  content = function(file) {
    write.csv(qual_dummy_data, file)
  }
)

output$qual_initial_input_example_txt = downloadHandler(
  filename = "qual_csv_example.txt",
  content = function(file) {
    write.table(qual_dummy_data, file, sep = "\t", row.names = FALSE, 
                col.names = TRUE)
  }
)


