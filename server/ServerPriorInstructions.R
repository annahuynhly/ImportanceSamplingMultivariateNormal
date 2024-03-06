dummy_data = data.frame(s1 = c(1, 0.5, 0.2, 0.5, 1),
                        s2 = c(7, 4, 3, 4, 7),
                        lower_bd = c(0,0,0,0,0),
                        upper_bd = c(50,50,50,50,50),
                        m1 = c(-5, -3, -2, -1, 0),
                        m2 = c(0,1,2,3,5))

# Prior sigma - text

# sample file - for users to understand the format
output$prior_sigma_txt_example = downloadHandler(
  filename = "prior_sigma_example.txt",
  content = function(file) {
    write.csv(dummy_data, file)
  }
)

# For telling the user how to input .txt files
observeEvent(input$prior_sigma_submit_info_txt, {
  # Show a modal when the button is pressed
  shinyalert(html = TRUE, text = tagList(
    prior_sigma_upload_instructions,
    br(),
    downloadButton(outputId = "prior_sigma_txt_example", 
                   label = "Download Sample"),
  ))
})

# Prior sigma - csv

# sample file - for users to understand the format
output$prior_sigma_csv_example = downloadHandler(
  filename = "prior_sigma_example.csv",
  content = function(file) {
    write.csv(dummy_data, file)
  }
)

# For telling the user how to input .csv files
observeEvent(input$prior_sigma_submit_info_csv, {
  # Show a modal when the button is pressed
  shinyalert(html = TRUE, text = tagList(
    prior_sigma_upload_instructions,
    br(),
    downloadButton(outputId = "prior_sigma_csv_example", 
                   label = "Download Sample"),
  ))
})

# Prior mu - txt

# sample file - for users to understand the format
output$prior_mu_txt_example = downloadHandler(
  filename = "prior_mu_example.txt",
  content = function(file) {
    write.csv(dummy_data, file)
  }
)

# For telling the user how to input .txt files
observeEvent(input$prior_mu_submit_info_txt, {
  # Show a modal when the button is pressed
  shinyalert(html = TRUE, text = tagList(
    prior_mu_upload_instructions,
    br(),
    downloadButton(outputId = "prior_mu_txt_example", 
                   label = "Download Sample"),
  ))
})

# Prior mu - csv

# sample file - for users to understand the format
output$prior_mu_csv_example = downloadHandler(
  filename = "prior_mu_example.csv",
  content = function(file) {
    write.csv(dummy_data, file)
  }
)

# For telling the user how to input .txt files
observeEvent(input$prior_mu_submit_info_csv, {
  # Show a modal when the button is pressed
  shinyalert(html = TRUE, text = tagList(
    prior_mu_upload_instructions,
    br(),
    downloadButton(outputId = "prior_mu_csv_example", 
                   label = "Download Sample"),
  ))
})

# Table
output$prior_sigma_txt_example_table = renderDT(
  dummy_data
)

################################################################
# POSTERIOR COMPUTATIONS EXAMPLE                               #
################################################################

# creating test sample data (changes every time for fun!)
test_sample_data = reactive({
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

# test data for uploading Y_{1i}s - csv
output$post_computation_input_example_csv = downloadHandler(
  filename = "Y_example.csv",
  content = function(file) {
    write.csv(test_sample_data(), file, row.names = FALSE)
  }
)

# test data for uploading Y_{1i}s - txt
output$post_computation_input_example_txt = downloadHandler(
  filename = "Y_example.txt",
  content = function(file) {
    write.csv(test_sample_data(), file, row.names = FALSE)
  }
)


observeEvent(input$post_download_info, {
  # Show a modal when the button is pressed
  shinyalert(html = TRUE, text = tagList(
    file_upload_example,
    br(),
    downloadButton(outputId = "post_computation_input_example_csv", label = "Download Sample"),
  ))
})

output$post_comp_example_csv_table = renderDT(
  test_sample_data()
)

