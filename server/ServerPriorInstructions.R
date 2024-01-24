dummy_data = data.frame(s1 = c(2, 2, 2), s2 = c(10,10,10),
                        lower_bd = c(0,0,0), upper_bd = c(50,50,50),
                        m1 = c(-5,-5,-5), m2 = c(5,5,5))

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

