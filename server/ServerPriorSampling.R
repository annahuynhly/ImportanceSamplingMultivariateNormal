################################################################
# BACKEND FOR PRIOR SAMPLING                                   #
################################################################

prior_sampling_seed = reactive(input$prior_seed)

sample_prior_values = eventReactive(input$submit_prior_sampling, {
  set.seed(prior_sampling_seed())
  sample_prior(Nprior = input$prior_sample_bigN, 
               p = input$num_dimensions, 
               alpha01 = prior_elicitation_sigma_values()$alpha01,
               alpha02 = prior_elicitation_sigma_values()$alpha02,
               mu0 = prior_elicitation_mu_values()$mu0,
               lambda0 = prior_elicitation_mu_values()$lambda0)
})

sample_prior_values_cleaned = eventReactive(input$submit_prior_sampling, {
  sample_prior_data_cleaning(N = input$prior_sample_bigN, 
                             p = input$num_dimensions, 
                             mu_matrix = sample_prior_values()$mu_matrix, 
                             Sigma_matrix = sample_prior_values()$Sigma_mat)
})

sample_prior_values_cleaned_round = eventReactive(input$submit_prior_sampling, {
  round(sample_prior_values_cleaned(), 4)
})

prior_sample_display_head = eventReactive(input$submit_prior_sampling, {
  head(sample_prior_values_cleaned(), 10)
})

################################################################
# CODE FOR TABLE DISPLAY                                       #
################################################################

prior_display_cols <- reactiveValues()   
prior_display_cols$showing <- 1:5    

observeEvent(input$submit_prior_sampling, {
  showCols(prior_proxy, 1:5, reset = FALSE) #show the first five cols (because the colums are now all hidden)
})

#show the next five columns 
observeEvent(input$prior_next_five, {
  #stop when the last column is displayed
  if(prior_display_cols$showing[[length(prior_display_cols$showing)]] < length(sample_prior_values_cleaned_round())) {
    hideCols(prior_proxy, prior_display_cols$showing, reset = FALSE) #hide displayed cols
    prior_display_cols$showing = prior_display_cols$showing + 5
    showCols(prior_proxy, prior_display_cols$showing, reset = FALSE) #show the next five 
  } 
})

#similar mechanism but reversed to show the previous cols
observeEvent(input$prior_prev_five, {
  #stop when the first column is displayed
  if(prior_display_cols$showing[[1]] > 1) {
    hideCols(prior_proxy, prior_display_cols$showing, reset = FALSE) #hide displayed cols
    prior_display_cols$showing <- prior_display_cols$showing - 5
    showCols(prior_proxy, prior_display_cols$showing, reset = FALSE) #show previous five
  } 
})

output$prior_sample_table = renderDT(
  sample_prior_values_cleaned_round(),
  options = list(
    columnDefs = list(list(visible = FALSE, targets = 1:length(sample_prior_values_cleaned_round()))), #hide all columns
    scrollX = TRUE)  #for when many columns are visible
)

prior_proxy = dataTableProxy('prior_sample_table')

################################################################
# DOWNLOADING THE DATA                                         #
################################################################

output$download_prior_sample = downloadHandler(
  filename = "prior_sample.csv",
  content = function(file) {
    write.csv(sample_prior_values_cleaned(), file, row.names = FALSE)
  }
)