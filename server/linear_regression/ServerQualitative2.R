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
                    beta0 = qual_prior_elicit_mu()$beta0,
                    lambda0 = qual_prior_elicit_mu()$lambda0)
})

qual_sample_post_values = eventReactive(input$qual_submit_post_sampling, {
  set.seed(qual_post_sampling_seed())
  qual_sample_post(Npost = input$qual_post_sample_bigN, 
                   X = qual_sufficient_stat_comp_manual()$X,
                   k = prod(create_necessary_vector(input$qual_num_levels)), 
                   n = sum(create_necessary_vector(input$qual_num_n)), 
                   alpha01 = qual_prior_sigma()$alpha01, 
                   alpha02 = qual_prior_sigma()$alpha02,  
                   lambda0 = qual_prior_elicit_mu()$lambda0, 
                   beta0 = qual_prior_elicit_mu()$beta0, 
                   b = qual_sufficient_stat_comp()$b, 
                   s_2 = qual_sufficient_stat_comp()$s_2)
})

qual_sample_prior_values_cleaned = eventReactive(input$qual_submit_prior_sampling, {
  qual_sample_reformat(levels = create_necessary_vector(input$qual_num_levels), 
                       sigma_2_vector = qual_sample_prior_values()$prior_sigma_2_vector, 
                       beta_matrix = qual_sample_prior_values()$prior_beta_matrix)
})

qual_sample_post_values_cleaned = eventReactive(input$qual_submit_post_sampling, {
  qual_sample_reformat(levels = create_necessary_vector(input$qual_num_levels), 
                       sigma_2_vector = qual_sample_post_values()$post_sigma_2_vector, 
                       beta_matrix = qual_sample_post_values()$post_beta_matrix)
})

################################################################
# CODE FOR TABLE DISPLAY                                       #
################################################################

# FOR THE PRIOR ################################################

qual_prior_display_cols = reactiveValues()   
qual_prior_display_cols$showing = 1:5    

observeEvent(input$qual_submit_prior_sampling, {
  showCols(qual_prior_proxy, 1:5, reset = FALSE) #show the first five cols (because the colums are now all hidden)
})

#show the next five columns 
observeEvent(input$qual_prior_next_five, {
  #stop when the last column is displayed
  if(qual_prior_display_cols$showing[[length(qual_prior_display_cols$showing)]] < length(qual_sample_prior_values_cleaned())) {
    hideCols(qual_prior_proxy, qual_prior_display_cols$showing, reset = FALSE) #hide displayed cols
    qual_prior_display_cols$showing = qual_prior_display_cols$showing + 5
    showCols(qual_prior_proxy, qual_prior_display_cols$showing, reset = FALSE) #show the next five 
  } 
})

#similar mechanism but reversed to show the previous cols
observeEvent(input$qual_prior_prev_five, {
  #stop when the first column is displayed
  if(qual_prior_display_cols$showing[[1]] > 1) {
    hideCols(qual_prior_proxy, qual_prior_display_cols$showing, reset = FALSE) #hide displayed cols
    qual_prior_display_cols$showing <- qual_prior_display_cols$showing - 5
    showCols(qual_prior_proxy, qual_prior_display_cols$showing, reset = FALSE) #show previous five
  } 
})

output$qual_prior_sample_table = renderDT(
  qual_sample_prior_values_cleaned(),
  options = list(
    columnDefs = list(list(visible = FALSE, targets = 1:length(qual_sample_prior_values_cleaned()))), #hide all columns
    scrollX = TRUE)  #for when many columns are visible
)

qual_prior_proxy = dataTableProxy('qual_prior_sample_table')

# FOR THE POSTERIOR ############################################

qual_post_display_cols = reactiveValues()   
qual_post_display_cols$showing = 1:5    

observeEvent(input$qual_submit_post_sampling, {
  showCols(qual_post_proxy, 1:5, reset = FALSE) #show the first five cols (because the colums are now all hidden)
})

#show the next five columns 
observeEvent(input$qual_post_next_five, {
  #stop when the last column is displayed
  if(qual_post_display_cols$showing[[length(qual_post_display_cols$showing)]] < length(qual_sample_post_values_cleaned())) {
    hideCols(qual_post_proxy, qual_post_display_cols$showing, reset = FALSE) #hide displayed cols
    qual_post_display_cols$showing = qual_post_display_cols$showing + 5
    showCols(qual_post_proxy, qual_post_display_cols$showing, reset = FALSE) #show the next five 
  } 
})

#similar mechanism but reversed to show the previous cols
observeEvent(input$qual_post_prev_five, {
  #stop when the first column is displayed
  if(qual_post_display_cols$showing[[1]] > 1) {
    hideCols(qual_post_proxy, qual_post_display_cols$showing, reset = FALSE) #hide displayed cols
    qual_post_display_cols$showing <- qual_post_display_cols$showing - 5
    showCols(qual_post_proxy, qual_post_display_cols$showing, reset = FALSE) #show previous five
  } 
})

output$qual_post_sample_table = renderDT(
  qual_sample_post_values_cleaned(),
  options = list(
    columnDefs = list(list(visible = FALSE, targets = 1:length(qual_sample_post_values_cleaned()))), #hide all columns
    scrollX = TRUE)  #for when many columns are visible
)

qual_post_proxy = dataTableProxy('qual_post_sample_table')

################################################################
# DOWNLOADING THE DATA                                         #
################################################################

output$qual_download_prior_sample = downloadHandler(
  filename = "qual_prior_sample.csv",
  content = function(file) {
    write.csv(qual_sample_prior_values_cleaned(), file, row.names = FALSE)
  }
)

output$qual_download_post_sample = downloadHandler(
  filename = "qual_post_sample.csv",
  content = function(file) {
    write.csv(qual_sample_post_values_cleaned(), file, row.names = FALSE)
  }
)

## WHAT WAS HERE BEFORE

#output$debuggingpriorqual123 = renderPrint({
#  qual_sample_prior_reformat(levels = create_necessary_vector(input$qual_num_levels), 
#                             sigma_2_vector = qual_sample_prior_values()$prior_sigma_2_vector, 
#                             beta_matrix = qual_sample_prior_values()$prior_beta_matrix)
#})

