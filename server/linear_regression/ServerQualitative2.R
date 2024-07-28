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

output$beta_order_output_prior = renderUI({
  beta_names = create_beta_list_names(levels = create_necessary_vector(input$qual_num_levels))
  beta_latex = beta_list_latex(beta_names, between = ", ", end = "")
  # Use HTML to display the expression with MathJax
  HTML(paste0("$$", beta_latex, ", \\sigma^{2}", "$$"))
})

# Reprocess MathJax to ensure proper rendering
observe({
  invalidateLater(500, session)
  session$sendCustomMessage(type = 'mathjax_reprocess2', message = list())
})

output$beta_order_output_post = renderUI({
  beta_names = create_beta_list_names(levels = create_necessary_vector(input$qual_num_levels))
  beta_latex = beta_list_latex(beta_names, between = ", ", end ="")
  # Use HTML to display the expression with MathJax
  HTML(paste0("$$", beta_latex, ", \\sigma^{2}",  "$$"))
})

# Reprocess MathJax to ensure proper rendering
observe({
  invalidateLater(500, session)
  session$sendCustomMessage(type = 'mathjax_reprocess3', message = list())
})

################################################################
# CODE FOR TABLE DISPLAY                                       #
################################################################

# FOR THE PRIOR ################################################

qual_prior_display_cols = reactiveValues(showing = 1:5)

observeEvent(input$qual_submit_prior_sampling, {
  # Show the first five columns
  qual_prior_display_cols$showing = 1:min(5, ncol(qual_sample_prior_values_cleaned()))
  showCols(qual_prior_proxy, qual_prior_display_cols$showing, reset = FALSE)
})

observeEvent(input$qual_prior_next_five, {
  last_col = qual_prior_display_cols$showing[[length(qual_prior_display_cols$showing)]]
  total_cols = ncol(qual_sample_prior_values_cleaned())
  
  # Check if there are more columns to show
  if (last_col < total_cols) {
    hideCols(qual_prior_proxy, qual_prior_display_cols$showing, reset = FALSE)
    qual_prior_display_cols$showing = (last_col + 1):min(last_col + 5, total_cols)
    showCols(qual_prior_proxy, qual_prior_display_cols$showing, reset = FALSE)
  }
})

observeEvent(input$qual_prior_prev_five, {
  first_col = qual_prior_display_cols$showing[[1]]
  
  # Check if there are previous columns to show
  if (first_col > 1) {
    hideCols(qual_prior_proxy, qual_prior_display_cols$showing, reset = FALSE)
    qual_prior_display_cols$showing = max(1, first_col - 5):(first_col - 1)
    showCols(qual_prior_proxy, qual_prior_display_cols$showing, reset = FALSE)
  }
})

output$qual_prior_sample_table = renderDT({
  datatable(
    qual_sample_prior_values_cleaned(),
    options = list(
      columnDefs = list(list(visible = FALSE, targets = 1:ncol(qual_sample_prior_values_cleaned()))),
      scrollX = TRUE
    )
  )
})

qual_prior_proxy = dataTableProxy('qual_prior_sample_table')

# FOR THE POSTERIOR ############################################

qual_post_display_cols = reactiveValues(showing = 1:5)

observeEvent(input$qual_submit_post_sampling, {
  # Show the first five columns
  qual_post_display_cols$showing = 1:min(5, ncol(qual_sample_post_values_cleaned()))
  showCols(qual_post_proxy, qual_post_display_cols$showing, reset = FALSE)
})

observeEvent(input$qual_post_next_five, {
  last_col = qual_post_display_cols$showing[[length(qual_post_display_cols$showing)]]
  total_cols = ncol(qual_sample_post_values_cleaned())
  
  # Check if there are more columns to show
  if (last_col < total_cols) {
    hideCols(qual_post_proxy, qual_post_display_cols$showing, reset = FALSE)
    qual_post_display_cols$showing = (last_col + 1):min(last_col + 5, total_cols)
    showCols(qual_post_proxy, qual_post_display_cols$showing, reset = FALSE)
  }
})

observeEvent(input$qual_post_prev_five, {
  first_col = qual_post_display_cols$showing[[1]]
  
  # Check if there are previous columns to show
  if (first_col > 1) {
    hideCols(qual_post_proxy, qual_post_display_cols$showing, reset = FALSE)
    qual_post_display_cols$showing = max(1, first_col - 5):(first_col - 1)
    showCols(qual_post_proxy, qual_post_display_cols$showing, reset = FALSE)
  }
})

output$qual_post_sample_table = renderDT({
  datatable(
    qual_sample_post_values_cleaned(),
    options = list(
      columnDefs = list(list(visible = FALSE, targets = 1:ncol(qual_sample_post_values_cleaned()))),
      scrollX = TRUE
    )
  )
})

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

