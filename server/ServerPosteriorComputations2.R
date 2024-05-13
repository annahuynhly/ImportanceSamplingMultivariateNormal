################################################################
# CODE FOR THE TABLE OF THE IMPORTANCE SAMPLER VALUES          #
################################################################

imp_display_cols = reactiveValues()   
imp_display_cols$showing = 1:5    

observeEvent(input$submit_imp_sampler, {
  showCols(imp_proxy, 1:5, reset = FALSE) #show the first five cols (because the colums are now all hidden)
})

#show the next five columns 
observeEvent(input$imp_next_five, {
  #stop when the last column is displayed
  if(imp_display_cols$showing[[length(imp_display_cols$showing)]] < length(important_values_reformatted_round())) {
    hideCols(imp_proxy, imp_display_cols$showing, reset = FALSE) #hide displayed cols
    imp_display_cols$showing = imp_display_cols$showing + 5
    showCols(imp_proxy, imp_display_cols$showing, reset = FALSE) #show the next five 
  } 
})

#similar mechanism but reversed to show the previous cols
observeEvent(input$imp_prev_five, {
  #stop when the first column is displayed
  if(imp_display_cols$showing[[1]] > 1) {
    hideCols(imp_proxy, imp_display_cols$showing, reset = FALSE) #hide displayed cols
    imp_display_cols$showing = imp_display_cols$showing - 5
    showCols(imp_proxy, imp_display_cols$showing, reset = FALSE) #show previous five
  } 
})

output$imp_display_table = renderDT(
  important_values_reformatted_round(),
  options = list(
    #hide all columns
    columnDefs = list(list(visible = FALSE, targets = 1:length(important_values_reformatted_round()))), 
    scrollX = TRUE)  #for when many columns are visible
)

imp_proxy = dataTableProxy('imp_display_table')

################################################################
# CODE FOR THE TABLE OF THE SIR ALGORITHM SAMPLES              #
################################################################

post_display_cols = reactiveValues()   
post_display_cols$showing = 1:5    

observeEvent(input$submit_sample_post, {
  showCols(post_proxy, 1:5, reset = FALSE) #show the first five cols (because the colums are now all hidden)
})

#show the next five columns 
observeEvent(input$post_next_five, {
  #stop when the last column is displayed
  if(post_display_cols$showing[[length(post_display_cols$showing)]] < length(post_SIR_calculations_reformat())) {
    hideCols(post_proxy, post_display_cols$showing, reset = FALSE) #hide displayed cols
    post_display_cols$showing = post_display_cols$showing + 5
    showCols(post_proxy, post_display_cols$showing, reset = FALSE) #show the next five 
  } 
})

#similar mechanism but reversed to show the previous cols
observeEvent(input$post_prev_five, {
  #stop when the first column is displayed
  if(post_display_cols$showing[[1]] > 1) {
    hideCols(post_proxy, post_display_cols$showing, reset = FALSE) #hide displayed cols
    post_display_cols$showing = post_display_cols$showing - 5
    showCols(post_proxy, post_display_cols$showing, reset = FALSE) #show previous five
  } 
})

output$post_display_table = renderDT(
  post_SIR_calculations_reformat(),
  options = list(
    #hide all columns
    columnDefs = list(list(visible = FALSE, targets = 1:length(post_SIR_calculations_reformat()))), 
    scrollX = TRUE)  #for when many columns are visible
)

post_proxy = dataTableProxy('post_display_table')


