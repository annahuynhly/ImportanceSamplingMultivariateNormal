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

################################################################
# DOWNLOADING THE CODE                                         #
################################################################

psi_code_text2 = c(
  "# The user is expected to modify values for Parts 0, 2, and 4 shown below.",
  "",
  "# Part 0: Set the working directory ***",
  "setwd(\"/Users/annaly/Downloads\")",
  "",
  "# Importing the files downloaded in the previous sections",
  "prior = read.csv(\"prior_sample.csv\")",
  "imp = read.csv(\"importance_sample.csv\")",
  
  "",
  
  "# Part 1: Converting the data to be more user-friendly for defining the psi function",
  "convert_for_psi = function(csv_data){",
  "  #' Given a data frame that contains values for mu and the correlations from the",
  "  #' Sigma matrix and xi matrix, reformats the data such that we have appropriate",
  "  #' matrices for Sigma and xi.",
  "  #' @param csv_data the csv data being used.",
  "",
  "  mu_indices = grep(\"^mu_\", names(csv_data), ignore.case = TRUE)",
  "  p = length(mu_indices)",
  "  mu_matrix = csv_data[,mu_indices]",
  "",
  "  sigma_indices = grep(\"^sigma_\", names(csv_data), ignore.case = TRUE)",
  "  sigma_values = csv_data[,sigma_indices]",
  "",
  "  xi_indices = grep(\"^xi_\", names(csv_data), ignore.case = TRUE)",
  "  xi_values = csv_data[,xi_indices]",
  "",
  "  Sigma_matrix = xi_matrix = vector(\"list\", nrow(sigma_values))",
  "  for(k in 1:nrow(sigma_values)){",
  "    new_sigma_matrix = new_xi_matrix = matrix(NA, nrow = p, ncol = p)",
  "    # Fill in the symmetric matrix",
  "    index = 1",
  "    for (i in 1:p) {",
  "      for (j in i:p) {",
  "        if(i != j){",
  "          new_sigma_matrix[i, j] = as.numeric(sigma_values[k,][index])",
  "          new_sigma_matrix[j, i] = as.numeric(sigma_values[k,][index])",
  "          new_xi_matrix[i, j] = as.numeric(xi_values[k,][index])",
  "          new_xi_matrix[j, i] = as.numeric(xi_values[k,][index])",
  "        } else {",
  "          new_sigma_matrix[i, i] = as.numeric(sigma_values[k,][index])",
  "          new_xi_matrix[i, i] = as.numeric(xi_values[k,][index])", 
  "        }",
  "        index = index + 1",
  "      }",
  "    }",
  "    Sigma_matrix[[k]] = new_sigma_matrix",
  "    xi_matrix[[k]] = new_xi_matrix",
  "  }",
  "",
  "  newlist = list(\"mu_matrix\" = mu_matrix,", 
  "                 \"Sigma_matrix\" = Sigma_matrix,", 
  "                 \"xi_matrix\" = xi_matrix)",
  "  return(newlist)",
  "}",
  
  "",
  
  "prior_val = convert_for_psi(prior)",
  "imp_val = convert_for_psi(imp)",
  
  "",
  
  "# Part 2: Modifying the Psi function ***",
  "psifn = function(muval, Sigmaval, xival){",
  "  #' @param muval is a vector containing a row of mu's from 1 to p",
  "  #' @param Sigmaval is a variance matrix", 
  "  #' @param xival is the precision matrix associated with Sigmaval",
  "  #' The user may change the psi function below, depending on what they wish to make inferences about.",
  "  #' Below, commented out values of psi, are some suggestions:",
  "  #psi = muval[1] # making inference for the first mu value",
  "  #psi = Sigmaval[2, 2] # making inferences for the variance of Y_2",
  "  psi = xival[1,1] # making inference for the ijth value of the matrix xi",
  "  return(psi)",
  "}",
  
  "",
  
  "# Part 3: Computing the psi values from the downloaded samples",
  "compute_psi_vals = function(mu, Sigma, xi){",
  "  #' Computing the values for psi to be used to make density histograms.",
  "  #' @mu denotes the mu matrix",
  "  #' @Sigma denotes a vector of Sigma matrices",
  "  #' @xi denotes a vector of xi matrices",
  "  N = nrow(mu)",
  "  dens = numeric(N)",
  "  for (i in 1:N){",
  "    dens[i] = as.numeric(psifn(mu[i,], Sigma[[i]], xi[[i]]))",
  "  }",
  "  return(dens)",
  "}",
  
  "",
  
  "prior_psi_vals = compute_psi_vals(prior_val$mu_matrix, prior_val$Sigma_matrix, prior_val$xi_matrix)",
  "imp_psi_vals = compute_psi_vals(imp_val$mu_matrix, imp_val$Sigma_matrix, imp_val$xi_matrix)",
  
  "",
  
  "# Part 4: Saving the results in a .csv. This is to be uploaded into the site! ***",
  "write.csv(prior_psi_vals, \"prior_psi_vals.csv\")",
  "write.csv(imp_psi_vals, \"imp_psi_vals.csv\")"
)
 
psi_code_text = reactive({
  cat(psi_code_text2, sep = "\n")
})

output$download_psi_code = downloadHandler(
  filename = function(){
    "psi_code_text.R"
  },
  content = function(file) {
    writeLines(psi_code_text2, file)
  }
)

output$psi_code = renderPrint({
  psi_code_text()
})

