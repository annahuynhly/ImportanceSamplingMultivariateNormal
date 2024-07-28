dot_product_expression = function(coefficients, betas){
  #' Expresses the dot product between coefficients and a vector containing characters.
  #' Assumption: length(coefficients) == length(betas)
  #' @param coefficients is a vector containing the coefficients.
  #' @param betas is a vector containing the characters. Doesn't necessarily have to be of beta's.
  #' @examples
  #' > dot_product_expression(c(1, 1, 0, -1, 1, 0), c("b1", "b2", "b3", "b4", "b5", "b6"))
  #' "b1 + b2 - b4 + b5"
  
  # Filter out zero coefficients
  non_zero_indices = which(coefficients != 0)
  non_zero_coefficients = coefficients[non_zero_indices]
  non_zero_betas = betas[non_zero_indices]
  
  # Create a vector to store the terms of the expression
  terms = vector("character", length(non_zero_coefficients))
  
  # Construct the expression terms
  for (i in seq_along(non_zero_coefficients)) {
    coeff = non_zero_coefficients[i]
    beta = non_zero_betas[i]
    
    # Add the term to the vector, handling the sign of the coefficient
    if (coeff == 1) {
      if(i == 1){
        terms[i] = beta
      } else {
        terms[i] = paste0("+ ", beta)
      }
    } else if (coeff > 1) {
      if(i == 1){
        terms[i] = paste0(coeff, beta)
      } else {
        terms[i] = paste0("+ ", coeff, beta)
      }
    } else if (coeff == -1) {
      terms[i] = paste0("- ", beta)
    } else if (coeff < -1){
      terms[i] = paste0("- ", coeff, beta)
    }
  }
  
  # Concatenate the terms into a single string
  expression <- paste(terms, collapse = " ")
  
  # Remove any leading '+' if present
  expression <- sub("^\\+\\s*", "", expression)
  
  return(expression)
}

dot_product_expression_latex = function(structure) {
  #' Assumption: user used dot_product_expression first.
  #' @param structure is the character containing the beta expression.
  expression <- gsub("(\\b\\d*\\s*)b(\\d+)", "\\1\\\\beta_{\\2}", structure)
  return(expression)
}

calculate_indices = function(i, L){
  # Function to calculate indices for each level
  indices = numeric(length(L))
  remaining = i - 1
  
  for(j in 1:length(L)) {
    indices[j] = (remaining %% L[j]) + 1
    remaining = (remaining %/% L[j])
  }
  return(indices)
}

create_beta_list_names = function(levels, text = "b"){
  #' Gives a list of the order of the beta matrix.
  #' @param levels a vector containing the number of levels per factor.
  #' @param text indicates the initials before the indices.
  #' @examples
  #' >generate_beta_vector(c(2,3,3))
  #' [1] "b111" "b112" "b113" "b121" "b122" "b123" "b131" "b132" "b133" "b211" "b212" "b213" "b221"
  #' [14] "b222" "b223" "b231" "b232" "b233
  Lprod = prod(levels) # Calculate the product of levels
  # Generate beta vector using vectorization
  beta_vector = sapply(1:Lprod, function(i){
    indices = calculate_indices(i, rev(levels))
    paste(text, paste(rev(indices), collapse = ""), sep = "")
  })
  return(beta_vector)
}

find_position = function(value, beta_vector){
  #' Function to find the position of a value in the beta vector
  #' @param value represents the beta value of interest
  #' @param beta_vector represents the vector containing the beta values
  #' @examples 
  #' > beta_vector = c("b111" "b112" "b113" "b121", "1222")
  #' > find_position("b113", beta_vector)
  #' [1] 3
  
  position = match(value, beta_vector)
  return(position)
}

beta_list_latex = function(structure){
  #' Assumption: user used create_beta_list_names first.
  #' @param structure is the character containing the beta expression.
  
  # Convert to LaTeX format
  latex_string = paste0("\\beta_{", substring(structure, 2), "}", collapse = ", \\quad")
  
  # Add dollar signs for LaTeX math mode
  latex_output = paste0("$$", latex_string, "$$")
  return(latex_output)
}

#test = create_beta_list_names(c(2, 3), text = "b")
#beta_list_latex(test)
