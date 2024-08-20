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
  #' Calculate Index Positions for Each Level
  #' @param i An integer representing the current index for which the 
  #' indices are to be calculated.
  #' @param L An integer vector where each element represents the 
  #' number of levels for a particular factor.
  
  indices = numeric(length(L))  # Initialize a numeric vector to store the indices
  remaining = i - 1  # Adjust index to 0-based for easier calculation
  
  for(j in 1:length(L)) {
    indices[j] = (remaining %% L[j]) + 1  # Calculate the current level index (remainder)
    remaining = (remaining %/% L[j])  # Update remaining for the next level (quotient)
  }
  
  return(indices)  # Return the vector of calculated indices
}

create_beta_list_names = function(levels, text = "b"){
  #' Generate Beta List Names for a Beta Matrix
  #' @param levels An integer vector where each element represents the number 
  #' of levels for a corresponding factor.
  #' @param text indicates the initials before the indices.
  #' @examples
  #' >generate_beta_vector(c(2,3,3))
  #' [1] "b111" "b112" "b113" "b121" "b122" "b123" "b131" "b132" "b133" "b211" "b212" "b213" "b221"
  #' [14] "b222" "b223" "b231" "b232" "b233
  
  Lprod = prod(levels)  # Calculate the total number of combinations (product of levels)
  
  # Generate beta vector by calculating indices for each combination
  beta_vector = sapply(1:Lprod, function(i){
    # Calculate the indices for the current combination
    indices = calculate_indices(i, rev(levels))  
    # Create the beta name by concatenating indices
    paste(text, paste(rev(indices), collapse = ""), sep = "")  
  })
  
  return(beta_vector)  # Return the vector of beta names
}

find_position = function(value, beta_vector){
  #' This function searches for a specific value within a vector of beta values 
  #' and returns the position (index) of that value in the vector. 
  #' @param value A character string representing the beta value of interest. 
  #' This is the value you want to locate in the beta_vector.
  #' @param beta_vector A character vector containing the beta values. 
  #' The function will search through this vector to find the position of value.
  #' @examples 
  #' > beta_vector = c("b111" "b112" "b113" "b121", "1222")
  #' > find_position("b113", beta_vector)
  #' [1] 3
  position = match(value, beta_vector)
  return(position)
}

beta_list_latex = function(structure, between = ", \\quad", end = "$$"){
  #' Assumption: user used create_beta_list_names first.
  #' @param structure is the character containing the beta expression.
  
  # Convert to LaTeX format
  latex_string = paste0("\\beta_{", substring(structure, 2), "}", collapse = between)
  
  # Add dollar signs for LaTeX math mode
  latex_output = paste0(end, latex_string, end)
  return(latex_output)
}

#test = create_beta_list_names(c(2, 3), text = "b")

