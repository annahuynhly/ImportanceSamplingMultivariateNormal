convert_to_hex = function(hex_colour){
  #' Given a hex colour code, converts it into an interpretable format.
  #' @param hex_colour represents the hex colour code.
  #' @examples
  #' convert_to_hex(" FFFFFF  ")
  #' # Returns: [1] "#FFFFFF"
  hex_colour = gsub(" ", "", hex_colour)
  first_char = substr(hex_colour, 1, 1)
  if(first_char != "#"){
    return(paste("#", hex_colour, sep = ""))
  } else {
    return(hex_colour)
  }
}

convert_char_to_vector = function(x){
  #' Given a character that contains a list of objects, converts 
  #' it into a vector.
  #' @examples
  #' convert_char_to_vector("1, 2, 3")
  #' # Returns: [1] 1 2 3
  if (is.character(x) == FALSE){
    return("Invalid input: the input is not a character.")
  }
  x = str_replace_all(x, fixed(" "), "") # removes all spaces
  x = (strsplit(x, ",")[[1]])
  
  check_numeric_count = 0
  for(i in 1:length(x)){
    if(check.numeric(x[i])){
      check_numeric_count = check_numeric_count + 1
    }
  }
  if (check_numeric_count == length(x)){
    x = as.numeric(x) # converts to vector
    x = x[!is.na(x)]
    return(as.double(x))
  } else {
    return("Invalid vector. Not all numbers are numeric.")
  }
}

valid_numeric_vector = function(x){
  #' Given a vector, determines whether is a vector that only contains numerical
  #' values. 
  #' @param x represents the vector that is to be examined.
  #' @examples
  #' valid_numeric_vector(c(1, 2, 3))
  #' # Returns: [1] TRUE
  #'
  #' valid_numeric_vector(c(1, 2, "2"))
  #' # Returns: [1] FALSE
  for (i in 1:length(x)) {
    if (is.numeric(x[[i]]) == FALSE){ 
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
}

create_necessary_vector = function(x){
  #' Accepts input that may be either a character (capable of being converted into a vector) 
  #' or an existing vector. 
  #' If the input is a character, this function transforms the value into a vector. 
  #' If the input is already a vector, the function returns the vector as is.
  #' @param x represents the input which may be a character or a vector.
  #' @examples
  #' create_necessary_vector("1, 1, 1, 1")
  #' # Returns: [1] 1 1 1 1
  #'
  #' create_necessary_vector(c(1, 1, 1, 1))
  #' # Returns: [1] 1 1 1 1
  if(is.character(x) == TRUE){
    return(convert_char_to_vector(x))
  } else if (typeof(x) == "double" | valid_numeric_vector(x) == TRUE){
    return(x)
  } else {
    return("Invalid vector.")
  }
}

find_inverse_alt = function(matrix){
  #' Computes the inverse of a matrix using an alternative method that 
  #' preserves positive-definiteness.
  #' @param matrix represents the matrix that is being inputted. 
  x = eigen(matrix, symmetric = TRUE, only.values=FALSE)
  Q = x$vectors
  V_inv = diag(1/x$values)
  B = Q%*%sqrt(V_inv)
  inverse_matrix = B%*%t(B)
  return(inverse_matrix)
}