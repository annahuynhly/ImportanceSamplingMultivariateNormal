
closed_bracket_grid = function(delta){
  #' Creates a sequence of values from 0 to 1 (inclusive) based on 
  #' the distance between two points.
  #' @param delta represents the distance between two points.
  grid = seq(0, 1, length= (1/delta)+1)
  return(grid)
}

open_bracket_grid = function(delta){
  #' Creates a grid of values from 0 to 1 (not inclusive) based on 
  #' the distance between two points.
  #' @param delta represents the distance between two points.
  grid = seq(delta/2, 1 - delta/2, length=(1/delta))
  return(grid)
}

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

average_vector_values = function(vector, num_average_pts = 3){
  #' Generates a new vector by averaging the values of a given vector based on the proximity 
  #' of each element to its neighbors.
  #' @param vector The input vector to be smoothed.
  #' @param num_average_pts The number of neighboring points to consider when calculating 
  #' the average for each element. Only the odd case is implemented.
  if(num_average_pts %% 2 == 0){
    return("Error: num_average_pts must be an odd number.")
  }
  
  if(num_average_pts == 1){
    return(vector)
  } 
  new_vector = rep(0, length(vector))
  
  pts = 0
  num_neighbours = floor(num_average_pts/2)
  for(i in 1:length(vector)){
    if(i <= num_neighbours | (length(vector) - i) < num_neighbours){ # Edge points case
      if(i == 1 | i == length(vector)){
        new_vector[i] = vector[i]
      } else {
        if (i <= num_neighbours){
          pts = i - 1
        } else if ((length(vector) - i) < num_neighbours){
          pts = length(vector) - i
        }
        new_vector[i] = sum(vector[(i-pts):(i+pts)])/(2*pts + 1)
      }
    } else {
      lower_int = i - num_neighbours
      upper_int = i + num_neighbours
      new_vector[i] = sum(vector[lower_int:upper_int])/(2*num_neighbours + 1)
    }
  }
  
  return(new_vector)
}

seq_alt = function(values, delta){
  #' Generate an alternative sequence of values based on the input vector and the distance between
  #' two points. If the maximum value is not already present in the sequence, it is added to the end.
  #' @param values represents the numerical vector for which an alternative sequence is generated.
  #' @param delta represents the distance between two points.
  min = floor(min(values))
  max = ceiling(max(values))
  grid = seq(min, max, by = delta)
  if(!(max %in% grid) == TRUE){
    grid = c(grid, max)
  }
  return(grid)
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

divison_alt = function(num, denom){
  #' An alternative method for division. When a denominator is zero, the 
  #' corresponding result is set to NaN instead of raising an error.
  #' @param num the numerator.
  #' @param denom the denominator.
  #' @details
  #' The function assumes that the length of 'num' is equal to the length of 'denom'.
  x = c()
  for(i in 1:length(num)){
    if(denom[i] == 0){
      x = c(x, NaN)
    } else {
      x = c(x, num[i]/denom[i])
    }
  }
  return(x)
}

force_bounds_0 = function(v){
  #' Regardless of the first and last value of a vector, replaces it with zero.
  #' This is mostly for creating polygons in graphs.
  #' @param represents the vector.
  v = v[2:(length(v)-1)]
  v = c(0, v, 0)
  return(v)
}