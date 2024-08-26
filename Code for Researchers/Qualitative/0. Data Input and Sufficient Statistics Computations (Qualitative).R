#############################################################
# Part 0: Data Input and Sufficient Statistics Computations #
#############################################################
#August 26, 2024

#If you haven't installed any of the packages yet, comment out below.
#install.packages("MASS")
#install.packages("plyr")
library(MASS) # Used for mvrnorm (generating from a multinormal dist)
library(plyr) # for rounding

# setting the seed so that computations can be replicated provided all relevant parts 
# are run consecutively
set.seed(1)

########################################################################################
#A Here is the code to generate some test data. To input data from a file see B below. #

l = c(2, 3) # input the number of levels per factor
m = length(l) # the number of factors.

k = prod(l) # letting this denote the possible number of combinations between the crossed factors 
n_vector = c(5, 5, 5, 5, 5, 5) # the length of n should be equal to k (if factors are crossed) 
n = sum(n_vector)

# Generating Y
mu = c(2, 4, 6, 8, 10, 12)
sigma = 2
Y = numeric(n)
for(i in 1:k){
  if(i == 1){
    Y[1:n[i]] = rnorm(n_vector[i], mean = mu[i], sd = sigma)
  } else {
    Y[(cumsum(n_vector)[i-1]+1):cumsum(n_vector)[i]] = rnorm(n_vector[i], mean = mu[i], sd = sigma)
  }
}

#################################################################################

######################################################################################
#B Here is the code to input data from a file. Use the code at A if using test data. #

# input the data into a_data nxp data matrix Y 
# the data is in a .csv file stored in a directory specified by the user
# use setwd to access this directory

setwd("C:/Users/AnnaH/OneDrive/Desktop/Stats RA/New Project/ImportanceSamplingMultivariateNormal/Code for Researchers/Qualitative")

data = read.csv("qual_Y_example.csv")

Y = data$Y
l = data$l[!is.na(data$l)]
m = length(l)
k = prod(l)
n_vector = data$n_vector[!is.na(data$n_vector)]
n = sum(n_vector)

######################################################################################
# Here you can set up the overall contrast matrix C or use the default which is the 
# Kronecker product of Helmert matrices.

#C = matrix(c(1, -1, -1, -1,  1,  1,
#            1,  1, -1, -1, -1,  1, 
#            1,  0,  2, -1,  0, -2, 
#            1, -1, -1,  1, -1, -1,
#            1,  1, -1,  1,  1, -1, 
#            1,  0,  2,  1,  0,  2), 
#           nrow = 6, ncol = 6, byrow = TRUE)

# You can also input individual contrast matrices C_{1}, ..., C_{m} and form the overall
# contrast matrix, where C_{i} \in \mathbb{R}^{[L_{i}] x [L_{i}]} with first columns all 
# mutually orthogonal.

#C1 = matrix(c(1, -1, 1, 1), nrow = 2, ncol = 2, byrow = TRUE)

#C2 = matrix(c(1, -1, -1, 1, 1, -1, 1, 0, 2), nrow = 3, ncol = 3, byrow = TRUE)

# C = C1 %x% C2 # This is the overall contrast matrix (using the kronecker product)

# If you don't know what to use, there is code below to generate one from
# the tensor product of Helmert matrices.

# The following function builds the default default contrast matrix C
qual_C_matrix = function(m, l){
  #' Form a contrast matrix C generate from a is tensor product of
  #' Helmert matrices.
  #' @param m represents the number of factors.
  #' @param l a vector that contains the number of levels per factor.
  C = 1
  for(i in 1:m){
    Ci = cbind(rep(1, l[i]), contr.helmert(l[i])) # dropped the normalization
    print(Ci)
    C = C %x% Ci # kronecker product
  }
  return(C)
}

results2 = qual_C_matrix(m, l)

C = results2

################################################################################
# Here is the code for computing X given the information above                 #
################################################################################

qual_generate_X = function(n_vector){
  #' Given the sample size per each combination, generates the X matrix.
  #' @param n a vector, which is the sample size per combination.
  #' @examples
  #' > qual_generate_X(c(1, 2, 1, 4))
  #'      [,1] [,2] [,3] [,4]
  #' [1,]    1    0    0    0
  #' [2,]    0    1    0    0
  #' [3,]    0    1    0    0
  #' [4,]    0    0    1    0
  #' [5,]    0    0    0    1
  #' [6,]    0    0    0    1
  #' [7,]    0    0    0    1
  #' [8,]    0    0    0    1
  X = matrix(0, nrow = length(n_vector), ncol = sum(n_vector))
  
  for(i in 1:length(n_vector)) {
    if(i == 1) {
      X[i, 1:n_vector[i]] = 1
    } else {
      start_idx = cumsum(n_vector)[i-1] + 1
      end_idx = cumsum(n_vector)[i]
      X[i, start_idx:end_idx] = 1
    }
  }
  return(t(X))
}

X = qual_generate_X(n_vector)

##########################################################################
# Here is the code to compute the minimal sufficient statistics: b, s^2 #                                                #
##########################################################################

qual_Y_minimal_suff_stat = function(X, Y){
  #' Given the values of the Y vector and the X matrix, computes the minimal
  #' sufficient statistics (b and s^2).
  #' @param X a matrix.
  #' @param Y a vector containing the data.
  b = solve(t(X) %*% X) %*% t(X) %*% Y
  s_2 = t(Y - X %*% b) %*% (Y - X %*% b)
  newlist = list("b" = b, "s_2" = s_2)
  return(newlist)
}

results1 = qual_Y_minimal_suff_stat(X, Y)

b = results1$b
s_2 = results1$s_2

#####################################################################################
# The order (indices) of beta 

# We treat the values of beta as a vector instead of a matrix. This displays the order
# of the beta's within the vector. So index i correspond to same (j_{1}, ..., j_{m}) for
# beta_{j_{1}, ..., j_{m}} for beta_{j_{1}, ..., j_{m}}

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
    # rev: reverses the order
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

# Getting the order of the betas in which the information is presented
beta_list = create_beta_list_names(levels = l)

df = data.frame(beta = beta_list)

print(df)

# tells you the position of each beta value. Below is just an example
find_position(value = "b13", beta_vector = beta_list)


