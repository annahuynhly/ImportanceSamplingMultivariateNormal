#############################################################
# Part 0: Data Input and Sufficient Statistics Computations #
#############################################################
#July 20, 2024

#If you haven't installed any of the packages yet, comment out below.
#install.packages("MASS")
#install.packages("plyr)
library(MASS) # Used for mvrnorm (generating from a multinormal dist)
library(plyr) # for rounding

# setting the seed so that computations can be repeated provided all relevant parts are run consecutively
set.seed(1)

########################################################################################
#A Here is the code to generate some test data. To input data from a file see B below. #

l = c(2, 3) # input the number of levels per factor
m = length(l) # the number of factors.

k = prod(l) # letting this denote the possible number of combinations between the crossed factors 
n_vector = c(5, 5, 5, 5, 5, 5) # the size of n should be equal to k (if factors are crossed) 
n = sum(n_vector)

# Generating Y
mu = c(2, 4, 6, 8, 10, 12)
Y = numeric(n)
for(i in 1:k){
  if(i == 1){
    Y[1:n[i]] = rnorm(n_vector[i], mean = mu[i], sd = 2)
  } else {
    Y[(cumsum(n_vector)[i-1]+1):cumsum(n_vector)[i]] = rnorm(n_vector[i], mean = mu[i], sd = 2)
  }
}

#################################################################################

######################################################################################
#B Here is the code to input data from a file. Use the code at A if using test data. #

# input the data into a_data nxp data matrix Y 
# the data is in a .csv file stored in a directory specified by the user
# use setwd to access this directory

# TODO: NEED TO IMPLEMENT LATER.


#####################################################################################

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

################################################################################
# Here is the code to compute the minimal sufficient statistics: b, S^2, and C #
################################################################################

qual_Y_metrics = function(X, Y){
  #' Given the values of the Y vector and the X matrix, computes the minimal
  #' sufficient statistics (b, s^2, and C).
  #' @param X a matrix.
  #' @param Y a vector containing the data.
  #' @param m represents the number of factors.
  #' @param l a vector that contains the number of levels per factor.
  b = solve(t(X) %*% X) %*% t(X) %*% Y
  s_2 = t(Y - X %*% b) %*% (Y - X %*% b)
  C = 1
  for(i in 1:m){
    Ci = cbind(rep(1, l[i]), contr.helmert(l[i])) # dropped the normalization
    C = C %x% Ci # kronecker product
  }
  newlist = list("b" = b, "s_2" = s_2, "C" = C)
  return(newlist)
}

results = qual_Y_metrics(X, Y)

b = results$b
s_2 = results$s_2
C = results$C
