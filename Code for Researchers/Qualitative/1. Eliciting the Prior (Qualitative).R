#####################################
# Part 1: Elicitation of the Prior  #
#####################################
#August 26, 2024

p = 1 # The data here is univariate.
gamma = 0.99

###########################################################
#A Here is the code to input for the elicitation manually #
# or use the code at B to read from a file                #

# s1 and s2 are elicited bounds as discussed in the paper to determine the prior on 1/sigma^2
s1 = c(1)
s2 = c(7)
# lower_bd, upper_bd are initial bounds on alpha01 for iterative procedure and may need to
# be modified for the iteration to work.
lower_bd = c(0)
upper_bd = c(50)
# m1, m2 are elicited bounds as in the paper to determine the priors on the means.
m1 = c(0, 2, 4, 6, 8, 10)
m2 = c(4, 6, 8, 10, 12, 14)

############################################################

###########################################################################
#B Here is the code to read the input for the elicitation                 #
# from a file or use the code at A to input this manually                 #
# the data is in a .csv file stored in a directory specified by the user  #
# use setwd to access this directory

setwd("C:/Users/AnnaH/OneDrive/Desktop/Stats RA/New Project/ImportanceSamplingMultivariateNormal/Code for Researchers/Qualitative")
elicit_data = read.csv("qual_elicit_example.csv", sep = ",")

# s1 and s2 are elicited bounds as discussed in the paper.
s1 = elicit_data$s1[!is.na(elicit_data$s1)]
s2 = elicit_data$s2[!is.na(elicit_data$s2)]
# lower_bd, upper_bd are initial bounds on alpha01 for iteration and may need to
# be modified for the iteration to work.
lower_bd = elicit_data$lower_bd[!is.na(elicit_data$lower_bd)]
upper_bd = elicit_data$upper_bd[!is.na(elicit_data$upper_bd)]
# m1, m2 are elicited bounds as in the paper.
m1 = elicit_data$m1
m2 = elicit_data$m2

############################################################

# Functions ######################################

# Set eps and maxits for the iterative procedure to determine if the hyperparameters for the prior
# on 1/sigma^{2}; if maxits is too small convergence will fail.

eps = 0.0001
maxits = 100

elicit_prior_sigma_function = function(p, gamma, s1, s2, upper_bd, lower_bd, eps, maxits){
  #' 1/sigma^2 ~ gamma(alpha01, alpha02). Here, we elicit the prior for sigma.
  #' We specify s1, s2 such that s1 <= sigma * z_{(1+gamma)/2} <= s2
  #' Then the values alpha01, alpha02 are solved for:
  #' Gamma(alpha01, 1, alpha02 * z^{2}_{(1+gamma)/2}/s^{2}_{1}) = (1+gamma)/2
  #' Gamma(alpha01, 1, alpha02 * z^{2}_{(1+gamma)/2}/s^{2}_{2}) = (1-gamma)/2
  #' Using the bisection method.
  #' @param p represents the number of dimensions.
  #' @param gamma represents the virtual certainty.
  sigmabounds_of_interest = list(s1, s2, upper_bd, lower_bd)
  for(i in sigmabounds_of_interest){
    if(length(i) != p){return("Error: there is a vector that doesn't have length p.")}
  }
  
  gam = (1+gamma)/2
  z0 = qnorm(gam,0,1)
  upbdinvsigma2 = (z0/s1)**2 
  lwbdinvsigma2 = (z0/s2)**2 
  
  alpha01 = numeric(p) 
  alpha02 = numeric(p)
  
  for(j in 1:p){
    # iterate until prob content of s1<= sigma*z0 <= s2 is within eps of p 
    alphalow = lower_bd[j]
    alphaup = upper_bd[j]
    for (i in 1:maxits){
      alpha = (alphalow + alphaup)/2
      beta = qgamma(gam, alpha, 1)/upbdinvsigma2[j]
      test = pgamma(beta*lwbdinvsigma2[j], alpha, 1)
      if (abs(test-(1-gam)) <= eps) { break }
      if (test < 1 - gam) { alphaup = alpha} 
      else if (test > 1 - gam) { alphalow = alpha }
      if(i == maxits){
        cat("Convergence failed", "\n")
      }
    }
    alpha01[j] = alpha
    alpha02[j] = beta
  }
  
  newlist = list("lwbdinvsigma2" = lwbdinvsigma2, "upbdinvsigma2" = upbdinvsigma2, 
                 "alpha01" = alpha01, "alpha02" = alpha02, 
                 "z0" = z0, "s1" = s1, "s2" = s2)
  return(newlist)
}

elicit_prior_beta0_function = function(p, gamma, m1, m2, alpha01, alpha02){
  #' We elicit the hyperparameters beta0 and lambda0.
  #' @param p represents the number of dimensions.
  #' @param gamma represents virtual certainty.
  #' @param m1 represents the lower bound for beta0.
  #' @param m2 represents the upper bound for beta0.
  #' @param alpha01 a vector generated from sigma, along with alpha02.
  
  beta0 = (m1 + m2)/2 
  gam = (1+gamma)/2
  lambda0 = (m2 - m1)/(2 * sqrt(alpha02/alpha01) * qt(gam, df = 2 * alpha01))
  
  newlist = list("beta0" = beta0, "lambda0" = lambda0, "m1" = m1, "m2" = m2)
  return(newlist)
}

# Values #########################################

prior_sigma_vals = elicit_prior_sigma_function(p, gamma, s1, s2, upper_bd, lower_bd, eps, maxits)

prior_sigma_vals

alpha01 = prior_sigma_vals$alpha01
alpha02 = prior_sigma_vals$alpha02

z0 = prior_sigma_vals$z0 # this is the (1+gamma)/2 quantile of the standard normal dist.

lwbdinvsigma2 = prior_sigma_vals$lwbdinvsigma2 # This is (z0/s2)^2
upbdinvsigma2 = prior_sigma_vals$upbdinvsigma2 # This is (z0/s1)^2

prior_beta0_vals = elicit_prior_beta0_function(p, gamma, m1, m2, alpha01, alpha02)

lambda0 = prior_beta0_vals$lambda0
beta0 = prior_beta0_vals$beta0
