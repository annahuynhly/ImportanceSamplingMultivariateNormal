rm(list=ls(all=TRUE))

# note: outdated; not latest version.

library(MASS)
library(mvtnorm)

# BELOW ARE USER INPUTS (all vectors must be of length p)
m1 = c(-5, -5, -5)
m2 = c(5, 5, 5)

# range for upper s2, delta_i need to be contained in this range 
const = c(2,2,2) 
# might ask them or generate ourselves; give them the options.
s1=c(2,2,2)
s2=c(4,4,4)

gamma = 0.999 #virtual certainty  


elicit = function(alpha, beta, gamma, s1, s2){
  # This function finds the two values of gamma which we are subtracting.
  
  prob1 = (1+gamma)/2
  prob2 = (1-gamma)/2
  G = matrix(0,2,1)
  
  G[1] = qgamma(prob1, alpha, beta) - (qnorm(prob1)/s1)^2
  G[2] = qgamma(prob2, alpha, beta) - (qnorm(prob1)/s2)^2
  
  return(G)  
}

generate_samp_var = function(gamma, p, const, s1, s2){
  # this checks whether the user inputs a valid s1, s2 or whether they just placed
  # constraints.
  if(is.numeric(const) == FALSE & is.numeric(s1) == TRUE & is.numeric(s2) == TRUE){
    if(length(s1) == length(s2)){
      return(list("s1" = s1, "s2" = s2))
    } else {
      return("Error")
    }
  } else if (is.numeric(const) == TRUE & is.numeric(s1) == FALSE & is.numeric(s2) == FALSE){
    s1 = numeric()           
    s2 = numeric()
    
    for (i in 1:p){
      s1 = c(s1, qnorm((1+gamma)/2))
      s2 = c(s2, const[i]*(qnorm((1+gamma)/2)))
    }
    return(list("s1" = s1, "s2" = s2))
  } else {
    return("Error")
  }
}

prior_elicitation = function(gamma, m1, m2, const = FALSE, s1 = FALSE, s2 = FALSE){
  # algorithm doesn't work with a poor choice of s1 and s2; keep this here, but
  # remove options for the user.
  if(length(m1) == length(m2)){
    p = length(m1)
  } else {
    return("Error: length of m1, m2 are not equal.")
  }
  mu0 = (m1+m2)/2 # multivariate mu_0
  
  s = generate_samp_var(gamma, p, const, s1, s2)
  if(s[1] != "Error"){
    s1 = s$s1
    s2 = s$s2
  } else {
    return("Error: cannot determine the values for s1, s2.")
  }
  
  alphas = matrix(rep(1, p), nrow=1) 
  betas = matrix(rep(1, p), nrow=1)          
  x = as.matrix(rbind(alphas, betas))
  delta = x
  
  j = 1
  # initializing
  delta0 = matrix(c(alphas[1],betas[1]), ncol=1)
  while(j <= p){
    maxiter = 1000   
    iter = 0
    tol = 0.5 
    
    while (norm(delta0) > tol & iter < maxiter){
      h = 0.000000001 # increase point
      
      fx = elicit(x[1,j], x[2,j], gamma, s1[j], s2[j])
      Jx = matrix(0,2,2)
      
      for(i in 1:2){
        xh = x # estimation values
        xh[i,j] = x[i,j] + h
        
        fxh = elicit(xh[1,j], xh[2,j], gamma, s1[j], s2[j]) # estimated
        Jx[,i] = (fxh - fx)/h 
      }
      
      delta[,j] = -solve(Jx) %*% elicit(x[1,j], x[2,j], gamma, s1[j], s2[j])  
      delta0[,1] = delta[,j]
      
      x[,j] = x[,j] + delta[,j]      
      iter = iter + 1   
    }
    j = j+1
    delta0 = matrix(c(alphas[j],betas[j]), ncol=1)
  }
  
  newlist = list("alphas" = x[1,], "betas" = x[2,], "s1" = s1, "s2" = s2)
  return(newlist)
}

