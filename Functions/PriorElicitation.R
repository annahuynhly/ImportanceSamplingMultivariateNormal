################################################################
# ELICITING FROM THE PRIOR                                     #
################################################################

elicit_prior_sigma_function = function(p, gamma, s1, s2, upper_bd, lower_bd){
  # p: the number of dimensions (user needs to manually insert)
  # gamma: probability corresponding to virtual certainty
  # s1: defined in the paper lol
  # s2: defined in the paper lol
  # upper_bd: lower bound of the iteration for alpha0i
  # lower_bd: upper bound of the iteration for alpha0i
  
  vectors_of_interest = list(s1, s2, upper_bd, lower_bd)
  for(i in vectors_of_interest){
    if(length(i) != p){
      return("Error: there is a vector that doesn't have length p.")
    }
  }
  
  gam = (1+gamma)/2
  z0 = qnorm(gam,0,1)
  c1 = (z0/s1)**2
  c2 = (z0/s2)**2
  
  alpha01 = numeric()
  alpha02 = numeric()
  
  for(j in 1:p){
    # iterate until prob content of s1<= sigma*z0 <= s2 is within eps of p 
    eps = .0001
    maxits = 100
    alphalow = lower_bd[j]
    alphaup = upper_bd[j]
    for (i in 1:maxits){
      alpha = (alphalow + alphaup)/2
      beta = qgamma(gam, alpha, 1)/c1[j]
      test = pgamma(beta*c2[j], alpha, 1)
      if (abs(test-(1-gam)) <= eps) {
        break
      }
      if(test < 1 - gam){
        alphaup = alpha
      } else if (test > 1 - gam){ # later: see if the else if is causing an error to make the code efficient
        alphalow = alpha
      }
    }
    alpha01[j] = alpha
    alpha02[j] = beta
  }
  
  newlist = list("c1" = c1, "c2" = c2, "alpha01" = alpha01, "alpha02" = alpha02, "z0" = z0,
                 "s1" = s1, "s2" = s2)
  return(newlist)
}

elicit_prior_mu_function = function(p, gamma, m1, m2, s1, s2, alpha01, alpha02){
 
  vectors_of_interest = list(m1, m2, s1, s2, alpha01, alpha02)
  for(i in vectors_of_interest){
    if(length(i) != p){
      return("Error: there is a vector that doesn't have length p.")
    }
  }
  mu0 = (m1 + m2)/2 # multivariate mu_0
  
  sigma0 = (m2 - m1)/(2 * s2)
  lambda0 = (m2 - m1)/(2 * sqrt(alpha02/alpha01) * qt((1 + gamma)/2, df = 2 * alpha01))
  
  mu = rep(0, p)
  for(i in 1:p){
    t_val = qt(gamma, df = alpha01[i] * 2)
    mu[i] = mu0[i] + sqrt(alpha02[i]/alpha01[i]) * lambda0[i] * t_val
  }
  
  newlist = list("mu0" = mu0, "sigma0" = sigma0, "lambda0" = lambda0,
                 "mu" = mu)
  return(newlist)
}


################################################################
# DISCARDED CODE (kept for rough)                              #
################################################################

# note: for the first page, may need to fix m1 and m2 temporarily to get the graph
# before loading the new section. (or replace it into two functions later)
elicit_prior = function(p, gamma, m1, m2, s1, s2, upper_bd, lower_bd){
  # p: the number of dimensions (user needs to manually insert)
  # gamma: probability corresponding to virtual certainty
  # s1: defined in the paper lol
  # s2: defined in the paper lol
  # upper_bd: lower bound of the iteration for alpha0i
  # lower_bd: upper bound of the iteration for alpha0i
  
  vectors_of_interest = list(m1, m2, s1, s2, upper_bd, lower_bd)
  for(i in new_col){
    if(length(i) != p){
      return("Error: there is a vector that doesn't have length p.")
    }
  }
  
  mu0 = (m1+m2)/2 # multivariate mu_0
  
  # note: add another condition to check to ensure the rest of the lengths are the same.
  # will add later.
  gam = (1+gamma)/2
  z0 = qnorm(gam,0,1)
  c1 = (z0/s1)**2
  c2 = (z0/s2)**2
  
  alpha01 = numeric()
  alpha02 = numeric()
  
  for(j in 1:p){
    # iterate until prob content of s1<= sigma*z0 <= s2 is within eps of p 
    eps = .0001
    maxits = 100
    alphalow = lower_bd[j]
    alphaup = upper_bd[j]
    for (i in 1:maxits){
      alpha = (alphalow + alphaup)/2
      beta = qgamma(gam, alpha, 1)/c1[j]
      test = pgamma(beta*c2[j], alpha, 1)
      if (abs(test-(1-gam)) <= eps) {
        break
      }
      if(test < 1 - gam){
        alphaup = alpha
      } else if (test > 1 - gam){ # later: see if the else if is causing an error to make the code efficient
        alphalow = alpha
      }
    }
    alpha01[j] = alpha
    alpha02[j] = beta
  }
  
  sigma0 = (m2 - m1)/(2*s2)
  lambda0 = (m2 - m1)/(2 * sqrt(alpha02/alpha01) * qt((1 + gamma)/2, df = 2 * alpha01))
  
  newlist = list("c1" = c1, "c2" = c2, "alpha01" = alpha01, "alpha02" = alpha02,
                 "mu0" = mu0, "sigma0" = sigma0, "lambda0" = lambda0, "z0" = z0)
  return(newlist)
}






elicit_mu = function(alpha, beta, gamma, s1, s2){
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

prior_elicitation_mu = function(gamma, m1, m2, const = FALSE, s1 = FALSE, s2 = FALSE){
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
      
      fx = elicit_mu(x[1,j], x[2,j], gamma, s1[j], s2[j])
      Jx = matrix(0,2,2)
      
      for(i in 1:2){
        xh = x # estimation values
        xh[i,j] = x[i,j] + h
        
        fxh = elicit_mu(xh[1,j], xh[2,j], gamma, s1[j], s2[j]) # estimated
        Jx[,i] = (fxh - fx)/h 
      }
      
      delta[,j] = -solve(Jx) %*% elicit_mu(x[1,j], x[2,j], gamma, s1[j], s2[j])  
      delta0[,1] = delta[,j]
      
      x[,j] = x[,j] + delta[,j]      
      iter = iter + 1   
    }
    j = j+1
    delta0 = matrix(c(alphas[j],betas[j]), ncol=1)
  }
  
  sigma0 = (m2 - m1)/(2*s1)
  
  newlist = list("alphas" = x[1,], "betas" = x[2,], "s1" = s1, "s2" = s2,
                 "mu0" = mu0, "sigma0" = sigma0)
  return(newlist)
}

# testing.
#x = elicit_prior(gamma = 0.999, 
#             m1 = c(-5, -5, -5), 
#             m2 = c(5, 5, 5), 
#           s1 = c(2, 2, 2), 
#             s2 = c(10, 10, 10), 
#             alphaup = c(0, 0, 0), 
#             alphalow = c(50, 50, 50))

#alpha = test$alpha
#beta = test$beta
#low = test$low
#up = test$up
#x=low+(up-low)*c(0:1000)/1000
#x3=sqrt(1/x)
#dens3=2*(x^(3/2))*dgamma(x,alpha,beta)
#plot(x3,dens3,xlab="sigma",ylab="prior density",type="l")







