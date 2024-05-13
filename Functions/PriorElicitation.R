################################################################
# ELICITING FROM THE PRIOR                                     #
################################################################

elicit_prior_sigma_function = function(p, gamma, s1, s2, upper_bd, lower_bd){
  #' 1/sigma^2 ~ gamma(alpha01, alpha02). Here, we elicit the prior for sigma.
  #' We must specify s1, s2 such that s1 <= sigma * z_{(1+gamma)/2} <= s2
  #' Then the values alpha01, alpha02 must be solved for:
  #' Gamma(alpha01, 1, alpha02 * z^{2}_{(1+p)/2}/s^{2}_{1}) = (1+gamma)/2
  #' Gamma(alpha01, 1, alpha02 * z^{2}_{(1+p)/2}/s^{2}_{2}) = (1-gamma)/2
  #' Using the bisection method.
  #' @param p represents the number of dimensions.
  #' @param gamma represents the virtual uncertainty.
  vectors_of_interest = list(s1, s2, upper_bd, lower_bd)
  for(i in vectors_of_interest){
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
    eps = .0001
    maxits = 100
    alphalow = lower_bd[j]
    alphaup = upper_bd[j]
    for (i in 1:maxits){
      alpha = (alphalow + alphaup)/2
      beta = qgamma(gam, alpha, 1)/upbdinvsigma2[j]
      test = pgamma(beta*lwbdinvsigma2[j], alpha, 1)
      if (abs(test-(1-gam)) <= eps) { break }
      if (test < 1 - gam) { alphaup = alpha} 
      else if (test > 1 - gam) { alphalow = alpha }
    }
    alpha01[j] = alpha
    alpha02[j] = beta
  }
  
  newlist = list("lwbdinvsigma2" = lwbdinvsigma2, "upbdinvsigma2" = upbdinvsigma2, 
                 "alpha01" = alpha01, "alpha02" = alpha02, 
                 "z0" = z0, "s1" = s1, "s2" = s2)
  return(newlist)
}

elicit_prior_mu_function = function(p, gamma, m1, m2, s1, s2, alpha01, alpha02){
  #' We elicit the prior for mu, which is given by:
  #' mu ~ mu0 + sqrt(alpha02/alpha01) (lambda0) (t_{2*alpha01}) 
  #' @param p represents the number of dimensions.
  #' @param gamma represents the virtual uncertainty.
  vectors_of_interest = list(m1, m2, s1, s2, alpha01, alpha02)
  for(i in vectors_of_interest){
    if(length(i) != p){ return("Error: there is a vector that doesn't have length p.") }
  }
  mu0 = (m1 + m2)/2 
  gam = (1+gamma)/2
  lambda0 = (m2 - m1)/(2 * sqrt(alpha02/alpha01) * qt(gam, df = 2 * alpha01))
  
  newlist = list("mu0" = mu0, "lambda0" = lambda0, "m1" = m1, "m2" = m2)
  return(newlist)
}

# note: may remove the function shown below.
elicit_prior_effective_range = function(p, m = 200, alpha01, alpha02, mu0, lambda0, 
                                        x_low, quantile_val = c(0.005, 0.995)){
  #' Computes the effective range from the true prior.
  #' @param p represents the number of dimensions.
  #' @param m represents the number of desired sub-intervals for the effective range.
  #' @param quantile_val represents a vector of size two where the first value denotes the 
  #' smaller quantile, and the second denotes the larger quantile. 
  #' @param x_low denotes the initation of where the search begins to find the effective range.
  #' The other parameters match the descriptions in section 2.1.
  #' @details 
  #' This function assumes that m is consistent throughout each mu. 
  desired_range = diff(quantile_val)
  
  x_range_list = vector("list", length = p)
  y_range_list = vector("list", length = p)
  delta_vector = numeric(p)
  grid_list = vector("list", length = p)
  
  for(k in 1:p){
    if (x_low < 0) {x = seq(x_low, -x_low, by = 0.02)}
    else {x_low = seq(x_low, x_low*2, by = 0.02)}
    y = dt(x,2*alpha01[k])
    scale = sqrt(alpha02[k]/alpha01[k])*lambda0[k]
    xnew = mu0[k] + scale*x
    ynew = y/scale
    # now computing the actual effective range
    x_center = which.min(abs(xnew-(mu0[k])))
    # computing the area underneath
    i = 1
    found_range = FALSE
    while(found_range == FALSE){
      x_low_range = x_center - i
      x_high_range = x_center + i
      x_area = xnew[x_low_range:x_high_range]
      y_area = ynew[x_low_range:x_high_range]
      area = trapz(x_area, y_area)
      if(area >= desired_range){
        found_range = TRUE
        x_range = x_area
        y_range = y_area
      } else {
        i = i + 1
      }
    }
    
    # creating new grid points based off of the effective range
    delta = (x_range[length(x_range)] - x_range[1])/m # length of the sub intervals
    x_grid = seq(x_range[1], x_range[length(x_range)], by = delta) # constructing the new grid
    
    x_range_list[[k]] = x_range
    y_range_list[[k]] = y_range
    delta_vector[k] = delta
    grid_list[[k]] = x_grid
  }
  newlist = list("x_range" = x_range_list, "y_range" = y_range_list,
                 "delta" = delta_vector, "grid" = grid_list)
  return(newlist)
}