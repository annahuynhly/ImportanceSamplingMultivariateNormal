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
    if (coeff > 0) {
      if(i == 1){
        terms[i] = beta
      } else {
        terms[i] = paste0("+ ", beta)
      }
    } else if (coeff < 0) {
      terms[i] = paste0("- ", beta)
    }
  }
  
  # Concatenate the terms into a single string
  expression <- paste(terms, collapse = " ")
  
  # Remove any leading '+' if present
  expression <- sub("^\\+\\s*", "", expression)
  
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

qual_Y_metrics = function(X, Y, m, l){
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

elicit_prior_beta0_function = function(p, gamma, m1, m2, s1, s2, alpha01, alpha02){
  #' We elicit the prior for beta0 and lambda0.
  #' @param p represents the number of dimensions.
  #' @param gamma represents the virtual certainty.
  #' @param m1 represents the lower bound for beta0.
  #' @param m2 represents the upper bound for beta0.
  #' @param alpha01 a vector generated from sigma, along with alpha02.
  #' s1 and s2 are added as parameters for convenience.
  
  beta0 = (m1 + m2)/2 
  gam = (1+gamma)/2
  lambda0 = (m2 - m1)/(2 * sqrt(alpha02/alpha01) * qt(gam, df = 2 * alpha01))
  
  newlist = list("beta0" = beta0, "lambda0" = lambda0, "m1" = m1, "m2" = m2,
                 "s1" = s1, "s2" = s2)
  return(newlist)
}

qual_sample_prior = function(Nprior, k, alpha01, alpha02, beta0, lambda0){
  #' This generates a sample of Nprior from the prior on N(mu, Sigma).
  #' @param Nprior represents the Monte Carlo sample size.
  #' @param k represents the number of possible combinations between the factors.
  #' The values of alpha01, alpha02, beta0, and lambda0 are from the prior elicitation.
  prior_sigma_2_vector = rep(0, Nprior)
  prior_beta_matrix = matrix(NA, nrow = Nprior, ncol = k)
  for(i in 1:Nprior){
    prior_sigma_2_vector[i] = 1/(rgamma(1, alpha01, alpha02))
    prior_Sigma = diag(lambda0 * prior_sigma_2_vector[i])
    prior_beta_matrix[i,] = mvrnorm(mu = beta0, Sigma = prior_Sigma)
  }
  newlist = list("prior_sigma_2_vector" = prior_sigma_2_vector,
                 "prior_beta_matrix" = prior_beta_matrix)
  return(newlist)
}

qual_sample_post = function(Npost, X, k, n, alpha01, alpha02, lambda0, beta0, b, s_2){
  #' This generates a sample of Npost from the posterior on N(mu, Sigma).
  #' @param Npost represents the Monte Carlo sample size.
  #' @param k represents the number of possible combinations between the factors.
  #' @param n represents the total sample size.
  #' The values of alpha01, alpha02, and beta0 are from the prior elicitation.
  #' The values of b and s_2 are computed from the minimal sufficient statistic.
  Lambda0 = diag(lambda0)
  inv_L0 = diag(1/lambda0)
  
  side_eqn = solve(Lambda0 + solve(t(X) %*% X))
  alpha02_y = alpha02 + s_2/2 + (t(b - beta0) %*% side_eqn %*% (b - beta0))/2
  
  Sigma_y = solve(t(X) %*% X + solve(Lambda0))
  beta_y = solve(t(X) %*% X + inv_L0) %*% (t(X) %*% X %*% b + inv_L0 %*% beta0)
  
  post_sigma_2_vector = rep(0, Npost)
  post_beta_matrix = matrix(NA, nrow = Npost, ncol = k)
  
  for(i in 1:Npost){
    post_sigma_2 = 1/rgamma(1, n/2 + alpha01, alpha02_y)
    post_sigma_2_vector[i] = post_sigma_2
    post_beta_matrix[i,] = mvrnorm(mu = beta_y, Sigma = post_sigma_2 * Sigma_y)
  }
  newlist = list("post_sigma_2_vector" = post_sigma_2_vector,
                 "post_beta_matrix" = post_beta_matrix)
  return(newlist)
}

qual_sample_prior_reformat = function(levels, sigma_2_vector, beta_matrix){
  #' Reformats the values to create a dataframe that contains sigma_2 vector and the
  #' beta matrix.
  new_names = create_beta_list_names(levels)
  df = as.data.frame(beta_matrix)
  colnames(df) = c("b11", "b21", "b12", "b22", "b13", "b23")
  df$sigma_2 = sigma_2_vector
  return(df)
}

smoother_function = function(psi_density, counts, smoother){
  #' A helper function that makes a smoothed plot of the density of psi.
  #' @param psi_density a vector containing the density of the histogram psi values.
  #' @param counts the counts associated with the histogram.
  #' @param smoother an odd number of points to average prior density values.
  #' 
  psi_dens_smoothed = psi_density
  numcells = length(counts)
  halfm = (smoother-1)/2
  for(i in (1+halfm):(numcells-halfm)){
    sum = 0
    for (j in (-halfm):halfm){
      sum = sum + psi_density[i+j]
    }
    psi_dens_smoothed[i]=sum/smoother 
  }
  return(psi_dens_smoothed)
}

psi_plot_vals = function(delta = 0.5, smoother = c(7, 7), prior_psi, post_psi){
  #' Obtains the smoothed plot of the density of psi for both the prior and the posterior.
  #' @param smoother a vector containing an odd number of points to average prior density values.
  #' The first value is associated for the prior and the second is for the posterior.
  #' @param prior_psi the vector containing the prior psi values.
  #' @param post_psi the vector containing the posterior psi values.
  
  lower_bd = round_any(min(prior_psi, post_psi), accuracy = 0.1, f = floor)
  upper_bd = round_any(max(prior_psi, post_psi), accuracy = 0.1, f = ceiling)
  
  breaks = seq(lower_bd, upper_bd, by = delta)
  if(breaks[length(breaks)] <= upper_bd){
    breaks = c(breaks, breaks[length(breaks)] + delta)
  }
  
  prior_psi_hist = hist(prior_psi, breaks, freq = F)
  psi_mids = prior_psi_hist$mids
  post_psi_hist = hist(post_psi, breaks, freq = F)
  
  prior_psi_dens_smoothed = smoother_function(prior_psi_hist$density, 
                                              prior_psi_hist$counts, 
                                              smoother[1])
  post_psi_dens_smoothed = smoother_function(post_psi_hist$density , 
                                             post_psi_hist$counts, 
                                             smoother[2])
  
  newlist = list("psi_mids" = psi_mids, "prior_psi_dens" = prior_psi_hist$density,
                 "prior_psi_dens_smoothed" = prior_psi_dens_smoothed,
                 "post_psi_dens" = post_psi_hist$density,
                 "post_psi_dens_smoothed" = post_psi_dens_smoothed,
                 "breaks" = breaks)
  return(newlist)
}

rbr_psi = function(prior_psi_dens_smoothed, post_psi_dens_smoothed, breaks){
  #' Obtain the relative belief ratio of psi based off of the prior and posterior values.
  #' @param prior_psi_dens_smoothed represents the prior psi values.
  #' @param post_psi_dens_smoothed represents the posterior psi values.
  #' @param breaks represents the breaks of the histogram.
  
  # Only need to focus on the max value due to endpoints
  numcells = length(breaks)-1
  RB_psi = rep(0, numcells)
  for (i in 1:numcells){
    if (prior_psi_dens_smoothed[i] != 0){
      RB_psi[i] = post_psi_dens_smoothed[i]/prior_psi_dens_smoothed[i]}
  }
  return(RB_psi)
}



