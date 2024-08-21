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

qual_C_matrix = function(m, l){
  #' Form a contrast matrix C generate form is tensor product of
  #' Hermit matrices.
  #' @param m represents the number of factors.
  #' @param l a vector that contains the number of levels per factor.
  C = 1
  for(i in 1:m){
    Ci = cbind(rep(1, l[i]), contr.helmert(l[i])) # dropped the normalization
    C = C %x% Ci # kronecker product
  }
  return(C)
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
  #' This generates a sample of Npost from the posterior on beta and signa^2.
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

qual_sample_reformat = function(levels, sigma_2_vector, beta_matrix){
  #' Reformats the values to create a dataframe that contains sigma_2 vector and the
  #' beta matrix.
  #' @param levels a vector containing the number of levels per factor.
  #' @param sigma_2_vector is a vector containing the sigma_2 values.
  #' @param beta_matrix is a matrix containing the beta values.
  new_names = create_beta_list_names(levels)
  df = as.data.frame(beta_matrix)
  colnames(df) = create_beta_list_names(levels, text = "b")
  df$sigma_2 = sigma_2_vector
  return(df)
}

smoother_function = function(psi_density, counts, smoother){
  #' A helper function that makes a smoothed plot of the density of psi.
  #' @param psi_density a vector containing the density of the histogram psi values.
  #' @param counts the counts associated with the histogram.
  #' @param smoother an odd number of points to average prior density values.
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

psi_plot_vals = function(delta = 0.5, smoother = c(1, 1), prior_psi, post_psi){
  #' Obtains the smoothed plot of the density of psi for both the prior and the posterior.
  #' @param smoother a vector containing an odd number of points to average density values.
  #' The first value is associated for the prior and the second is for the posterior.
  #' @param prior_psi the vector containing the prior psi values.
  #' @param post_psi the vector containing the posterior psi values.
  
  lower_bd = delta * floor(min(prior_psi)/delta) - 0.5 * delta
  upper_bd = delta * ceiling(max(prior_psi)/delta) + 0.5 * delta
  
  breaks = seq(lower_bd, upper_bd, by = delta)
  
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

qual_rbr_psi = function(prior_psi_dens_smoothed, post_psi_dens_smoothed, breaks){
  #' Obtain the relative belief ratio of psi based on the prior and posterior.
  #' @param prior_psi_dens_smoothed represents the prior psi values.
  #' @param post_psi_dens_smoothed represents the posterior psi values.
  #' @param breaks represents the breaks of the histogram.
  
  # Only need to focus on the max value due to endpoints
  numcells = length(breaks)-1 # also size length(psi_mids)
  RB_psi = rep(0, numcells)
  for (i in 1:numcells){
    if (prior_psi_dens_smoothed[i] != 0){
      RB_psi[i] = post_psi_dens_smoothed[i]/prior_psi_dens_smoothed[i]}
  }
  return(RB_psi)
}



