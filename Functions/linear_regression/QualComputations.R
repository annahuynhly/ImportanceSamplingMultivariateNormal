################################
#TODO: write documentation!!   #
################################

create_beta_list_names = function(levels){
  # Create a list of factors
  factors = lapply(levels, function(x) 1:x)
  # Generate all combinations
  combinations = expand.grid(factors)
  # Create the names
  names = apply(combinations, 1, function(row) paste0("b", paste(row, collapse = "")))
  return(names)
}

qual_generate_X = function(k, n_vector){
  # TODO: write documentation
  X = matrix(0, nrow = k, ncol = sum(n_vector))
  
  for(i in 1:k) {
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
  # TODO: write documentation
  b = solve(t(X) %*% X) %*% t(X) %*% Y
  s_2 = t(Y - X %*% b) %*% (Y - X %*% b)
  C = 1
  for(i in 1:m){
    # possibly need to debug
    Ci = cbind(rep(1, l[i]), contr.helmert(l[i])) # dropped the normalization
    C = C %x% Ci # kronecker product
  }
  newlist = list("b" = b, "s_2" = s_2, "C" = C)
  return(newlist)
}

elicit_prior_beta0_function = function(p, gamma, m1, m2, s1, s2, alpha01, alpha02){
  #' We elicit the prior for mu, which is given by:
  #' mu ~ mu0 + sqrt(alpha02/alpha01) (lambda0) (t_{2*alpha01}) 
  #' @param p represents the number of dimensions.
  #' @param gamma represents the virtual certainty.
  vectors_of_interest = list(m1, m2, s1, s2, alpha01, alpha02)
  
  beta0 = (m1 + m2)/2 
  gam = (1+gamma)/2
  lambda0 = (m2 - m1)/(2 * sqrt(alpha02/alpha01) * qt(gam, df = 2 * alpha01))
  
  newlist = list("beta0" = beta0, "lambda0" = lambda0, "m1" = m1, "m2" = m2)
  return(newlist)
}

qual_sample_prior = function(Nprior, k, alpha01, alpha02, beta0){
  # k here denotes the number of factors
  # todo: write more detailed documentation later
  prior_sigma_2_vector = rep(0, Nprior)
  prior_beta_matrix = matrix(NA, nrow = Nprior, ncol = k)
  for(i in 1:Nprior){
    prior_sigma_2_vector[i] = 1/(rgamma(1, alpha01, alpha02))
    # is this s^2?
    prior_beta_matrix[i,] = rnorm(beta0, lambda0 * prior_sigma_2_vector[i])
  }
  newlist = list("prior_sigma_2_vector" = prior_sigma_2_vector,
                 "prior_beta_matrix" = prior_beta_matrix)
  return(newlist)
}

qual_sample_post = function(Npost, k, n, alpha01, alpha02, lambda0, beta0, b, s_2){
  # write the description later
  Lambda0 = diag(lambda0)
  inv_L0 = diag(1/lambda0)
  
  side_eqn = solve(Lambda0 + solve(t(X) %*% X))
  alpha02_y = alpha02 + s_2/2 + (t(b - beta0) %*% side_eqn %*% (b - beta0))/2
  
  beta_y = solve(t(X) %*% X + inv_L0) %*% (t(X) %*% X %*% b + inv_L0 %*% beta0)
  
  post_sigma_2_vector = rep(0, Npost)
  post_beta_matrix = matrix(NA, nrow = Npost, ncol = k)
  
  for(i in 1:Npost){
    post_sigma_2 = 1/rgamma(1, n/2 + lambda0, alpha02_y)
    post_sigma_2_vector[i] = post_sigma_2
    post_beta_matrix[i,] = rnorm(beta_y, post_sigma_2 * solve(t(X) %*% X + Lambda0))
  }
  newlist = list("post_sigma_2_vector" = post_sigma_2_vector,
                 "post_beta_matrix" = post_beta_matrix)
  return(newlist)
}

alpha_plot_vals = function(Nmontecarlo, smoother = 7, delta = 0.5, alpha_vals){
  #' Obtains the smoothed plot of the density of alpha (applicable to prior and
  #' posterior)
  #' @param Nprior represents the Monte Carlo sample size used for the prior.
  #' @param smoother an odd number of points to average prior density values.
  #' write more later....
  vals = abs(alpha_vals)
  alpha_upper_bd = max(vals)
  
  breaks = seq(0, alpha_upper_bd, by = delta)
  if (tail(breaks, n=1) <= alpha_upper_bd) {
    breaks = c(breaks, breaks[length(breaks)] + delta)
  } 
  
  alpha_hist = hist(vals, breaks, freq = F)
  alpha_mids = alpha_hist$mids
  alpha_density = alpha_hist$density 
  
  # making a smoothed plot of the density of alpba
  alpha_dens_smoothed = alpha_density
  numcells = length(alpha_hist$counts)
  halfm = (smoother-1)/2
  for(i in (1+halfm):(numcells-halfm)){
    sum = 0
    for (j in (-halfm):halfm){
      sum = sum + alpha_density[i+j]
    }
    alpha_dens_smoothed[i]=sum/smoother 
  }
  
  newlist = list("alpha_mids" = alpha_mids, 
                 "alpha_dens" = alpha_density,
                 "alpha_dens_smoothed" = alpha_dens_smoothed,
                 "breaks" = breaks)
  return(newlist)
}

rbr_alpha = function(prior_alpha_dens_smoothed, prior_alpha_breaks, 
                     post_alpha_dens_smoothed, post_alpha_breaks){
  #' Obtain the relative belief ratio of psi based off of the prior and posterior values.
  #' fix description later
  
  # only need to focus on the max value due to endpoints
  extra = abs(length(prior_alpha_breaks) - length(post_alpha_breaks))
  if(length(prior_alpha_breaks) > length(post_alpha_breaks)){
    post_alpha_dens_smoothed = c(post_alpha_dens_smoothed, rep(0, extra))
    rbr_breaks = prior_alpha_breaks
  } else {
    prior_alpha_dens_smoothed = c(prior_alpha_dens_smoothed, rep(0, extra))
    rbr_breaks = post_alpha_breaks
  }
  numcells = length(rbr_breaks)-1
  RB_alpha = rep(0, numcells)
  for (i in 1:numcells){
    if (prior_alpha_dens_smoothed[i] != 0){
      RB_alpha[i] = post_alpha_dens_smoothed[i]/prior_alpha_dens_smoothed[i]}
  }
  # getting the midpoints for rbr
  comp_delta = diff(rbr_breaks)[1]
  half_delta = comp_delta/2
  RB_mids = seq(from = rbr_breaks[1] + half_delta, 
                to = rbr_breaks[length(rbr_breaks)] - half_delta, 
                by = comp_delta)
  
  newlist = list("RB_breaks" = rbr_breaks, "RB_mids" = RB_mids,
                 "RB_alpha" = RB_alpha,
                 "post_alpha_dens_smoothed" = post_alpha_dens_smoothed,
                 "prior_alpha_dens_smoothed" = prior_alpha_dens_smoothed)
  return(newlist)
}