#######################################
# Part 3: Sampling from the posterior #
#######################################
# August 20, 2024

# Npost = size of sample from the posterior
Npost = 10000 

# Functions ######################################

qual_sample_post = function(Npost, X, k, n, alpha01, alpha02, lambda0, beta0, b, s_2){
  #' This generates a sample of Npost from the posterior on beta and sigma^2.
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

# Values #########################################

sample_post_vals = qual_sample_post(Npost, X, k, n, alpha01, alpha02, lambda0, 
                                    beta0, b, s_2)

post_sigma_2_vector = sample_post_vals$post_sigma_2_vector
post_beta_matrix = sample_post_vals$post_beta_matrix