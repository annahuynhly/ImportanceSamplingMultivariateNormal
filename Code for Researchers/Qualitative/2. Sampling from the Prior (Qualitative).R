###################################
# Part 2: Sampling from the prior #
###################################
# July 18, 2024

# Nprior = size of sample from the prior
Nprior = 10000

# Functions ######################################

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

# Values #########################################

sample_prior_vals = qual_sample_prior(Nprior = Nprior, k = k, alpha01 = alpha01, 
                                      alpha02 = alpha02, beta0 = beta0, lambda0 = lambda0)

prior_sigma_2_vector = sample_prior_vals$prior_sigma_2_vector
prior_beta_matrix = sample_prior_vals$prior_beta_matrix