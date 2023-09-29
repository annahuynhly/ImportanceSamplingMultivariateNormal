# note: might want to rename this as generating 

################################################################
# HELPER FUNCTIONS                                             #
################################################################

vnorm = function(x, t){
  # Computes the norm of the matrix x of type t.
  norm(matrix(x, ncol=1), t)
}

onion = function(dimension){
  # Generating using the onion method from the paper: On Bayesian Hotelling's T^{2} test for the mean
  d = dimension + 1
  prev_corr = matrix(1, 1, 1)
  
  for(k in 2:(d-1)){
    # sample y = r^2 from a beta distribution, with alpha_1 = (k-1)/2 and alpha_2 = (d-k)/2
    y = rbeta(1, (k-1)/2, (d-k)/2)
    r = as.matrix(sqrt(y))
    
    # sample a unit vector theta uniformly from the unit ball surface B^(k-1)
    v = matrix(rnorm((k-1)), nrow=1)
    theta = v/vnorm(v, '1')
    
    w = r %*% theta # set w = r theta
    
    # set q = prev_corr^(1/2) w, q to the power of 0.5
    e = eigen(prev_corr)
    VV = e$vectors
    q_prep = VV %*% diag(sqrt(e$values)) %*% t(VV)
    q = (w%*% q_prep)
    
    next_corr = matrix(0, nrow=k, ncol=k)
    next_corr[1:(k-1), 1:(k-1)] = prev_corr # R_k-1
    next_corr[k, 1:(k-1)] = q
    next_corr[1:(k-1), k] = q
    diag(next_corr) = 1
    
    prev_corr = next_corr
  }
  return(next_corr)
}

################################################################
# MAIN FUNCTIONS                                               #
################################################################

sample_prior = function(alphas, betas, mu_0, sigma_0){
  # Let 1/delta_{i} ~ gamma(alpha_{i}, beta_{i}) for i = 1, 2, .., p where p is the sample size.
  # mu ~ N_{p}(mu_0, sigma_{0}^{2} * SIGMA) 
  if(length(alphas) != length(betas)){
    return("Error: the vector for alpha and beta are of a different size.")
  }
  sample_size = length(alphas)
  deltas = c() 
  for(i in 1:sample_size){
    gam = rgamma(1, shape = alphas[i], scale = betas[i]) # need to VERIFY if it is 1/beta.. ASK which gamma funct. they use
    deltas = c(deltas, gam)
  }
  # Making diag(delta_{1}, delta_{2}, ..., delta_{p})
  diagtri = diag(sample_size)*deltas
  
  # Obtain R using onion method
  R = onion(sample_size)
  
  # Find SIGMA
  SIGMA = diagtri^{1/2} * R * diagtri^{1/2}
  
  # Generate normal rvs
  z_vector = rnorm(n = sample_size, mean = 0, sd = 1)
  
  # Finally geenrating mu
  mu = mu_0 + sigma_0 * SIGMA^{1/2} * z_vector
  
  newlist = list("mu" = mu, "sigma" = SIGMA)
  return(newlist)
}

#alphas = c(1, 2, 3, 4, 5)
#betas = c(1, 2, 3, 4, 5)
#mu_0 = 1
#sigma_0 = 1
#sample_prior(alphas, betas, mu_0, sigma_0)


# trying to sample from the posterior (note: may change file name entirely!)

# generate zeta_{i} ~ Wp(A^{-1}(Y), n-p+1) #wishet
######################################

# first, compute A(Y) which is defined by
#compute_AY = function(n, s, sigma_0, ybar, mu_0)


# calculate SIGMA_{i} = zeta_{i}^{-1} to obtain sigma_{11}, ..., sigma_{pp} (diag elements)
######################################

# generate mu_{i} = N_{p}(...)
######################################

# evaluate the estimator I_{N}(h)
######################################

################################################################
# ELICITING FROM THE PRIOR                                     #
################################################################



