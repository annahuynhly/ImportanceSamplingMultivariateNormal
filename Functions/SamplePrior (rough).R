# Need to do: Importance Sampling for the Multivariate Normal Distribution
library(expm) # used for the onion method 
library(powerplus)

# First: sample from the prior

# Generate 1/delta^_{1}, ..., 1/delta_{p} and make a diag(delta_{1}, ...., delta_{p})

# 1/delta_{1} ~ gamma(aloha_{i}, beta_{i})

# this is the user input!! MAY CHANGE
alphas = c(1, 2, 3, 4, 5)
betas = c(1, 2, 3, 4, 5)

deltas = c() # assumption: len(alphas) == len(betas)
for(i in 1:length(alphas)){
  gam = rgamma(1, shape = alphas[i], scale = betas[i]) # need to VERIFY if it is 1/beta
  deltas = c(deltas, gam)
}
# making diag(delta_{1}, delta_{2}, ..., delta_{p})
diagtri = diag(length(alphas))*deltas

# generate R (onion method) ##############
vnorm = function(x, t) {
  norm(matrix(x, ncol=1), t)
}

onion = function(dimension) {
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

# find sigma for sigma = diag(delta1, ..., deltap)^{1/2} R diag(delta1, ..., deltap)^{1/2}

sigma = diagtri^{1/2} * onion(length(alphas)) * diagtri^{1/2}

# generate mu

# note: GIVEN: mu_0, sigma^{2}-{0}
mu_0 = 2
sigma_0 = 2

# generate normal rvs
z_vector = rnorm(n = length(alphas), mean = 0, sd = 1)

# finally geenrating mu

mu = mu_0 + sigma_0 * sigma^{1/2} * z_vector
mu



