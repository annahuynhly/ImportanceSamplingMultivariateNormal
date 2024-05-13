###################################
# Part 2: Sampling from the prior #
###################################
# April 30, 2024

# Nprior= size of sample from the prior
Nprior=10000

# Functions ######################################

find_inverse_alt = function(matrix){
  #' Helper function that computes the inverse of a matrix using an 
  #' alternative method that preserves positive-definiteness.
  #' @param matrix represents the matrix that is being inputted. 
  x = eigen(matrix, symmetric = TRUE, only.values=FALSE)
  Q = x$vectors
  V_inv = diag(1/x$values)
  B = Q%*%sqrt(V_inv)
  inverse_matrix = B%*%t(B)
  return(inverse_matrix)
}


onion = function(dimension){
  #' Generating using the onion method from the uniform distribution
  #' on the set of all pxp correlation matrices.
  #' @param dimension denotes the number of dimensions of the matrix.
  d = dimension + 1
  prev_corr = matrix(1, 1, 1)
  
  for(k in 2:(d-1)){
    # sample y = r^2 from a beta distribution, with alpha_1 = (k-1)/2 and alpha_2 = (d-k)/2
    y = rbeta(1, (k-1)/2, (d-k)/2)
    r = as.matrix(sqrt(y))
    
    # sample a unit vector theta uniformly from the unit ball surface B^(k-1)
    v = matrix(rnorm((k-1)), nrow=1)
    theta = v/norm(v, 'F')
    
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

sample_prior = function(Nprior, p, alpha01, alpha02, mu0, lambda0){
  #' This generates a sample of Nprior from the prior on (mu, Sigma).
  #' 1/sigmaii ~ gamma(alpha01, alpha02)
  #' R is a correlation matrix, R ~ uniform(pxp correlation matrices)
  #' Sigma = diag(sigmaii^1/2) x R x diag(sigmaii^1/2)
  #' mu|Sigma = multivariate_norm(mu0, diag(lambda0) x Sigma x diag(lambda0))
  #' @param Nprior represents the Monte Carlo sample size.
  #' @param p represents the number of dimensions.
  
  mu_mat = matrix(NA, nrow = Nprior, ncol = p)
  sigma_ii_mat = matrix(NA, nrow = Nprior, ncol = p)
  Sigma_mat = vector("list", length = Nprior)
  xi_mat = vector("list", length = Nprior)
    
  for(i in 1:Nprior){
    sigma_ii = 1/rgamma(p, alpha01, alpha02)
    D = diag(sqrt(sigma_ii)) 
    R = onion(p) # the correlation matrix
    Lambda = diag(lambda0)
    Sigma = D %*% R %*% D
    xi=find_inverse_alt(Sigma)
    var_mat = Lambda %*% Sigma %*% Lambda
    
    MU = mvrnorm(1, mu = mu0, Sigma = var_mat)
    
    # Store results in preallocated matrices/lists
    mu_mat[i,] = MU
    sigma_ii_mat[i,] = sigma_ii
    Sigma_mat[[i]] = Sigma
    xi_mat[[i]] = xi
    
  }
  
  return(list("mu_matrix" = mu_mat, "sigma_ii_mat" = sigma_ii_mat, 
               "Sigma_mat" = Sigma_mat, "xi_mat" = xi_mat))
}

# Values #########################################

sample_prior_vals = sample_prior(Nprior, p, alpha01, alpha02, mu0, lambda0)

mu_prior = sample_prior_vals$mu_matrix
sigma_ii_prior = sample_prior_vals$sigma_ii_mat
Sigma_prior =  sample_prior_vals$Sigma_mat[]
xi_prior = sample_prior_vals$xi_mat

