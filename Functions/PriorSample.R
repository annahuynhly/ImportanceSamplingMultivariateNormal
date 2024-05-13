################################################################
# HELPER FUNCTIONS                                             #
################################################################

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

################################################################
# MAIN FUNCTIONS                                               #
################################################################

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

# MAY REMOVE THE FUNCTION BELOW.
psi = function(mu, xi){
  #' A function the user is supposed to specify, but for now it 
  #' just gives you mu.
  return(mu)
}

sample_prior_data_cleaning = function(N, p, mu_matrix, 
                                      sigma_ii_matrix,
                                      correlation_matrix) {
  #' Mike requested a specific file format, and this cleans it accordingly.
  sigma_names = paste("1/sigma_", 1:p, "^2", sep = "")
  mu_names = paste("mu_", 1:p, sep = "")
  rho_names = combn(p, 2, FUN = function(x) paste("rho_", x[1], x[2], sep = ""), simplify = TRUE)
  
  # FIRST: cleaning 1/sigma^2
  sigma_ii_data = as.data.frame(1/sigma_ii_matrix)
  names(sigma_ii_data) = sigma_names
  
  # SECOND: cleaning rhos from the correlation matrix
  rho_matrix = matrix(nrow = N, ncol = length(rho_names))
  for(k in 1:N){
    rho_matrix[k,] = as.vector(correlation_matrix[[k]][lower.tri(correlation_matrix[[k]])])
  }
  
  rho_data = as.data.frame(rho_matrix)
  names(rho_data) = rho_names
  
  # THIRD: cleaning mus from the mu matrix
  mu_data = as.data.frame(mu_matrix)
  names(mu_data) = mu_names
  
  return(cbind(mu_data, sigma_ii_data, rho_data))
}
