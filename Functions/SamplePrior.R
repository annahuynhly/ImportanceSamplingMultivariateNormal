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

sample_prior = function(alpha01, alpha02, mu_0, sigma_0){
  # Let 1/delta_{i} ~ gamma(alpha01_{i}, alpha02_{i}) for i = 1, 2, .., p where p is the sample size.
  # mu ~ N_{p}(mu_0, sigma_{0}^{2} * SIGMA) 
  if(length(alpha01) != length(alpha02)){
    return("Error: the vector for alpha_01 and beta_01 are of a different size.")
  }
  sample_size = length(alpha01)
  deltas = c() 
  for(i in 1:sample_size){
    gam = rgamma(1, shape = alpha01[i], scale = alpha02[i]) # need to VERIFY if it is 1/beta.. ASK which gamma funct. they use
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

sample_posterior = function(alpha01, alpha02, n, N, mu_0, sigma_0){
  if(length(alpha01) != length(alpha02)){
    return("Error: the vector for alpha_01 and beta_01 are of a different size.")
  }
  p = length(alpha01)
  if(n < (2*p)){
    return("Error: the value of n (size of Y) is too small.")
  }
  
  mu = rep(0, p) #temporary dummy data -> MAY REPLACE?
  sigma = diag(p) # identity matrix, for now.
  
  # generating Y. May have an option where the user inputs Y themselves - ask?
  Y = mvrnorm(n = n, mu = mu, Sigma = sigma)
  
  Yprime = t(Y)
  Ybar = rowMeans(Yprime)
  
  In = matrix(t(rep(1, n))) # identity column
  Ybar_t = matrix(Ybar,nrow=1, ncol = p) # transpose
  S = (1/(n-1)) * t(Y - In%*%Ybar_t) %*% (Y - In%*%Ybar_t)
  
  # getting the A(Y) formula
  AY = (n-1)*S + n*(n * sigma_0^2 + 1)^(-1) * ((Ybar - mu_0) %*% t(Ybar - mu_0))
  
  # finding the inverse. to ensure positive definite: need to round.
  inverse_AY = round(solve(AY), 15)
  
  zetas = rWishart(n = N, df = (n - p - 1), Sigma = inverse_AY)
  
  temp_mean = (mu_0/(sigma_0^{2}) + n * Ybar)/(1/sigma_0^{2} +n) 
  mui_matrix = c()
  for(i in 1:N){
    SIGMA_i = solve(zetas[,,i])
    temp_variance = ((1/sigma_0^{2}) + n)^{-1} * SIGMA_i
    # need to do the rounding things again...
    mu_i = mvrnorm(n = 1, mu = temp_mean, Sigma = temp_variance)
    mui_matrix = rbind(matrix = mui_matrix, c(mu_i)) 
  }
  
  # getting the k(SIGMA) function
  K_Sigma_vect = c()
  for(i in 1:N){
    k_Sigma = 1
    sigma_ii = diag(solve(zetas[,,i])) # getting the diagonals
    for(i in 1:p){
      prod = (sigma_ii[i])^(-alpha01[i] - (p+1)/2) * exp(-alpha02[i]/sigma_ii[i])
      k_Sigma = k_Sigma * prod
    }
    K_Sigma_vect = c(K_Sigma_vect, k_Sigma)
  }
  
  # test for h: let h(mu, Sigma) = mu_1 (first coordinate of vector mu)
  INh = sum(mui_matrix[, 1]*K_Sigma_vect)/sum(K_Sigma_vect)
  
  newlist = list("AY" = AY, "INh" = INh)
  
  return(newlist)
}

################################################################
# ELICITING FROM THE PRIOR                                     #
################################################################

# functions we discarded.

test_matrix_symmetry = function(matrix){
  # Note: issue with the isSymmetric function where it doesn't count for
  # past a certain decimal place.
  ans = TRUE
  for(i in 1:nrow(matrix)){
    for(j in 1:ncol(matrix)){
      if(matrix[i,j]!=matrix[j,i]){
        ans = FALSE;break}
    }
  }
  return(ans)
}

test_matrix_symmetry = function(matrix){all(matrix==t(matrix))}

ensure_positive_definite = function(matrix){
  i = 5 # start the iteration. 
  # 5 is an arbitrary start, but if we start at i we will get the 0 matrix.
  is_pos_def = TRUE
  while(is_pos_def == TRUE){
    test_matrix = round(matrix, i)
    if(test_matrix_symmetry(test_matrix) == TRUE){
      if(is.positive.definite(test_matrix) == TRUE){
        i = i + 1
      } else {
        is_pos_def = FALSE
      }
    } else {
      is_pof_def = FALSE
    }
  }
  if(i == 1){
    return("Error: matrix is not symmetric or positive definite at all.")
  } else {
    return(round(matrix, i-1))
  }
}


