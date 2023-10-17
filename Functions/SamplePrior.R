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
  p = length(alpha01)
  deltas = c() 
  for(i in 1:p){
    gam = rgamma(1, shape = alpha01[i], scale = alpha02[i]) # need to VERIFY if it is 1/beta.. ASK which gamma funct. they use
    deltas = c(deltas, gam)
  }
  # Making diag(delta_{1}, delta_{2}, ..., delta_{p})
  diagtri = diag(p)*deltas
  
  # Obtain R using onion method
  R = onion(p)
  
  # Find SIGMA
  SIGMA = diagtri^{1/2} * R * diagtri^{1/2}
  
  # Generate normal rvs
  z_vector = rnorm(n = p, mean = 0, sd = 1)
  
  # Finally generating mu
  mu = mu_0 + sigma_0 * diag(SIGMA^{1/2}) * z_vector
  
  newlist = list("mu" = mu, "sigma" = diag(SIGMA))
  
  return(newlist)
}

sample_multiple_prior = function(n, alpha01, alpha02, mu_0, sigma_0){
  mu_vectors = c()
  sigma_vectors = c()
  for(i in 1:n){
    sample = sample_prior(alpha01, alpha02, mu_0, sigma_0)
    mu_vectors = rbind(mu_vectors, sample$mu)
    sigma_vectors = rbind(sigma_vectors, sample$sigma)
  }
  newlist = list("mu" = mu_vectors, "sigma" = sigma_vectors)
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
  
  # finding the inverse.
  inverse_AY = find_inverse_alt(AY)
  
  zetas = rWishart(n = N, df = (n - p - 1), Sigma = inverse_AY)
  
  temp_mean = (mu_0/(sigma_0^{2}) + n * Ybar)/(1/sigma_0^{2}+n) 
  mui_matrix = c()
  SIGMA_i_matrices = list()
  for(i in 1:N){
    SIGMA_i = find_inverse_alt(zetas[,,i])
    SIGMA_i_matrices[[i]] = SIGMA_i
    temp_variance = ((1/sigma_0^{2}) + n)^{-1} * SIGMA_i
    # need to do the rounding things again...
    mu_i = mvrnorm(n = 1, mu = temp_mean, Sigma = temp_variance)
    mui_matrix = rbind(matrix = mui_matrix, c(mu_i)) 
  }
  
  # getting the k(SIGMA) function
  K_Sigma_vect = c()
  for(i in 1:N){
    k_Sigma = 1
    sigma_ii = diag(SIGMA_i_matrices[[i]])
    #sigma_ii = diag(find_inverse_alt(zetas[,,i])) # getting the diagonals
    for(i in 1:p){
      prod = (sigma_ii[i])^(-alpha01[i] - (p+1)/2) * exp(-alpha02[i]/sigma_ii[i])
      k_Sigma = k_Sigma * prod
    }
    K_Sigma_vect = c(K_Sigma_vect, k_Sigma)
  }
  
  # test for h: let h(mu, Sigma) = mu_1 (first coordinate of vector mu)
  #INh = sum(mui_matrix[, 1]*K_Sigma_vect)/sum(K_Sigma_vect)
  
  newlist = list("mu" = mui_matrix, "sigma" = SIGMA_i_matrices,
                 "k_zeta" = K_Sigma_vect)
  
  return(newlist)
}

################################################################
# ELICITING FROM THE PRIOR                                     #
################################################################

elicit = function(alpha, beta, gamma, s1, s2){
  # This function finds the two values of gamma which we are subtracting.
  
  prob1 = (1+gamma)/2
  prob2 = (1-gamma)/2
  G = matrix(0,2,1)
  
  G[1] = qgamma(prob1, alpha, beta) - (qnorm(prob1)/s1)^2
  G[2] = qgamma(prob2, alpha, beta) - (qnorm(prob1)/s2)^2
  
  return(G)  
}

generate_samp_var = function(gamma, p, const, s1, s2){
  # this checks whether the user inputs a valid s1, s2 or whether they just placed
  # constraints.
  if(is.numeric(const) == FALSE & is.numeric(s1) == TRUE & is.numeric(s2) == TRUE){
    if(length(s1) == length(s2)){
      return(list("s1" = s1, "s2" = s2))
    } else {
      return("Error")
    }
  } else if (is.numeric(const) == TRUE & is.numeric(s1) == FALSE & is.numeric(s2) == FALSE){
    s1 = numeric()           
    s2 = numeric()
    
    for (i in 1:p){
      s1 = c(s1, qnorm((1+gamma)/2))
      s2 = c(s2, const[i]*(qnorm((1+gamma)/2)))
    }
    return(list("s1" = s1, "s2" = s2))
  } else {
    return("Error")
  }
}

prior_elicitation = function(gamma, m1, m2, const = FALSE, s1 = FALSE, s2 = FALSE){
  # algorithm doesn't work with a poor choice of s1 and s2; keep this here, but
  # remove options for the user.
  if(length(m1) == length(m2)){
    p = length(m1)
  } else {
    return("Error: length of m1, m2 are not equal.")
  }
  mu0 = (m1+m2)/2 # multivariate mu_0
  
  s = generate_samp_var(gamma, p, const, s1, s2)
  if(s[1] != "Error"){
    s1 = s$s1
    s2 = s$s2
  } else {
    return("Error: cannot determine the values for s1, s2.")
  }
  
  alphas = matrix(rep(1, p), nrow=1) 
  betas = matrix(rep(1, p), nrow=1)          
  x = as.matrix(rbind(alphas, betas))
  delta = x
  
  j = 1
  # initializing
  delta0 = matrix(c(alphas[1],betas[1]), ncol=1)
  while(j <= p){
    maxiter = 1000   
    iter = 0
    tol = 0.5 
    
    while (norm(delta0) > tol & iter < maxiter){
      h = 0.000000001 # increase point
      
      fx = elicit(x[1,j], x[2,j], gamma, s1[j], s2[j])
      Jx = matrix(0,2,2)
      
      for(i in 1:2){
        xh = x # estimation values
        xh[i,j] = x[i,j] + h
        
        fxh = elicit(xh[1,j], xh[2,j], gamma, s1[j], s2[j]) # estimated
        Jx[,i] = (fxh - fx)/h 
      }
      
      delta[,j] = -solve(Jx) %*% elicit(x[1,j], x[2,j], gamma, s1[j], s2[j])  
      delta0[,1] = delta[,j]
      
      x[,j] = x[,j] + delta[,j]      
      iter = iter + 1   
    }
    j = j+1
    delta0 = matrix(c(alphas[j],betas[j]), ncol=1)
  }
  
  sigma0 = (m2 - m1)/(2*s1)
  
  newlist = list("alphas" = x[1,], "betas" = x[2,], "s1" = s1, "s2" = s2,
                 "mu0" = mu0, "sigma0" = sigma0)
  return(newlist)
}

find_inverse_alt = function(matrix){
  # issue with solve(...): doesn't ensure the function is positive definite.
  # this ensures that it is.
  x = eigen(matrix, symmetric = TRUE, only.values=FALSE)
  Q = x$vectors
  V_inv = diag(1/x$values)
  B = Q%*%sqrt(V_inv)
  inverse_matrix = B%*%t(B)
  return(inverse_matrix)
}

################################################################
# GRAPH FUNCTIONS                                              #
################################################################

prior_mu_graph = function(prior_mu, 
                          col_num,
                          colour_choice = c("blue", "blue"),
                          lty_type = 2,
                          transparency = 0.1){
  # This generates the graph for the prior of the mu, given the number of mu.
  
  mu_values = prior_mu[,col_num]
  # see latex support later for graphs - was able to for a paper.
  title = paste("Graph of the Prior of mu", col_num, sep = " ")
  xlab_title = paste("mu", col_num, sep = " ")
  
  rgb_version = col2rgb(colour_choice[1])
  
  hist_col = rgb(rgb_version[1]/255, rgb_version[2]/255, rgb_version[3]/255, 
                alpha = transparency)
  
  # Plots of the Prior and the Posterior
  hist(mu_values, 
       main = title, ylab = "Densities", xlab = xlab_title, 
       col = hist_col, border = "#ffffff",
       prob = TRUE,
       ylim = c(0, 1))
  lines(density(mu_values), lwd = 2, lty = lty_type, col = colour_choice[2])
}

# need to test below

#test = sample_multiple_prior(n = 100, alpha01 = c(2, 2, 2), 
#                      alpha02 = c(4, 4, 4), mu_0 = c(0, 0, 0), 
#                      sigma_0 = c(1, 1, 1))
#prior_mu_graph(test$mu, col_num = 1)

################################################################
# DISCARDED - MAY BE USED LATER                                #
################################################################

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


