# old functions; not used anymore.

sample_prior = function(alpha01, alpha02, mu_0, sigma_0){
  # Let 1/delta_{i} ~ gamma(alpha01_{i}, alpha02_{i}) for i = 1, 2, .., p where p is the sample size.
  # mu ~ N_{p}(mu_0, sigma_{0}^{2} * SIGMA) 
  if(length(alpha01) != length(alpha02)){
    return("Error: the vector for alpha_01 and beta_01 are of a different size.")
  }
  p = length(alpha01)
  deltas = c() 
  for(i in 1:p){
    gam = rgamma(1, shape = alpha01[i], scale = alpha02[i]) 
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
  
  newlist = list("mu" = mu, "sigma" = diag(SIGMA), "R" = R)
  
  return(newlist)
}

sample_multiple_prior = function(n, alpha01, alpha02, mu_0, sigma_0){
  mu_vectors = c()
  sigma_vectors = c()
  R_vectors = list() # recall, R is from the onion method
  for(i in 1:n){
    sample = sample_prior(alpha01, alpha02, mu_0, sigma_0)
    mu_vectors = rbind(mu_vectors, sample$mu)
    sigma_vectors = rbind(sigma_vectors, sample$sigma)
    #print(sample$R) # debugging
    R_vectors[[i]] = sample$R
  }
  newlist = list("mu" = mu_vectors, "sigma" = sigma_vectors, 
                 "R" = R_vectors)
  return(newlist)
}

sample_post = function(alpha01, alpha02, Y = FALSE, N, mu_0, sigma_0){
  if(length(alpha01) != length(alpha02)){
    return("Error: the vector for alpha_01 and beta_01 are of a different size.")
  }
  p = length(alpha01)
  if(is.numeric(Y) == TRUE){
    n = nrow(Y)
    if(n < (2*p)){
      return("Error: the value of n (size of Y) is too small.")
    }
    Yprime = t(Y)
    Ybar = rowMeans(Yprime)
    In = matrix(t(rep(1, n))) # identity column
    Ybar_t = matrix(Ybar,nrow=1, ncol = p) # transpose
    S = (1/(n-1)) * t(Y - In%*%Ybar_t) %*% (Y - In%*%Ybar_t)
  } else {
    return("Error: no data given.")
  }
  
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

sample_rbr_mu = function(prior_mu, post_mu, delta){
  
  if(ncol(prior_mu) != ncol(post_mu)){
    return("Error: the number of columns for the prior mu and posterior 
           mu are not equal.")
  }
  # note: delta is the length of the bins.
  rbr = list()
  rbr_sequence = list()
  for(i in 1:ncol(prior_mu)){
    # we first start by focusing on each individual column.
    min_val = floor(min(prior_mu[,i], post_mu[,i]))
    max_val = ceiling(max(prior_mu[,i], post_mu[,i]))
    grid = seq(min_val, max_val, by = delta)
    if(!(max_val %in% grid) == TRUE){
      grid = c(grid, max)
    }
    prior_density = hist(prior_mu[,i], prob = TRUE, breaks = grid)
    post_density = hist(post_mu[,i], prob = TRUE, breaks = grid)
    rbr_density = divison_alt(prior_density$counts, post_density$counts)
    rbr[[i]] = rbr_density
    rbr_sequence[[i]] = prior_density$mids # can be prior or posterior
  }
  newlist = list("rbr" = rbr, "rbr_sequence" = rbr_sequence)
  return(newlist)
}

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

# reason: we need 3 graphs due to the inconsistent endpoints
prior_post_mu_graph = function(prior_mu,
                               post_mu,
                               col_num,
                               delta,
                               colour_hist = c("blue", "red"),
                               lty_type = 2,
                               transparency = 0.1){
  post_mu_values = post_mu[,col_num]
  prior_mu_values = prior_mu[,col_num]
  # see latex support later for graphs - was able to for a paper.
  title = paste("Graph of the Prior of mu", col_num, sep = " ")
  xlab_title = paste("mu", col_num, sep = " ")
  
  prior_rgb_version = col2rgb(colour_hist[1])
  post_rgb_version = col2rgb(colour_hist[2])
  prior_hist_col = rgb(prior_rgb_version[1]/255, 
                       prior_rgb_version[2]/255, 
                       prior_rgb_version[3]/255, 
                       alpha = transparency)
  post_hist_col = rgb(post_rgb_version[1]/255, 
                      post_rgb_version[2]/255, 
                      post_rgb_version[3]/255, 
                      alpha = transparency)
  
  # getting the sequence values
  min = min(floor(min(post_mu_values)), floor(min(prior_mu_values)))
  max = max(ceiling(max(post_mu_values)), ceiling(max(prior_mu_values)))
  grid = seq(min, max, by = delta)
  if(!(max %in% grid) == TRUE){
    grid = c(grid, max)
  }
  
  # Plots of the Prior and the Posterior
  prior_plot = hist(prior_mu_values, breaks = grid, prob = TRUE)
  post_plot = hist(post_mu_values, breaks = grid, prob = TRUE)
  plot(prior_plot, main = title, ylab = "Densities", xlab = xlab_title, 
       col = prior_hist_col, border = "#ffffff")
  plot(post_plot, add = T, col = post_hist_col, border = "#ffffff")
  lines(density(prior_mu_values), lwd = 2, lty = lty_type, col = "black")
  lines(density(post_mu_values), lwd = 2, lty = lty_type, col = "black")
}


