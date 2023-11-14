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

seq_alt = function(values, delta){
  # creates a sequence and includes the last value, even if it isn't captured
  # by the original sequence.
  min = floor(min(values))
  max = ceiling(max(values))
  grid = seq(min, max, by = delta)
  if(!(max %in% grid) == TRUE){
    grid = c(grid, max)
  }
  return(grid)
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

divison_alt = function(num, denom){
  # assumption: length(num) == length(denom)
  x = c()
  for(i in 1:length(num)){
    if(denom[i] == 0){
      x = c(x, NaN)
    } else {
      x = c(x, num[i]/denom[i])
    }
  }
  return(x)
}

################################################################
# MAIN FUNCTIONS                                               #
################################################################

sample_prior_new = function(N, alpha01, alpha02, mu0, lambda0){
  # a new way to sample the prior; this function name will be changed in the future.
  # N: refers to the monte carlo sample size.
  prior_samples = c()
  for(i in 1:N){
    sigma = 1/rgamma(1, shape = alpha01, rate = alpha02)
    sample = rnorm(N, mu0, lambda0^2 * sigma^2) 
    prior_samples = c(prior_samples, sample)
  }
  return(sample)
}

sample_prior_hyperparameters = function(gamma, alpha01, alpha02, m1, m2){
  lambda0 = (m2 - m1)/(2 * sqrt(alpha02/alpha01) * qt((1 + gamma)/2, df = 2 * alpha01))
  mu0 = (m2 - m1)/2
  newlist = list("mu0" = mu0, "lambda0" = lambda0)
  return(newlist)
}

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

#mu = rep(0, p) #temporary dummy data -> MAY REPLACE?
#sigma = diag(p) # identity matrix, for now.
# generating Y. May have an option where the user inputs Y themselves - ask?
#Y = mvrnorm(n = n, mu = mu, Sigma = sigma)

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
# GRAPH FUNCTIONS                                              #
################################################################

# note: this might also work for the posterior as well...
mu_graph = function(mu, type = "prior", col_num,
                    delta,
                    colour_choice = c("blue", "blue"),
                    lty_type = 2,
                    transparency = 0.1){
  # This generates the graph for the prior of the mu, given the number of mu.
  
  mu_values = mu[,col_num]
  # TODO: see latex support later for graphs - was able to for a paper.
  #title = TeX(paste(r'($\mu$)', "sample text for testing", sep = " "))
  title = TeX(paste("Graph of the", type, r'(of $\mu$)', col_num, sep = " "))
  
  xlab_title = TeX(paste(r'($\mu$)', col_num, sep = " "))
  
  rgb_version = col2rgb(colour_choice[1])
  
  hist_col = rgb(rgb_version[1]/255, rgb_version[2]/255, rgb_version[3]/255, 
                alpha = transparency)
  
  # getting the sequence values
  grid = seq_alt(mu_values, delta)
  
  # Plots of the Prior and the Posterior
  hist(mu_values, 
       main = title, ylab = "Densities", xlab = xlab_title, 
       col = hist_col, border = "#ffffff",
       breaks = grid,
       prob = TRUE)
  lines(density(mu_values), lwd = 2, lty = lty_type, col = colour_choice[2])
}

# separate function needs to exist based on the type of what rbr is.
rbr_mu_graph = function(mu, type = "rbr", col_num,
                        grid_vals,
                        colour_choice = c("blue", "blue"),
                        lty_type = 2,
                        transparency = 0.1){
  mu_values = mu[[col_num]]
  mu_values = unlist(mu_values)
  grid = grid_vals[[col_num]]
  grid = unlist(grid)
  
  title = TeX(paste("Graph of the", type, r'(of $\mu$)', col_num, sep = " "))
  
  xlab_title = TeX(paste(r'($\mu$)', col_num, sep = " "))
  
  rgb_version = col2rgb(colour_choice[1])
  
  hist_col = rgb(rgb_version[1]/255, rgb_version[2]/255, rgb_version[3]/255, 
                 alpha = transparency)
  
  # Plotting
  hist(mu_values, 
       main = title, ylab = "Densities", xlab = xlab_title, 
       col = hist_col, border = "#ffffff",
       breaks = grid,
       prob = TRUE)
  lines(density(mu_values, na.rm = TRUE), lwd = 2, lty = lty_type, col = colour_choice[2])
}
  


#rbr_mu_graph(mu = x$rbr, type = "rbr", 1,
#                        grid_vals = x$rbr_sequence,
#                       colour_choice = c("blue", "blue"),
#                        lty_type = 2,
#                        transparency = 0.1)


# need to test below

# double check why mu_0 and sigma_0 are vectors ere..?

#test = sample_multiple_prior(n = 1000, 
#                             alpha01 = x$alpha01, 
#                             alpha02 = x$alpha02, 
#                             mu_0 = x$mu0, 
#                             sigma_0 = x$sigma0)

#test$mu
#test$mu[,1] # this is for mu 1
#plot(density(test$mu[,1]))


#test2 = sample_post(alpha01 = c(2, 2, 2), 
#                    alpha02 = c(4, 4, 4), 
#                    n = 100, N = 100, mu_0 = 0, sigma_0 = 1)
# error with this code...
#x = sample_rbr_mu(test$mu, test2$mu, 0.5)

#delta = 0.5
#min_val = floor(min(test$mu[,1], test2$mu[,1]))
#max_val = ceiling(max(test$mu[,1], test2$mu[,1]))
#grid = seq(min_val, max_val, by = delta)
#if(!(max_val %in% grid) == TRUE){
#  grid = c(grid, max)
#}

#prior_density = hist(test$mu[1,], prob = TRUE, breaks = grid)
#post_density = hist(test2$mu[1,], prob = TRUE, breaks = grid)
#prior_density$mids

#x1 = divison_alt(prior_density$counts, post_density$counts)

#v = matrix(, nrow = 100, ncol = 0) # empty matrix
#cbind(v, rep(0, 100))

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




## TESTING!!!


#p = 3
#mu = rep(0, p) 
#sigma = diag(p) 
#n = 100
#Y = mvrnorm(n = n, mu = mu, Sigma = sigma)

#alpha01 = c(3.149414, 3.149414, 3.149414)
#alpha02 = c(5.748669, 5.748669, 5.748669)
#N = 1000
#mu_0 = 0
#sigma_0 = 2.5
#x = sample_post(alpha01, alpha02, Y, N, mu_0, sigma_0)
