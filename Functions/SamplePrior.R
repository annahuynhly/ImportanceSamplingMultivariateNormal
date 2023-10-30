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
  
  newlist = list("mu" = mu, "sigma" = SIGMA, "R" = R)
  
  return(newlist)
}

sample_multiple_prior = function(n, alpha01, alpha02, mu_0, sigma_0){
  mu_vectors = c()
  sigma_vectors = list()
  R_vectors = list() # recall, R is from the onion method
  for(i in 1:n){
    sample = sample_prior(alpha01, alpha02, mu_0, sigma_0)
    mu_vectors = rbind(mu_vectors, sample$mu)
    sigma_vectors[[i]] = sample$sigma
    #print(sample$R) # debugging
    R_vectors[[i]] = sample$R
  }
  newlist = list("mu" = mu_vectors, "sigma" = sigma_vectors, 
                 "R" = R_vectors)
  return(newlist)
}

sample_post = function(alpha01, alpha02, n, N, mu_0, sigma_0){
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

# warning: will need to test the following function below.
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
# ELICITING FROM THE PRIOR                                     #
################################################################

elicit_mu = function(alpha, beta, gamma, s1, s2){
  # This function finds the two values of gamma which we are subtracting.
  
  prob1 = (1+gamma)/2
  prob2 = (1-gamma)/2
  G = matrix(0,2,1)
  
  G[1] = qgamma(prob1, alpha, beta) - (qnorm(prob1)/s1)^2
  G[2] = qgamma(prob2, alpha, beta) - (qnorm(prob1)/s2)^2
  
  return(G)  
}

elicit_sigma = function(gamma, s1, s2, alphaup, alphalow){
  # gamma: probability corresponding to virtual certainty
  # alphaup: bounds on alpha in the gamma_rate(alpha, beta) dist.
  p = gamma # will switch all notations later!
  gam = (1+p)/2
  z0 = qnorm(gam,0,1)
  up = (z0/s1)**2
  low = (z0/s2)**2
  
  # iterate until prob content of s1<= sigma*z0 <= s2 is within eps of p - HARDCODED for now.
  eps = .0001
  maxits = 100
  
  for (i in 1:maxits){
    alpha = (alphalow + alphaup)/2
    beta = qgamma(gam, alpha, 1)/up
    test = pgamma(beta*low, alpha, 1)
    if (abs(test-(1-gam)) <= eps) {
      break 
    }
    if(test < 1 - gam){
      alphaup = alpha
    } else if (test > 1 - gam){
      alphalow = alpha
    }
  }
  newlist = list("up" = up, "low" = low, "alpha" = alpha, "beta" = beta, "z0" = z0)
  return(newlist)
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

prior_elicitation_mu = function(gamma, m1, m2, const = FALSE, s1 = FALSE, s2 = FALSE){
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
      
      fx = elicit_mu(x[1,j], x[2,j], gamma, s1[j], s2[j])
      Jx = matrix(0,2,2)
      
      for(i in 1:2){
        xh = x # estimation values
        xh[i,j] = x[i,j] + h
        
        fxh = elicit_mu(xh[1,j], xh[2,j], gamma, s1[j], s2[j]) # estimated
        Jx[,i] = (fxh - fx)/h 
      }
      
      delta[,j] = -solve(Jx) %*% elicit_mu(x[1,j], x[2,j], gamma, s1[j], s2[j])  
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
#test = sample_multiple_prior(n = 100, alpha01 = c(2, 2, 2), 
#                      alpha02 = c(4, 4, 4), mu_0 = 0, 
#                      sigma_0 = 1)
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
