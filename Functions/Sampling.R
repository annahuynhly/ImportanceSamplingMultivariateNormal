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

# this graph shouldn't be necessary; we don't need to draw samples since
# we know the true distribution.
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

sample_hyperparameters = function(gamma, alpha01, alpha02, m1, m2){
  lambda0 = (m2 - m1)/(2 * sqrt(alpha02/alpha01) * qt((1 + gamma)/2, df = 2 * alpha01))
  mu0 = (m2 + m1)/2
  newlist = list("mu0" = mu0, "lambda0" = lambda0)
  return(newlist)
}

prior_true_mu = function(gamma, alpha01, alpha02, m1, m2){
  # idea: no need to sample since the distribution is actually known.
  # n represents length here
  data = sample_hyperparameters(gamma, alpha01, alpha02, m1, m2)
  mu0 = data$mu0
  lambda0 = data$lambda0
  # assuming alphas have the same length
  n = length(alpha01)
  mu = rep(0, n)
  for(i in 1:n){
    t_val = qt(gamma, df = alpha01[i] * 2)
    mu[i] = mu0[i] + sqrt(alpha02[i]/alpha01[i]) * lambda0[i] * t_val
  }
  return(mu)
}

sample_post_new = function(N, Y, gamma, alpha01, alpha02, m1, m2){
  data = sample_hyperparameters(gamma, alpha01, alpha02, m1, m2)
  mu0 = data$mu0
  lambda0 = max(data$lambda0)
  # replace the new name later; this is to distinguish between the earlier version.
  if(length(alpha01) != length(alpha02)){
    # may want to turn into a helper function for readability later on?
    return("Error: the vector for alpha_01 and beta_02 are of a different size.")
  }
  p = length(alpha01)
  if(is.numeric(Y) == TRUE){
    n = nrow(Y)
    if(n < (2*p)){
      return("Error: the value of n (size of Y) is too small.")
    }
    Yprime = t(Y)
    Ybar = rowMeans(Yprime) # rowMeans(t(Y))
    In = matrix(t(rep(1, n))) # identity column
    Ybar_t = matrix(Ybar,nrow=1, ncol = p) # transpose
    # NOTE: need to check if S was computed correctly... different in his notes than this
    # version
    #S = (1/(n-1)) * t(Y - In%*%Ybar_t) %*% (Y - In%*%Ybar_t)
    S = (1/(n-1)) * t(Y - In%*%rowMeans(t(Y))) %*% (Y - In%*%rowMeans(t(Y))) # paper version - may be incorrect?
  } else {
    return("Error: no data given.")
  }
  
  # instead of using solve, may need to move to an alt version (see helper functions)
  Sigma_Y = find_inverse_alt((S + n/(1 + n * lambda0^2) * (rowMeans(t(Y)) - mu0) %*% t(rowMeans(t(Y)) - mu0)))
  mu_Y = ((n + 1/lambda0^2)^-1) * (mu0/lambda0^2 + n * rowMeans(t(Y)))
  mu_Sigma = ((n + 1/lambda0^2)^-1) * find_inverse_alt(Sigma_Y)
  
  xi = rWishart(n = N, df = (n - p - 1), Sigma = Sigma_Y)
  mu_xi = mvrnorm(n = N, mu = mu_Y, Sigma = mu_Sigma)
  
  return(list("xi" = xi, "mu_xi" = mu_xi))
}

sample_rbr_new = function(gamma, delta, alpha01, alpha02, m1, m2, mu_post){
  
  p = length(alpha01)
  mu_prior = sample_hyperparameters(gamma, alpha01, alpha02, m1, m2)
  mu_prior_matrix = c()
  mu_post_matrix = c()
  rbr_matrix = c()
  grid_matrix = c()
  
  for(i in 1:p){
    grid = seq_alt(mu_post[, i], delta)
    
    # mostly to obtain the midpoints used for the graph
    post_plot = hist(mu_post[, i], breaks = grid, plot = FALSE)
    new_grid = post_plot$mids 
    grid_matrix = cbind(grid_matrix, new_grid)
    mu_post_matrix = cbind(mu_post_matrix, post_plot$density)
    
    # getting the values from the prior to compare to the posterior
    x1 = mu_prior$mu0[i]
    x2 = sqrt(alpha01[i]/alpha02[i])
    x3 = mu_prior$lambda0[i]
    t_dist = dt(new_grid, df = 2 * alpha01[i])  
    
    mu_prior_matrix = cbind(mu_prior_matrix, x1 + x2 * x3 * t_dist)
    
    # obtaining the relative belief ratio
    rbr_matrix = cbind(rbr_matrix, post_plot$density/mu_prior_matrix[,i])
  }
  
  newlist = list("grid" = grid_matrix, "prior_mu" = mu_prior_matrix, 
                 "post_mu" = mu_post_matrix, "rbr_mu" = rbr_matrix)
  
  return(newlist)
}

################################################################
# GRAPH FUNCTIONS                                              #
################################################################

# note: this might also work for the posterior as well...
mu_graph = function(mu, type = "prior", col_num,
                    delta, smooth_num = 1,
                    colour_choice = c("blue", "blue"),
                    lty_type = 2,
                    transparency = 0.1){
  # This generates the graph for the prior of the mu, given the number of mu.
  
  mu_values = mu[,col_num]
  # TODO: see latex support later for graphs - was able to for a paper.
  #title = TeX(paste(r'($\mu$)', "sample text for testing", sep = " "))
  title = TeX(paste("Graph of the", type, "of $\\mu_{", col_num, "}$"))
  
  xlab_title = TeX(paste("$\\mu_{", col_num, "}$", sep = ""))
  
  rgb_version = col2rgb(colour_choice[1])
  
  hist_col = rgb(rgb_version[1]/255, rgb_version[2]/255, rgb_version[3]/255, 
                alpha = transparency)
  
  # getting the sequence values
  grid = seq_alt(mu_values, delta)
  
  # Plots of the Prior and the Posterior
  mu_plot = hist(mu_values, breaks = grid,
                 main = title, ylab = "Densities", xlab = xlab_title, 
                 col = hist_col, border = "#ffffff", prob = TRUE)
  
  if(smooth_num == 1){
    lines(density(mu_values), lwd = 2, lty = lty_type, col = colour_choice[2])
  } else {
    line_plot_vals = average_vector_values(mu_plot$density, smooth_num)
    lines(mu_plot$mids, line_plot_vals, 
          lwd = 2, lty = lty_type, col = colour_choice[2])
  }
  
}

mu_graph_comparison = function(grid, mu_prior, mu_post, col_num,
                               smooth_num = 1,
                               colour_choice = c("blue", "red"),
                               lty_type = c(2, 2),
                               transparency = 0.1){
  # note: assumes that we have the mu_prior and mu_post from the other
  # function (sample_rbr_new)
  
  rgb_prior = col2rgb(colour_choice[1])
  rgb_post = col2rgb(colour_choice[2])
  
  prior_area_col = rgb(rgb_prior[1]/255, rgb_prior[2]/255, rgb_prior[3]/255, 
                       alpha = transparency)
  post_area_col = rgb(rgb_post[1]/255, rgb_post[2]/255, rgb_post[3]/255, 
                      alpha = transparency)
  
  # makes a graph that compares the prior and the posterior.
  max_val = max(c(max(mu_prior[,col_num]), max(mu_post[,col_num])))
  min_val = min(c(min(mu_prior[,col_num]), min(mu_post[,col_num])))
  
  # first plotting the prior
  plot(grid[, col_num], 
       mu_prior[, col_num],
       type = "l", 
       lty = lty_type[1],
       col = colour_choice[1],
       ylim = c(min_val, max_val), 
       main = TeX(paste("Graph of the Prior and Posterior of $\\mu_{", col_num, "}$")),
       ylab = "Density",
       xlab = TeX(paste("Value of $\\mu_{$", col_num, "}$")))
  # second plotting the posterior
  lines(grid[, col_num], 
        average_vector_values(mu_post[, col_num], smooth_num),
        type = "l", col = colour_choice[2],
        lty = lty_type[2])
  
  # adding the area under the graph plot
  #polygon(grid[, col_num], 
  #        force_bounds_0(mu_prior[, col_num]), 
  #        col = prior_area_col, border = NA)
  polygon(grid[, col_num], 
          average_vector_values(mu_post[, col_num], smooth_num), 
          col = post_area_col, border = NA)
}

# separate function needs to exist based on the type of what rbr is.
rbr_mu_graph = function(grid, mu, type = "rbr", col_num,
                        smooth_num = 1,
                        colour_choice = "darkgreen",
                        lty_type = 2,
                        transparency = 0.1){
  title = TeX(paste("Graph of the", type, "of $\\mu_{", col_num, "}$"))
  
  rbr_rgb = col2rgb(colour_choice)
  
  rbr_area_col = rgb(rbr_rgb[1]/255, rbr_rgb[2]/255, rbr_rgb[3]/255, 
                    alpha = transparency)
  
  # first plotting the prior
  plot(grid[, col_num], 
       average_vector_values(mu[, col_num], smooth_num),
       type = "l", 
       lty = lty_type,
       col = colour_choice,
       main = title, ylab = "Density",
       xlab = TeX(paste("Value of $\\mu_{$", col_num, "}$")))
  
  # adding the area under the graph plot
  polygon(grid[, col_num], 
          average_vector_values(mu[, col_num], smooth_num),
          col = rbr_area_col, border = NA)
}

#gamma = 0.05
#alpha01 = c(3.149414, 3.149414, 3.149414)
#alpha02 = c(5.748669, 5.748669, 5.748669)
#m1 = c(2, 2, 2)
#m2 = c(10, 10, 10)

#mu0 = 0
#lambda0 = 2.5
#N = 1000

#test = sample_prior_new(N, alpha01, alpha02, mu0, lambda0)


#p = 3
#mu = rep(0, p) 
#sigma = diag(p) 
#n = 100
#Y = mvrnorm(n = n, mu = mu, Sigma = sigma)

#test = sample_post_new(N, Y, gamma, alpha01, alpha02, m1, m2)

#mu_values = test$mu_xi
#delta = 0.05

#test2 = sample_rbr_new(gamma, delta, alpha01, alpha02, m1, m2, mu_post = mu_values)

#mu_graph_comparison(grid = test2$grid, 
#                    mu_prior = test2$prior_mu, 
#                    mu_post = test2$post_mu,  
#                    col_num = 3,
#                    colour_choice = c("blue", "red"),
#                    lty_type = 2,
#                    transparency = 0.1)

#mu_plot = hist(mu_values, breaks = grid, prob = TRUE)

#mu_plot$mids # note: this should be the grid
#mu_plot$density

# getting the values from the prior to compare to the posterior
# should match the posterior values.
#mu_prior = sample_hyperparameters(gamma, alpha01, alpha02, m1, m2)
#mu_prior_dist = c()
#for(i in 1:length(alpha01)){
#  x1 = mu_prior$mu0[i]
#  x2 = sqrt(alpha01[i]/alpha02[i])
#  x3 = mu_prior$lambda0[i]
#  t_dist = dt(mu_plot$mids, df = 2 * alpha01[i])  
#  mu_prior_dist = cbind(mu_prior_dist, x1 + x2 * x3 * t_dist)
#}


# getting the relative belief ratio
#rbr = mu_plot$density/mu_prior_dist[,1]
#lines(mu_plot$mids, rbr)



#example = mu_graph(mu = x$mu, type = "posterior", col_num = 1,
#         delta = 0.05, smooth_num = 3,
#         colour_choice = c("blue", "blue"),
#         lty_type = 2,
#         transparency = 0.1)

#lines(example$mids, c(example$density))
