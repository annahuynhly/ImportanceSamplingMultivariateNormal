################################################################
# MAIN FUNCTIONS                                               #
################################################################

true_prior_comparison = function(p, alpha01, alpha02, mu0, lambda0, grid) {
  #' Given a grid of values (typically where the posterior density is allegedly 
  #' concentrated), computes the true prior density.
  #' This is used for graph building and computing the relative belief ratio.
  #' @param p represents the number of dimensions.
  #' @param grid represents a list of the grid of values where the posterior is based off of.
  #' The other parameters match the descriptions from the paper.
  
  prior_matrix = matrix(0, nrow = length(grid[[1]]) - 1, ncol = p)
  midpt_grid_matrix = matrix(0, nrow = length(grid[[1]]) - 1, ncol = p)
  
  for (i in 1:p) {
    midpt_grid = grid[[i]][-length(grid[[i]])] + diff(grid[[i]]) / 2
    
    scale = sqrt(alpha02[i] / alpha01[i]) * lambda0[i]
    reg_x = (midpt_grid - mu0[i]) / scale
    y = dt(reg_x, 2 * alpha01[i]) / scale
    
    prior_matrix[, i] = y
    midpt_grid_matrix[, i] = midpt_grid
  }
  
  newlist = list("prior_matrix" = prior_matrix,
                  "midpoint_grid_matrix" = midpt_grid_matrix)
  return(newlist)
}

sample_sigma_ii = function(N, p, alpha01, alpha02){
  #' Generates a sample of sigma_ii, without having the sample the entire prior.
  #' @param N represents the Monte Carlo sample size.
  #' @param p represents the number of dimensions.
  #' #' The other parameters match the descriptions from the paper.
  sigma_ii_matrix = 1/rgamma(N * p, alpha01, alpha02)
  sigma_ii = matrix(sigma_ii_matrix, nrow = N, ncol = p, byrow = TRUE)
  return(sigma_ii)
}

sample_post_computations = function(N, Y, p, mu0, lambda0){
  #' This represents section 3.2 of the paper, theorem 4.
  #' @param N represents the Monte Carlo sample size.
  #' @param Y represents the observed sample.
  #' @param p represents the number of dimensions.
  #' The other parameters match the descriptions in section 3.2.
  
  if((p != length(mu0)) & (p != length(lambda0))){
    return("Error: the vector for mu0 and lambda0 are of a different size.")
  }
  
  if(is.numeric(Y) == TRUE){
    n = nrow(Y)
    if(n < (2*p)){
      return("Error: the value of n (size of Y) is too small.")
    }
    Yprime = t(Y)
    Ybar = rowMeans(Yprime) # rowMeans(t(Y))
    In = matrix(t(rep(1, n))) # identity column
    Ybar_t = matrix(Ybar, nrow=1, ncol = p) # transpose
    
    S = t(Y - In%*%rowMeans(t(Y))) %*% (Y - In%*%rowMeans(t(Y))) 
  } else {
    return("Error: no data given.")
  }
  
  lambda0 = max(lambda0)
  
  Sigma_Y = find_inverse_alt((S + n/(1 + n * lambda0^2) * (rowMeans(t(Y)) - mu0) %*% t(rowMeans(t(Y)) - mu0)))
  mu_Y = ((n + 1/lambda0^2)^-1) * (mu0/lambda0^2 + n * rowMeans(t(Y)))
  xi = rWishart(n = N, df = (n - p - 1), Sigma = Sigma_Y) # See Eq 13
  
  mu_xi = matrix(NA, nrow = N, ncol = p)
  for(k in 1:N){
    mu_Sigma = ((n + 1/lambda0^2)^-1) * find_inverse_alt(xi[,,k])
    mu_xi[k,] = mvrnorm(n = 1, mu = mu_Y, Sigma = mu_Sigma) # See Eq 13 (mu conditional on xi)
  }
  
  return(list("xi" = xi, "mu_xi" = mu_xi))
}

k = function(p, mu, xi, mu0, lambda0, sigma_ii, alpha01, alpha02){
  #' This represents the k function explained in theorem 2.
  #' @param p represents the number of dimensions.
  #' The other parameters match the descriptions in the paper.
  lambda02=(max(lambda0))**2
  Lambda0 = diag(lambda0)
  inv_Lambda0 = find_inverse_alt(Lambda0)
  logk = -(1/2) * t(mu - mu0) %*% (inv_Lambda0 %*% xi %*% inv_Lambda0 - (1/lambda02)*xi) %*% (mu - mu0)
  x2 = 1
  for(i in 1:p){
    x2 = x2 * (1/sigma_ii[i])^(alpha01[i] + (p+1)/2) * exp(-alpha02[i]/sigma_ii[i])
  }
  return(exp(logk)*x2)
}

weights = function(N, p, mu, xi, mu0, lambda0, sigma_ii, alpha01, alpha02){
  #' Computes the weights given for the posterior content.
  #' @param N represents the Monte Carlo sample size.
  #' @param p represents the number of dimensions.
  #' The other parameters match the descriptions in the paper.
  k_vector = numeric(N)
  for(i in 1:N){
    k_vector[i] = k(p, mu[i,], xi[,,i], mu0, lambda0, sigma_ii[i,], alpha01, alpha02)
  }
  weights_vector = k_vector / sum(k_vector)
  return(weights_vector)
}

sample_post_reformat = function(N, p, post_mu, post_xi, weights){
  #' Reformats the data for the user to download.
  #' @param N represents the Monte Carlo sample size.
  #' @param p represents the number of dimensions.
  #' @param post_mu represents the mu from integrating w.r.t. the posterior.
  #' @param post_xi represents the xi from integrating w.r.t. the posterior.
  #' @param weights represents the weights calculated from post_mu and post_xi.
  
  mu_title = paste("Mu_", 1:p, sep = "")
  weights_title = "Weights"
  
  xi_title = character(p*(p+1)/2)  # Preallocate memory for the vector
  k = 1
  for(i in 1:p){
    for(j in i:p){
      xi_title[k] = paste("Xi_", i, j, sep = "")
      k = k + 1
    }
  }
  xi_matrix = matrix(NA, nrow = N, ncol = length(xi_title))
  for(i in 1:N){ 
    indices = which(upper.tri(post_xi[,,i], diag=TRUE), arr.ind=TRUE)
    new_row = post_xi[,,i][indices[order(indices[,1]),]]
    xi_matrix[i,] = as.vector(new_row)
  }
  
  weights_data = as.data.frame(weights)
  mu_data = as.data.frame(post_mu)
  xi_matrix = as.data.frame(xi_matrix)
  
  names(weights_data) = weights_title
  names(mu_data) = mu_title
  names(xi_matrix) = xi_title
  
  result = cbind(weights_data, mu_data, xi_matrix)
  rownames(result) = 1:N
  
  return(result)
}

posterior_content = function(N, p, effective_range, mu, xi, weights) {
  #' Computes the posterior content.
  #' @param N represents the Monte Carlo sample size.
  #' @param p represents the number of dimensions.
  #' @param effective_range denotes a list of grid points where the density
  #'        is highly concentrated (this is computed from the sampling of the  prior).'
  #' @param weights denote the weights given, calculated from the psi.
  #'        The other parameters match the descriptions in the paper.
  
  psi_val = psi(mu, xi)
  post_content_matrix = matrix(0, nrow = length(effective_range[[1]]) - 1, ncol = p)
  post_density_matrix = matrix(0, nrow = length(effective_range[[1]]) - 1, ncol = p)
  
  for (k in 1:p) {
    grid = effective_range[[k]]
    delta = diff(grid)[1]
    post_content_vec = numeric(length(grid) - 1)
    
    for (i in 1:(length(grid) - 1)) {
      when_true <- psi_val[, k] >= grid[i] & psi_val[, k] < grid[i + 1]
      post_content_vec[i] = sum(weights[when_true])
    }
    
    post_density = post_content_vec / delta
    post_content_matrix[, k] = post_content_vec
    post_density_matrix[, k] = post_density
  }
  
  newlist <- list("post_content" = post_content_matrix,
                  "post_density" = post_density_matrix)
  return(newlist)
}

relative_belief_ratio = function(p, prior_content, post_content) {
  #' Computes the relative belief ratio.
  #' @param p represents the number of dimensions.
  #' @param prior_content denotes the vector containing the prior.
  #' @param post_content denotes the vector containing the posterior.
  
  rbr_vector = post_content / prior_content
  rbr_vector_mod = ifelse(is.na(rbr_vector), 0, rbr_vector)
  
  newlist = list("RBR" = rbr_vector, "RBR_modified" = rbr_vector_mod)
  return(newlist)
}

################################################################
# GRAPH FUNCTIONS                                              #
################################################################

comparison_content_density_plot = function(prior_density, post_density, col_num, 
                                           prior_grid, post_grid,
                                           min_xlim = -10, max_xlim = 10,
                                           smooth_num = c(1, 1), 
                                           colour_choice = c("red", "blue"),
                                           lty_type = c(2, 2), transparency = 0.4){
  #' Creates a density plot for the prior/posterior content.
  #' @param prior_density vector containing prior density values.
  #' @param post_density vector containing the posterior density values.
  #' @param col_num column number of interest.
  #' @param prior_grid plotted x-values for the prior.
  #' @param post_grid plotted x-values for the posterior.
  #' @param min_xlim smaller cutoff of the plot.
  #' @param max_xlim larger cutoff of the plot.
  #' @param smooth_num vector containing the number of points to average out the density plot.
  #'        The first value is for the prior, and the second is for the posterior.
  #' @param colour_choice vector containing colour for the density plot line.
  #'        The first value is for the prior, and the second is for the posterior.
  #' @param lty_type vector containing line type (same values as base R plotting).
  #'        The first value is for the prior, and the second is for the posterior.
  #' @param transparency transparency percentage for the area of the density plot. 
  #'                     Set to 0 if you don't want the area highlighted.
  prior_col_rgb = col2rgb(colour_choice[1])
  post_col_rgb = col2rgb(colour_choice[2])
  
  prior_area_col = rgb(prior_col_rgb[1]/255, prior_col_rgb[2]/255, prior_col_rgb[3]/255, 
                       alpha = transparency)
  post_area_col = rgb(post_col_rgb[1]/255, post_col_rgb[2]/255, post_col_rgb[3]/255, 
                      alpha = transparency)
  
  prior_density_vals = average_vector_values(prior_density[,col_num], smooth_num[1])
  post_density_vals = average_vector_values(post_density[,col_num], smooth_num[2])
  
  max_ylim = max(c(max(prior_density_vals), max(post_density_vals)))
  
  plot(prior_grid[,col_num], prior_density_vals,
       xlim = c(min_xlim, max_xlim), 
       ylim = c(0, max_ylim),
       col = colour_choice[1],
       main = TeX(paste("Prior & Posterior Density Histogram of $\\mu_{", col_num, "}$")),
       xlab = TeX(paste("Value of $\\mu_{", col_num, "}$")),
       ylab = "Density",
       type = "l", lty = lty_type[1], lwd = 2)
  
  lines(post_grid[,col_num], post_density_vals, 
        lty = lty_type[2], lwd = 2, col = colour_choice[2])
  
  polygon(prior_grid[, col_num], prior_density_vals, col = prior_area_col, border = NA)
  polygon(post_grid[, col_num], post_density_vals, col = post_area_col, border = NA)
  
  legend("topleft", legend=c("Prior", "Posterior"),
         col= colour_choice, lty=lty_type, cex=0.8)
}

################################################################
# OLD FUNCTIONS                                                #
################################################################

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

# note: below is the old function. It is being replaced.
compute_rbr = function(gamma, delta, alpha01, alpha02, m1, m2, mu_post, min_xlim, max_xlim){
  # min_xlim: minimum values for the x-limit
  # max_xlim: maximum values for the x-limit
  
  p = length(alpha01)
  mu_prior = sample_hyperparameters(gamma, alpha01, alpha02, m1, m2)
  mu_prior_matrix = c()
  mu_post_matrix = c()
  rbr_matrix = c()
  grid_matrix = c()
  
  for(i in 1:p){
    grid = seq(min_xlim, max_xlim, by = delta)
    #grid = seq_alt(mu_post[, i], delta)
    
    # mostly to obtain the midpoints used for the graph
    post_plot = hist(mu_post[, i], breaks = grid, plot = FALSE)
    new_grid = post_plot$mids 
    grid_matrix = cbind(grid_matrix, new_grid)
    mu_post_matrix = cbind(mu_post_matrix, post_plot$density)
    
    # getting the values from the prior to compare to the posterior
    x1 = mu_prior$mu0[i]
    scale = sqrt(alpha02[i]/alpha01[i]) * mu_prior$lambda0[i]
    t_dist = dt(new_grid, df = 2 * alpha01[i])  #instead of new_grid
    
    mu_prior_matrix = cbind(mu_prior_matrix, t_dist/scale)
    
    # obtaining the relative belief ratio
    rbr_matrix = cbind(rbr_matrix, post_plot$density/mu_prior_matrix[,i])
  }
  
  newlist = list("grid" = grid_matrix, "prior_mu" = mu_prior_matrix, 
                 "post_mu" = mu_post_matrix, "rbr_mu" = rbr_matrix)
  
  return(newlist)
}

# FUNCTIONS FOR GRAPHING!!

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
                               #min_xlim = -10, max_xlim = 10,
                               smooth_num = 1,
                               colour_choice = c("blue", "red"),
                               lty_type = c(2, 2),
                               transparency = 0.1){
  # note: temporary name, but grid is referring to the grid of the posterior
  # grid2 is referring to the grid of the prior
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
  
  # previous - commented out.
  # first plotting the prior
  plot(grid[, col_num], 
       mu_prior[, col_num],
       type = "l", 
       lty = lty_type[1],
       col = colour_choice[1],
       ylim = c(min_val, max_val), 
       #xlim = c(min_xlim, max_xlim),
       main = TeX(paste("Graph of the Prior and Posterior of $\\mu_{", col_num, "}$")),
       ylab = "Density",
       xlab = TeX(paste("Value of $\\mu_{$", col_num, "}$")))
  # second plotting the posterior
  lines(grid[, col_num], 
        average_vector_values(mu_post[, col_num], smooth_num),
        type = "l", col = colour_choice[2],
        lty = lty_type[2])
  
  # adding the area under the graph plot
  polygon(grid[, col_num], force_bounds_0(mu_prior[, col_num]), 
          col = prior_area_col, border = NA)
  polygon(grid[, col_num], average_vector_values(mu_post[, col_num], smooth_num), 
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
