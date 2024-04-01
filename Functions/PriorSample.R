################################################################
# HELPER FUNCTIONS                                             #
################################################################

vnorm = function(x, t){
  # Computes the norm of the matrix x of type t.
  norm(matrix(x, ncol=1), t)
}

onion = function(dimension){
  #' Generating using the onion method from the paper: 
  #' On Bayesian Hotelling's T^{2} test for the mean
  #' @param dimension denotes the number of dimensions of the matrix.
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

sample_prior = function(N, p, alpha01, alpha02, mu0, lambda0){
  #' This represents section 3.1 of the paper.
  #' @param N represents the Monte Carlo sample size.
  #' @param p represents the number of dimensions.
  #' The other parameters match the descriptions in section 2.1.
  
  mu_mat = matrix(NA, nrow = N, ncol = p)
  sigma_ii_mat = matrix(NA, nrow = N, ncol = p)
  sigma_mat = vector("list", length = N)
  covariance_mat = vector("list", length = N)
  correlation_mat = vector("list", length = N)
  
  for(i in 1:N){
    sigma_ii = 1/rgamma(p, alpha01, alpha02)
    D = diag(sigma_ii^2)
    R = onion(p) # the correlation matrix
    Lambda = diag(lambda0)
    SIGMA = D %*% R %*% D
    var_mat = Lambda %*% SIGMA %*% Lambda
    
    MU = mvrnorm(n = 1, mu = mu0, Sigma = var_mat)
    
    # Store results in preallocated matrices/lists
    mu_mat[i,] = MU
    sigma_ii_mat[i,] = sigma_ii
    sigma_mat[[i]] = SIGMA
    covariance_mat[[i]] = var_mat
    correlation_mat[[i]] = R
  }
  
  return(list("mu_matrix" = mu_mat, "sigma_ii" = sigma_ii_mat,
              "sigma_matrix" = sigma_mat,
              "covariance_matrix" = covariance_mat, 
              "correlation_matrix" = correlation_mat))
}

psi = function(mu, xi){
  #' A function the user is supposed to specify, but for now it 
  #' just gives you mu.
  return(mu)
}

true_prior_density = function(p, alpha01, alpha02, lambda0, mu0){
  #' Plots the true prior.
  #' @param p represents the number of dimensions.
  x = seq(-10, 10, length.out = 1001)
  x_vector = matrix(nrow = length(x), ncol = p)
  y_vector = matrix(nrow = length(x), ncol = p)
  for(i in 1:p){
    y = dt(x,2*alpha01[i])
    scale = sqrt(alpha02[i]/alpha01[i])*lambda0[i]
    xnew = mu0[i] + scale*x
    ynew = y/scale
    
    x_vector[, i] = xnew
    y_vector[, i] = ynew
  }
  newlist = list("x_vector" = x_vector, "y_vector" = y_vector)
  return(newlist)
}

find_effective_range = function(p, m, x_vector_matrix, y_vector_matrix,
                                quantile_val = c(0.005,0.995)){
  #' Calculates the effective range.
  #' @param p represents the number of dimensions.
  #' @param m represents the number of desired sub-intervals for the effective range.
  #' @param quantile_val represents the smaller quantile of interest for the effective range.
  #' @details 
  #' This function assumes that m is consistent throughout each mu. 
  desired_range = diff(quantile_val)
  
  x_range_matrix = matrix(nrow = 2, ncol = p)
  y_range_matrix = matrix(nrow = 2, ncol = p)
  delta_vector = numeric(p)
  grid_matrix = list()
  
  for(k in 1:p){
    x_vector = x_vector_matrix[,k]
    y_vector = y_vector_matrix[,k]
    
    area_cum = cumsum(y_vector)
    total_area = compute_area(x_vector, y_vector)
    
    lower_index = max(which(area_cum / total_area <= quantile_val[1]))
    upper_index = min(which(area_cum / total_area >= quantile_val[2]))
    
    x_range = x_vector[c(lower_index, upper_index)]
    y_range = y_vector[c(lower_index, upper_index)]
    
    delta = diff(x_range) / m
    x_grid = seq(x_range[1], x_range[2], by = delta)
    
    x_range_matrix[, k] = x_range
    y_range_matrix[, k] = y_range
    delta_vector[k] = delta
    grid_matrix[[k]] = x_grid
  }
  
  newlist = list("x_range" = x_range_matrix, "y_range" = y_range_matrix,
                 "delta" = delta_vector, "grid" = grid_matrix)
  return(newlist)
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

content_density_plot = function(density, col_num, grid, type = "Prior",
                                min_xlim = -10, max_xlim = 10,
                                smooth_num = 1, colour_choice = "blue",
                                lty_type = 2, transparency = 0.4){
  #' Creates a density plot for the prior/posterior content.
  #' @param density vector containing density values.
  #' @param col_num column number of interest.
  #' @param grid plotted x-values.
  #' @param type indicates whether it is the prior or the posterior (for the graph title).
  #' @param min_xlim smaller cutoff of the plot.
  #' @param max_xlim larger cutoff of the plot.
  #' @param smooth_num number of points to average out the density plot.
  #' @param colour_choice colour for the density plot line.
  #' @param lty_type line type (same values as base R plotting).
  #' @param transparency transparency percentage for the area of the density plot. 
  #'                     Set to 0 if you don't want the area highlighted.
  rgb_col = col2rgb(colour_choice)
  area_col = rgb(rgb_col[1]/255, rgb_col[2]/255, rgb_col[3]/255, 
                 alpha = transparency)
  
  density_vals = average_vector_values(density[,col_num], smooth_num)
  
  plot(grid[,col_num], density_vals,
       xlim = c(min_xlim, max_xlim), col = colour_choice,
       main = TeX(paste("Sample ", type, " Density Histogram of $\\mu_{", col_num, "}$")),
       xlab = TeX(paste("Value of $\\mu_{", col_num, "}$")),
       ylab = "Density",
       type = "l", lty = lty_type, lwd = 2)
  
  polygon(grid[, col_num], density_vals, col = area_col, border = NA)
}

################################################################
# OLD FUNCTIONS                                                #
################################################################

prior_content = function(N, p, m, mu, xi, 
                         small_quantile = 0.005, large_quantile = 0.995){
  #' Provides the prior content of the sample.
  #' @param N represents the Monte Carlo sample size.
  #' @param p represents the number of dimensions.
  #' @param m represents the number of desired sub-intervals for the effective range.
  #' @param small_quantile represents the smaller quantile of interest for the effective range.
  #' @param large_quantile represents the larger quantile of interest for the effective range.
  #' @details 
  #' This function assumes that m is consistent throughout each mu. 
  psi_val = psi(mu, xi) 
  plotting_grid = c()
  effective_range = c()
  prior_content_matrix = c()
  prior_density_matrix = c()
  
  for(k in 1:p){
    # first: gathering the effective range
    quant_range = quantile(psi_val[,k], prob = c(small_quantile, large_quantile))
    delta = (quant_range[2] - quant_range[1])/m # length of the sub intervals
    grid = seq(quant_range[1], quant_range[2], by = delta) # making a grid of the sub intervals
    # Note: since we're looking at the effective range, the prior content doesn't always sum to 1.
    effective_range = cbind(effective_range, grid)
    plot_grid = grid[-length(grid)] + diff(grid)/2
    plotting_grid = cbind(plotting_grid, plot_grid)
    
    prior_content_vec = rep(0, m)
    for(j in 1:N){
      for(i in 1:m){
        if(between(psi_val[,k][j], grid[i], grid[i+1])){ # CHANGES HERE
          prior_content_vec[i] =  prior_content_vec[i] + 1
          break
        }
      }
    }
    
    prior_content_vec = prior_content_vec/N
    prior_density = prior_content_vec / delta
    prior_content_matrix = cbind(prior_content_matrix, prior_content_vec)
    prior_density_matrix = cbind(prior_density_matrix, prior_density)
  }
  newlist = list("plotting_grid" = plotting_grid,
                 "effective_range" = effective_range,
                 "prior_content" = prior_content_matrix,
                 "prior_density" = prior_density_matrix,
                 "delta" = as.numeric(delta))
  return(newlist)
}

sample_prior_hist = function(mu_prior, col_num, delta = 0.1, 
                             min_xlim = -10, max_xlim = 10,
                             smooth_num = 1, colour_choice = "blue",
                             lty_type = 2, transparency = 0.4){
  # WARNING: this one is outdated and doesn't deal with the prior content
  # as explained through mike. But it plots the sampled prior!
  rgb_col = col2rgb(colour_choice)
  area_col = rgb(rgb_col[1]/255, rgb_col[2]/255, rgb_col[3]/255, 
                 alpha = transparency)
  
  max_val = ceiling(max(mu_prior[,col_num]))
  min_val = floor(min(mu_prior[,col_num]))
  
  grid = seq(min_val, max_val, by = delta)
  
  hist = hist(mu_prior[,col_num], breaks = grid, prob = TRUE, 
              xlim = c(min_xlim, max_xlim), border = "white", col = area_col,
              main = TeX(paste("Sample Prior Density Histogram of $\\mu_{", col_num, "}$")),
              xlab = TeX(paste("Value of $\\mu_{", col_num, "}$")))
  
  lines(hist$mids, average_vector_values(hist$density, smooth_num), 
        xlim = c(min_xlim, max_xlim), lty = lty_type, col = colour_choice)
}

