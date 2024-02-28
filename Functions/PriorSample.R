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

sample_prior = function(N, p, alpha01, alpha02, mu0, lambda0){
  # p: number of columns of alpha01, alpha02, mu0, lambda0,
  # N: monte carlo sample size
  mu_mat = c()
  sigma_mat = list()
  covariance_mat = list() 
  sigma_ii_mat = c()
  
  R = onion(p) # the correlation matrix
  Lambda = diag(lambda0)
  
  for(i in 1:N){
    sigma_ii = 1/rgamma(p, alpha01, alpha02)
    D = diag(sigma_ii^2)
    SIGMA = D %*% R %*% D

    var_mat = Lambda %*% SIGMA %*% Lambda
    
    MU = mvrnorm(n = 1, mu = mu0, Sigma = var_mat)
    
    mu_mat = rbind(mu_mat, MU)
    sigma_ii_mat = rbind(sigma_ii_mat, sigma_ii)
    sigma_mat[[i]] = SIGMA
    covariance_mat[[i]] = var_mat
  }
  return(list("mu_matrix" = mu_mat, "sigma_ii" = sigma_ii_mat,
              "sigma_matrix" = sigma_mat,
              "covariance_matrix" = covariance_mat, "correlation_matrix" = R))
}

psi = function(mu, xi){
  # user should modify this. for now, we have a degenerate function
  return(mu)
}

prior_content = function(N, p, m, mu, xi, 
                         small_quantile = 0.005, large_quantile = 0.995){
  # first part: denote psi(mu, xi)
  # m: number of sub intervals 
  # (note: assumption is that the num of time intervals are the same for each mu1 - may need to change
  # this later if it poses an issue...
  
  # CHANGE HERE
  psi_val = psi(mu, xi) # note: the user will need to manually change this
  plotting_grid = c()
  effective_range = c()
  prior_content_matrix = c()
  prior_density_matrix = c()
  
  for(k in 1:p){
    # first: gathering the effective range
    quant_range = quantile(psi_val[,k], prob = c(small_quantile, large_quantile))
    delta = (quant_range[2] - quant_range[1])/m # length of the sub intervals
    grid = seq(quant_range[1], quant_range[2], by = delta) # making a grid of the sub intervals
    # note: since we're looking at the effective range, the prior content doesn't sum to 1.
    
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

content_density_plot = function(density, col_num, grid, type = "Prior",
                                min_xlim = -10, max_xlim = 10,
                                smooth_num = 1, colour_choice = "blue",
                                lty_type = 2, transparency = 0.4){
  
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

# NOTE: currently not using this one!
sample_prior_hist = function(mu_prior, col_num, delta = 0.1, 
                             min_xlim = -10, max_xlim = 10,
                             smooth_num = 1, colour_choice = "blue",
                             lty_type = 2, transparency = 0.4){
  # WARNING: this one is outdated and doesn't deal with the prior content
  # as explained through mike.
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

################################################################
# TESTING                                                      #
################################################################

# inputs
#p = 3
#alpha01 = c(3, 3, 3)
#alpha02 = c(6, 6, 6)
#mu0 = c(0, 0, 0)
#lambda0 = c(1, 1, 1)

#N = 1000 # monte carlo 
#test = sample_prior(N, p, alpha01, alpha02, mu0, lambda0)

#vals = prior_content(N, p, m = 50, mu = test$mu_matrix, xi = find_inverse_alt(test$covariance_matrix[,,1]))

#plot(vals$plotting_grid[,1],
#     vals$prior_content_matrix[,1], type = "l")

#test$sigma_matrix[[1]]

#test$mu_matrix[,1]

#test2 = test$mu_matrix

#grid = seq(-1700, 1700, by = 1)

#hist = hist(test$mu_matrix[,1], breaks = grid, prob = T, xlim = c(-20, 20),
#            border = "white", col = area_col)

#hist$mids

#lines(hist$mids, hist$density, lty = 2)

# the graph verson
#grid = seq(-20, 20, by = 0.01)

#mu0_vectors = list()
#for(i in 1:p){
#  mu0_vectors[[i]] = mu0[i] + sqrt(alpha02[i]/alpha01[i]) * lambda0[i] * dt(grid, df = 2 * alpha01[i])
#}
#mu0_vectors[[1]]
#plot(grid, mu0_vectors[[1]], xlim = c(-20, 20), type = "l")








