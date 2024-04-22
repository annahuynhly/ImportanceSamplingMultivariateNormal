# If you haven't installed any of the packages yet, comment out below.
#install.packages("latex2exp")
#install.packages("MASS")
#install.packages("pracma")
#install.packages("dplyr")
library(latex2exp) # for LaTeXing in the graphs
library(MASS) # Used for mvrnorm (generating from a multinormal dist)
library(pracma) # Used for trapz (computing area of histogram)
library(dplyr) # Used for between

##################################################
# Inputs                                         #
##################################################

# Mandatory manual inputs
p = 5
gamma = 0.99
N = 100000 # monte carlo sample size (TODO: CHANGE BACK LATER...)
m = 30 # number of sub-intervals (oddly consistent all around)

# Manual input (data version) #####
s1 = c(1, 0.5, 0.2, 0.5, 1)
s2 = c(7, 4, 3, 4, 7)
lower_bd = c(0,0,0,0,0)
upper_bd = c(50,50,50,50,50)
m1 = c(-5, -3, -2, -1, 0)
m2 = c(0,1,2,3,5)

# manually making the Y-data
set.seed(1) # setting seed for the default example.
mu = c(-2, -1, 0, 1, 2) 
sigma = diag(c(2, 1, 0.5, 1, 2))
R = 1/2 * diag(5) + 1/2 * c(1, 1, 1, 1, 1) %*%  t(c(1, 1, 1, 1, 1))
sigma_mat = sigma %*% R %*% sigma
n = 50 # num samples
Y = mvrnorm(n = n, mu = mu, Sigma = sigma_mat)

Y_data = as.data.frame(Y)
colnames(Y_data) = c("Y1", "Y2", "Y3", "Y4", "Y5")

Y_data = as.matrix(Y_data)

Y_metrics = function(Y, p){
  #' Given the observed sample (Y) and the number of dimensions (p), 
  #' computes Ybar (the row means of the observed sample) and S.
  if(is.numeric(Y) == TRUE){
    n = nrow(Y)
    if(n < (2*p)){
      return("Error: the value of n (size of Y) is too small.")
    }
    Yprime = t(Y)
    Ybar = rowMeans(Yprime) # rowMeans(t(Y))
    In = matrix(t(rep(1, n))) # identity column
    Ybar_t = matrix(Ybar, nrow=1, ncol = p) # transpose
    
    S = t(Y - In%*%Ybar) %*% (Y - In%*%Ybar) 
  } else {
    return("Error: no proper data given.")
  }
  newlist = list("Ybar" = Ybar, "S" = S)
  return(newlist)
}

Y_suff_stat = Y_metrics(Y = Y_data, p = p)

S = Y_suff_stat$S
Ybar = Y_suff_stat$Ybar

##################################################
# PART 1: ELICITATION FOR THE PRIOR              #
##################################################

# Functions ######################################
elicit_prior_sigma_function = function(p, gamma, s1, s2, upper_bd, lower_bd){
  #' 1/sigma^2 ~ gamma(alpha01, alpha02). Here, we elicit the prior for sigma.
  #' We must specify s1, s2 such that s1 <= sigma * z_{(1+gamma)/2} <= s2
  #' Then the values alpha01, alpha02 must be solved for:
  #' Gamma(alpha01, 1, alpha02 * z^{2}_{(1+p)/2}/s^{2}_{1}) = (1+gamma)/2
  #' Gamma(alpha01, 1, alpha02 * z^{2}_{(1+p)/2}/s^{2}_{2}) = (1-gamma)/2
  #' Using the bisection method.
  #' @param p represents the number of dimensions.
  #' @param gamma represents the virtual uncertainty.
  vectors_of_interest = list(s1, s2, upper_bd, lower_bd)
  for(i in vectors_of_interest){
    if(length(i) != p){return("Error: there is a vector that doesn't have length p.")}
  }
  
  gam = (1+gamma)/2
  z0 = qnorm(gam,0,1)
  upbdinvsigma2 = (z0/s1)**2 
  lwbdinvsigma2 = (z0/s2)**2 
  
  alpha01 = numeric(p) 
  alpha02 = numeric(p)
  
  for(j in 1:p){
    # iterate until prob content of s1<= sigma*z0 <= s2 is within eps of p 
    eps = .0001
    maxits = 100
    alphalow = lower_bd[j]
    alphaup = upper_bd[j]
    for (i in 1:maxits){
      alpha = (alphalow + alphaup)/2
      beta = qgamma(gam, alpha, 1)/upbdinvsigma2[j]
      test = pgamma(beta*lwbdinvsigma2[j], alpha, 1)
      if (abs(test-(1-gam)) <= eps) { break }
      if (test < 1 - gam) { alphaup = alpha} 
      else if (test > 1 - gam) { alphalow = alpha }
    }
    alpha01[j] = alpha
    alpha02[j] = beta
  }
  
  newlist = list("lwbdinvsigma2" = lwbdinvsigma2, "upbdinvsigma2" = upbdinvsigma2, 
                 "alpha01" = alpha01, "alpha02" = alpha02, 
                 "z0" = z0, "s1" = s1, "s2" = s2)
  return(newlist)
}

elicit_prior_mu_function = function(p, gamma, m1, m2, s1, s2, alpha01, alpha02){
  #' We elicit the prior for mu, which is given by:
  #' mu ~ mu0 + sqrt(alpha02/alpha01) (lambda0) (t_{2*alpha01}) 
  #' @param p represents the number of dimensions.
  #' @param gamma represents the virtual uncertainty.
  vectors_of_interest = list(m1, m2, s1, s2, alpha01, alpha02)
  for(i in vectors_of_interest){
    if(length(i) != p){ return("Error: there is a vector that doesn't have length p.") }
  }
  mu0 = (m1 + m2)/2 
  lambda0 = (m2 - m1)/(2 * sqrt(alpha02/alpha01) * qt((1 + gamma)/2, df = 2 * alpha01))
  
  newlist = list("mu0" = mu0, "lambda0" = lambda0, "m1" = m1, "m2" = m2)
  return(newlist)
}

# Values #########################################
prior_sigma_vals = elicit_prior_sigma_function(p, gamma, s1, s2, upper_bd, lower_bd)

prior_sigma_vals

alpha01 = prior_sigma_vals$alpha01
alpha02 = prior_sigma_vals$alpha02

z0 = prior_sigma_vals$z0 # this is the (1+gamma)/2 quantile of the standard normal dist.

lwbdinvsigma2 = prior_sigma_vals$lwbdinvsigma2 # This is (z0/s1)^2
upbdinvsigma2 = prior_sigma_vals$upbdinvsigma2 # This is (z0/s2)^2

prior_mu_vals = elicit_prior_mu_function(p, gamma, m1, m2, s1, s2, alpha01, alpha02)

lambda0 = prior_mu_vals$lambda0
mu0 = prior_mu_vals$mu0

##################################################
# PART 2: SAMPLING FROM THE PRIOR                #
##################################################

# Functions ######################################

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

sample_prior = function(N, p, alpha01, alpha02, mu0, lambda0){
  #' This generates a sample of N from the prior on (mu, Sigma).
  #' 1/sigmaii ~ gamma(alpha01, alpha02)
  #' R is a correlation matrix, R ~ uniform(pxp correlation matrices)
  #' Sigma = diag(sigmaii^1/2) x R x diag(sigmaii^1/2)
  #' mu|Sigma = multivariate_norm(mu0, diag(lambda0) x Sigma x diag(lambda0))
  #' @param N represents the Monte Carlo sample size.
  #' @param p represents the number of dimensions.
  
  mu_mat = matrix(NA, nrow = N, ncol = p)
  sigma_ii_mat = matrix(NA, nrow = N, ncol = p)
  sigma_mat = vector("list", length = N)
  covariance_mat = vector("list", length = N)
  correlation_mat = vector("list", length = N)
  
  for(i in 1:N){
    sigma_ii = 1/rgamma(p, alpha01, alpha02)
    D = diag(sqrt(sigma_ii)) 
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

# Values #########################################

set.seed(1)

sample_prior_vals = sample_prior(N, p, alpha01, alpha02, mu0, lambda0)

##################################################
# PART 3: INTEGRATING W.R.T THE POSTERIOR        #
##################################################

# Functions ######################################

psi = function(mu, xi){
  #' User should modify this. for now, we have a degenerate function
  return(mu)
}

find_inverse_alt = function(matrix){
  #' Helper function that computes the inverse of a matrix using an 
  #' alternative method that preserves positive-definiteness.
  #' @param matrix represents the matrix that is being inputted. 
  x = eigen(matrix, symmetric = TRUE, only.values=FALSE)
  Q = x$vectors
  V_inv = diag(1/x$values)
  B = Q%*%sqrt(V_inv)
  inverse_matrix = B%*%t(B)
  return(inverse_matrix)
}

# NOTE: not updated on the site yet; was modified to be sent to Mike to confirm first.
sample_post_computations = function(N, Ybar, S, p, mu0, lambda0, alpha01, alpha02){
  #' This generates a sample of N from the posterior on (mu, xi) from theorem 4 of the paper.
  #' It also computes the weights!
  #' @param N represents the Monte Carlo sample size.
  #' @param Y represents the row means of the observed sample.
  #' @param S represents the covariance matrix of the observed sample.
  #' @param p represents the number of dimensions.
  lambda02 = max(lambda0)^2
  Lambda0 = diag(lambda0)
  inv_L0 = diag(1/lambda0)
  
  Sigma_Y = find_inverse_alt((S + n/(1 + n * lambda02) * (Ybar - mu0) %*% t(Ybar - mu0)))
  xi = rWishart(N, df = (n - p - 1), Sigma = Sigma_Y) # See Eq 13
  mu_Y = ((n + 1/lambda02)^-1) * (mu0/lambda02 + n * Ybar)
  mu_xi = matrix(NA, nrow = N, ncol = p)
  
  k_vector = numeric(N)
  for(k in 1:N){
    mu_Sigma = ((n + 1/lambda02)^-1) * find_inverse_alt(xi[,,k])
    mu_xi[k,] = mvrnorm(n = 1, mu = mu_Y, Sigma = mu_Sigma) # See Eq 13 (mu conditional on xi)
    # computing the k function
    sigma_ii = diag(find_inverse_alt(xi[,,k])) 
    logk = -(1/2) * t(mu_xi[k,] - mu0) %*% (inv_L0 %*% xi[,,k] %*% inv_L0 - (1/lambda02)*xi[,,k]) %*% (mu_xi[k,] - mu0)
    logk2 = sum(-(alpha01 + (p+1)/2) * log(sigma_ii) - (alpha02/sigma_ii))
    k_vector[k] = exp(logk + logk2)
  }
  weights_vector = k_vector / sum(k_vector)
  cum_weights = cumsum(weights_vector)
  
  return(list("xi" = xi, "mu_xi" = mu_xi, "k_vector" = k_vector, 
              "weights_vector" = weights_vector, "cum_weights" = cum_weights))
}

# Kind of unsure what better description to give here?
elicit_prior_effective_range = function(p, m = 200, alpha01, alpha02, mu0, lambda0, 
                                        x_low, quantile_val = c(0.005, 0.995)){
  #' Computes the effective range from the true prior.
  #' @param p represents the number of dimensions.
  #' @param m represents the number of desired sub-intervals for the effective range.
  #' @param quantile_val represents a vector of size two where the first value denotes the 
  #' smaller quantile, and the second denotes the larger quantile. 
  #' @param x_low denotes the initation of where the search begins to find the effective range.
  #' The other parameters match the descriptions in section 2.1.
  #' @details 
  #' This function assumes that m is consistent throughout each mu. 
  desired_range = diff(quantile_val)
  
  x_range_list = vector("list", length = p)
  y_range_list = vector("list", length = p)
  delta_vector = numeric(p)
  grid_list = vector("list", length = p)
  
  for(k in 1:p){
    if (x_low < 0) {x = seq(x_low, -x_low, by = 0.02)}
    else {x_low = seq(x_low, x_low*2, by = 0.02)}
    y = dt(x,2*alpha01[k])
    scale = sqrt(alpha02[k]/alpha01[k])*lambda0[k]
    xnew = mu0[k] + scale*x
    ynew = y/scale
    # now computing the actual effective range
    x_center = which.min(abs(xnew-(mu0[k])))
    # computing the area underneath
    i = 1
    found_range = FALSE
    while(found_range == FALSE){
      x_low_range = x_center - i
      x_high_range = x_center + i
      x_area = xnew[x_low_range:x_high_range]
      y_area = ynew[x_low_range:x_high_range]
      area = trapz(x_area, y_area)
      if(area >= desired_range){
        found_range = TRUE
        x_range = x_area
        y_range = y_area
      } else {
        i = i + 1
      }
    }
    
    # creating new grid points based off of the effective range
    delta = (x_range[length(x_range)] - x_range[1])/m # length of the sub intervals
    x_grid = seq(x_range[1], x_range[length(x_range)], by = delta) # constructing the new grid
    
    x_range_list[[k]] = x_range
    y_range_list[[k]] = y_range
    delta_vector[k] = delta
    grid_list[[k]] = x_grid
  }
  newlist = list("x_range" = x_range_list, "y_range" = y_range_list,
                 "delta" = delta_vector, "grid" = grid_list)
  return(newlist)
}

posterior_content = function(N, p, effective_range, mu, xi, weights){
  #' Computes the posterior content from the sample of the posterior.
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
  
  newlist = list("post_content" = post_content_matrix,
                  "post_density" = post_density_matrix)
  return(newlist)
}

true_prior_comparison = function(p, alpha01, alpha02, mu0, lambda0, grid){
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

relative_belief_ratio = function(p, prior_content, post_content){
  #' Computes the relative belief ratio.
  #' @param p represents the number of dimensions.
  #' @param prior_content denotes the vector containing the prior.
  #' @param post_content denotes the vector containing the posterior.
  
  rbr_vector = post_content / prior_content
  rbr_vector_mod = ifelse(is.na(rbr_vector), 0, rbr_vector)
  
  newlist = list("RBR" = rbr_vector, "RBR_modified" = rbr_vector_mod)
  return(newlist)
}

# Values #########################################

post_vals = sample_post_computations(N = N, Ybar = Ybar, S = S, p = p, 
                                     mu0 = mu0, lambda0 = lambda0,
                                     alpha01 = alpha01, alpha02 = alpha02)

post_xi = post_vals$xi
post_mu = post_vals$mu

TRU_eff_ran = elicit_prior_effective_range(p, m = m, alpha01, alpha02, mu0, lambda0, 
                                           x_low = -10, quantile_val = c(0.005, 0.995))

post_content_vals = posterior_content(N = n, p = p, 
                                      effective_range = TRU_eff_ran$grid, #eff_ran$grid,
                                      mu = post_mu, 
                                      xi = post_xi, 
                                      weights = post_vals$weights_vector)

test_tru_prior = true_prior_comparison(p, alpha01, alpha02, mu0, lambda0, 
                                       grid = TRU_eff_ran$grid)

rbr_vals = relative_belief_ratio(p, 
                                 prior_content = test_tru_prior$prior_matrix, 
                                 post_content = post_content_vals$post_content)

##################################################
# PART 4: PLOTTING                               #
##################################################






column_number = 1 # pick a value from 1 to p

eff_range_min = TRU_eff_ran$grid[[column_number]][1] #eff_ran$x_range[,column_number][1]
eff_range_max = TRU_eff_ran$grid[[column_number]][length(TRU_eff_ran$grid[[column_number]])]#eff_ran$x_range[,column_number][2]

average_vector_values = function(vector, num_average_pts = 3) {
  #' Generates a new vector by averaging the values of a given vector based on the proximity 
  #' of each element to its neighbors.
  #' @param vector The input vector to be smoothed.
  #' @param num_average_pts The number of neighboring points to consider when calculating 
  #' the average for each element. Only the odd case is implemented.
  if (num_average_pts %% 2 == 0) {
    return("Error: num_average_pts must be an odd number.")
  }
  
  if (num_average_pts == 1) {return(vector)} 
  
  num_neighbours = floor(num_average_pts / 2)
  new_vector = numeric(length(vector))
  
  for (i in 1:length(vector)) {
    lower_index = max(1, i - num_neighbours)
    upper_index = min(length(vector), i + num_neighbours)
    new_vector[i] = mean(vector[lower_index:upper_index])
  }
  
  return(new_vector)
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
       xlim = c(min_xlim, max_xlim), ylim = c(0, max_ylim),
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



par(mfrow = c(1, 2))

comparison_content_density_plot(prior_density = test_tru_prior$prior_matrix, 
                                post_density = post_content_vals$post_density, 
                                col_num = column_number, 
                                prior_grid = test_tru_prior$midpoint_grid_matrix,
                                post_grid = test_tru_prior$midpoint_grid_matrix,
                                min_xlim = eff_range_min, 
                                max_xlim = eff_range_max,
                                smooth_num = c(1,3), colour_choice = c("red", "blue"),
                                lty_type = c(2, 2), transparency = 0)

content_density_plot(density = rbr_vals$RBR_modified, 
                     col_num = column_number, 
                     grid = test_tru_prior$midpoint_grid_matrix, 
                     type = "RBR",
                     min_xlim = eff_range_min, 
                     max_xlim = eff_range_max,
                     smooth_num = 3, colour_choice = "green",
                     lty_type = 2, transparency = 0)

