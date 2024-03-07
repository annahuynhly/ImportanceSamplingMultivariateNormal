# Code for Mike Evans (Prior Elicitation Only)

# install.packages(latex2exp) # if you haven't installed it yet
library(latex2exp) # for latex within graphs
library(MASS)
library(pracma)
library(dplyr)

##################################################
# Inputs                                         #
##################################################

# Mandatory manual inputs

p = 5
gamma = 0.99
N = 1000 # monte carlo sample size
m = 25 # number of sub-intervals (oddly consistent all around)

# Manual input data version ############################

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

############################################
# FUNCTIONS FOR THE PRIOR                  #
############################################

elicit_prior_sigma_function = function(p, gamma, s1, s2, upper_bd, lower_bd){
  #' This represents section 2.1 of the paper.
  #' @param p represents the number of dimensions.
  #' @param gamma represents the virtual uncertainty.
  #' The other parameters match the descriptions in section 2.1.
  vectors_of_interest = list(s1, s2, upper_bd, lower_bd)
  for(i in vectors_of_interest){
    if(length(i) != p){return("Error: there is a vector that doesn't have length p.")}
  }
  
  gam = (1+gamma)/2
  z0 = qnorm(gam,0,1)
  c1 = (z0/s1)**2
  c2 = (z0/s2)**2
  
  alpha01 = numeric()
  alpha02 = numeric()
  
  for(j in 1:p){
    # iterate until prob content of s1<= sigma*z0 <= s2 is within eps of p 
    eps = .0001
    maxits = 100
    alphalow = lower_bd[j]
    alphaup = upper_bd[j]
    for (i in 1:maxits){
      alpha = (alphalow + alphaup)/2
      beta = qgamma(gam, alpha, 1)/c1[j]
      test = pgamma(beta*c2[j], alpha, 1)
      if (abs(test-(1-gam)) <= eps) { break }
      if (test < 1 - gam) { alphaup = alpha} 
      else if (test > 1 - gam) { alphalow = alpha }
    }
    alpha01[j] = alpha
    alpha02[j] = beta
  }
  
  newlist = list("c1" = c1, "c2" = c2, "alpha01" = alpha01, "alpha02" = alpha02, 
                 "z0" = z0, "s1" = s1, "s2" = s2)
  return(newlist)
}

elicit_prior_mu_function = function(p, gamma, m1, m2, s1, s2, alpha01, alpha02){
  #' This represents section 2.2 of the paper.
  #' @param p represents the number of dimensions.
  #' @param gamma represents the virtual uncertainty.
  #' The other parameters match the descriptions in section 2.1.
  vectors_of_interest = list(m1, m2, s1, s2, alpha01, alpha02)
  for(i in vectors_of_interest){
    if(length(i) != p){ return("Error: there is a vector that doesn't have length p.") }
  }
  mu0 = (m1 + m2)/2 
  lambda0 = (m2 - m1)/(2 * sqrt(alpha02/alpha01) * qt((1 + gamma)/2, df = 2 * alpha01))
  
  newlist = list("mu0" = mu0, "lambda0" = lambda0, "m1" = m1, "m2" = m2)
  return(newlist)
}

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

average_vector_values = function(vector, num_average_pts = 3){
  #' Generates a new vector by averaging the values of a given vector based on the proximity 
  #' of each element to its neighbors.
  #' @param vector The input vector to be smoothed.
  #' @param num_average_pts The number of neighboring points to consider when calculating 
  #' the average for each element. Only the odd case is implemented.
  if(num_average_pts %% 2 == 0){
    return("Error: num_average_pts must be an odd number.")
  }
  
  if(num_average_pts == 1){
    return(vector)
  } 
  new_vector = rep(0, length(vector))
  
  pts = 0
  num_neighbours = floor(num_average_pts/2)
  for(i in 1:length(vector)){
    if(i <= num_neighbours | (length(vector) - i) < num_neighbours){ # Edge points case
      if(i == 1 | i == length(vector)){
        new_vector[i] = vector[i]
      } else {
        if (i <= num_neighbours){
          pts = i - 1
        } else if ((length(vector) - i) < num_neighbours){
          pts = length(vector) - i
        }
        new_vector[i] = sum(vector[(i-pts):(i+pts)])/(2*pts + 1)
      }
    } else {
      lower_int = i - num_neighbours
      upper_int = i + num_neighbours
      new_vector[i] = sum(vector[lower_int:upper_int])/(2*num_neighbours + 1)
    }
  }
  
  return(new_vector)
}

psi = function(mu, xi){
  # user should modify this. for now, we have a degenerate function
  return(mu)
}

sample_prior = function(N, p, alpha01, alpha02, mu0, lambda0){
  #' This represents section 3.1 of the paper.
  #' @param N represents the Monte Carlo sample size.
  #' @param p represents the number of dimensions.
  #' The other parameters match the descriptions in section 2.1.
  mu_mat = c()
  sigma_mat = list()
  covariance_mat = list() 
  correlation_mat = list()
  sigma_ii_mat = c()
  
  for(i in 1:N){
    sigma_ii = 1/rgamma(p, alpha01, alpha02)
    D = diag(sigma_ii^2)
    R = onion(p) # the correlation matrix
    Lambda = diag(lambda0)
    SIGMA = D %*% R %*% D
    
    var_mat = Lambda %*% SIGMA %*% Lambda
    
    MU = mvrnorm(n = 1, mu = mu0, Sigma = var_mat)
    
    mu_mat = rbind(mu_mat, MU)
    sigma_ii_mat = rbind(sigma_ii_mat, sigma_ii)
    sigma_mat[[i]] = SIGMA
    covariance_mat[[i]] = var_mat
    correlation_mat[[i]] = R
  }
  return(list("mu_matrix" = mu_mat, "sigma_ii" = sigma_ii_mat,
              "sigma_matrix" = sigma_mat,
              "covariance_matrix" = covariance_mat, 
              "correlation_matrix" = correlation_mat))
}

true_prior_density = function(p, alpha01, alpha02, lambda0, mu0){
  #' Plots the true prior.
  #' @param p represents the number of dimensions.
  x = -10+20*c(0:1000)/1000
  x_vector = c()
  y_vector = c()
  for(i in 1:p){
    y = dt(x,2*alpha01[i])
    scale = sqrt(alpha02[i]/alpha01[i])*lambda0[i]
    xnew = mu0[i] + scale*x
    ynew = y/scale
    
    x_vector = cbind(x_vector, xnew)
    y_vector = cbind(y_vector, ynew)
  }
  newlist = list("x_vector" = x_vector, "y_vector" = y_vector)
  return(newlist)
}

# NEW function: may actually work for desired effects.
find_effective_range = function(p, m, x_vector_matrix, y_vector_matrix,
                                quantile_val = c(0.005,0.995)){
  #' Calculates the effective range.
  #' @param p represents the number of dimensions.
  #' @param m represents the number of desired sub-intervals for the effective range.
  #' @param quantile_val represents the smaller quantile of interest for the effective range.
  #' @details 
  #' This function assumes that m is consistent throughout each mu. 
  desired_range = quantile_val[2] - quantile_val[1]
  
  x_range_matrix = c()
  y_range_matrix = c()
  delta_vector = c()
  grid_matrix = c()
  
  for(k in 1:p){
    x_vector = x_vector_matrix[,k]
    y_vector = y_vector_matrix[,k]
    found_range = FALSE
    if(length(x_vector) == length(y_vector)){
      n = length(x_vector)
    } else {
      return("Error: x_vector and y_vector contain different lengths.")
    }
    i = 0
    while(found_range == FALSE){
      x_new = x_vector[(i+1):(n-i)]
      y_new = y_vector[(i+1):(n-i)]
      area = trapz(x_new, y_new)
      if(area <= desired_range){
        found_range = TRUE
      } else {
        i = i + 1
      }
    }
    x_range = c(x_new[1], x_new[length(x_new)])
    y_range = c(x_new[1], x_new[length(x_new)])
    # creating new grid points based off of the effective range
    delta = (x_range[2] - x_range[1])/m # length of the sub intervals
    x_grid = seq(x_range[1], x_range[2], by = delta) # constructing the new grid
    
    x_range_matrix = cbind(x_range_matrix, x_range)
    y_range_matrix = cbind(y_range_matrix, y_range)
    delta_vector = c(delta_vector, delta)
    grid_matrix = cbind(grid_matrix, x_grid)
  }
  
  newlist = list("x_range" = x_range_matrix, "y_range" = y_range_matrix,
                 "delta" = delta_vector, "grid" = grid_matrix)
  return(newlist)
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

##################################################
# ELICITATION FOR THE PRIOR (VALUES)             #
##################################################

prior_sigma_vals = elicit_prior_sigma_function(p, gamma, s1, s2, upper_bd, lower_bd)

prior_sigma_vals

alpha01 = prior_sigma_vals$alpha01
alpha02 = prior_sigma_vals$alpha02
c1 = prior_sigma_vals$c1
c2 = prior_sigma_vals$c2
z0 = prior_sigma_vals$z0

prior_mu_vals = elicit_prior_mu_function(p, gamma, m1, m2, s1, s2, alpha01, alpha02)

lambda0 = prior_mu_vals$lambda0
mu0 = prior_mu_vals$mu0

prior_mu_vals

############################################
# SAMPLING THE PRIOR                       #
############################################

set.seed(1) # note: seed does seem to influence the results quite a bit!

sample_prior_vals = sample_prior(N, p, alpha01, alpha02, mu0, lambda0)

sigma_ii = sample_prior_vals$sigma_ii

#sample_prior_data_cleaning(N = N, p =p, 
#                           mu_matrix= sample_prior_vals$mu_matrix, 
#                           sigma_ii_matrix = sample_prior_vals$sigma_ii,
#                           correlation_matrix = sample_prior_vals$correlation_matrix)

test_tru_prior_vals = true_prior_density(p, alpha01, alpha02, lambda0, mu0)

eff_ran = find_effective_range(p = p, m = m, 
                               x_vector_matrix = test_tru_prior_vals$x_vector, 
                               y_vector_matrix = test_tru_prior_vals$y_vector)

test_tru_prior_vals$x_vector

eff_ran$x_range

############################################
# FUNCTIONS FOR THE POSTERIOR              #
############################################

find_inverse_alt = function(matrix){
  #' Computes the inverse of a matrix using an alternative method that 
  #' preserves positive-definiteness.
  #' @param matrix represents the matrix that is being inputted. 
  x = eigen(matrix, symmetric = TRUE, only.values=FALSE)
  Q = x$vectors
  V_inv = diag(1/x$values)
  B = Q%*%sqrt(V_inv)
  inverse_matrix = B%*%t(B)
  return(inverse_matrix)
}

seq_alt = function(values, delta){
  #' Generate an alternative sequence of values based on the input vector and the distance between
  #' two points. If the maximum value is not already present in the sequence, it is added to the end.
  #' @param values represents the numerical vector for which an alternative sequence is generated.
  #' @param delta represents the distance between two points.
  min = floor(min(values))
  max = ceiling(max(values))
  grid = seq(min, max, by = delta)
  if(!(max %in% grid) == TRUE){
    grid = c(grid, max)
  }
  return(grid)
}

sample_post_computations = function(N, Y, p, mu0, lambda0){
  #' This represents section 3.2 of the paper.
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
    
    S = (1/(n-1)) * t(Y - In%*%rowMeans(t(Y))) %*% (Y - In%*%rowMeans(t(Y))) 
  } else {
    return("Error: no data given.")
  }
  
  lambda0 = rep(max(lambda0), p)
  
  # instead of using solve, may need to move to an alt version (see helper functions)
  Sigma_Y = find_inverse_alt((S + n/(1 + n * lambda0^2) * (rowMeans(t(Y)) - mu0) %*% t(rowMeans(t(Y)) - mu0)))
  mu_Y = ((n + 1/lambda0^2)^-1) * (mu0/lambda0^2 + n * rowMeans(t(Y)))
  mu_Sigma = ((n + 1/lambda0^2)^-1) * find_inverse_alt(Sigma_Y)
  
  xi = rWishart(n = N, df = (n - p - 1), Sigma = Sigma_Y)
  mu_xi = mvrnorm(n = N, mu = mu_Y, Sigma = mu_Sigma)
  
  return(list("xi" = xi, "mu_xi" = mu_xi))
}

k = function(p, mu, xi, mu0, lambda0, sigma_ii, alpha01, alpha02){
  #' This represents the k function explained in theorem 2.
  #' @param p represents the number of dimensions.
  #' The other parameters match the descriptions in the paper.
  Lambda0 = diag(lambda0)
  inv_Lambda0 = find_inverse_alt(Lambda0)
  x1 = exp(-(1/2) * t(mu - mu0) %*% inv_Lambda0 %*% xi %*% inv_Lambda0 * (mu - mu0))
  x3 = exp(-alpha02/sigma_ii)
  x2 = 1
  for(i in 1:p){
    x2 = x2 * (1/sigma_ii[i])^(alpha01[i] + (p+1)/2)
  }
  return(x1 * x2 * x3)
}

weights = function(N, p, mu, xi, mu0, lambda0, sigma_ii, alpha01, alpha02){
  #' Computes the weights given for the posterior content.
  #' @param N represents the Monte Carlo sample size.
  #' @param p represents the number of dimensions.
  #' The other parameters match the descriptions in the paper.
  k_vector = c()
  for(i in 1:N){
    k_val = k(p, mu[i,], xi[,,i], mu0, lambda0, sigma_ii[i,], alpha01, alpha02)
    k_vector = rbind(k_vector, k_val)
  }
  weights_vector = c()
  for(q in 1:p){
    k_val2 = k_vector[,q] / sum(k_vector[,q])
    weights_vector = cbind(weights_vector, k_val2)
  }
  #weights_vector = k_vector / colSums(k_vector)
  return(weights_vector)
}

posterior_content = function(N, p, effective_range, mu, xi, weights){
  #' Computes the posterior content.
  #' @param N represents the Monte Carlo sample size.
  #' @param p represents the number of dimensions.
  #' @param effective_range denotes the vector of grid points where the density
  #'        is highly concentrated (this is computed from the sampling of the  prior).'
  #' @param weights denote the weights given to each value of psi.
  #'        The other parameters match the descriptions in the paper.
  psi_val = psi(mu, xi) # note: the user will need to manually change this
  post_content_matrix = c()
  post_density_matrix = c()
  
  for(k in 1:p){
    grid = effective_range[,k]
    delta = diff(effective_range[,k])[1]
    post_content_vec = c() 
    for(i in 1:(length(grid) - 1)){
      post_content = 0
      for(j in 1:N){
        if(between(psi_val[,k][j], grid[i], grid[i+1])){
          post_content = post_content + weights[,k][j]
        }
      }
      post_content_vec = c(post_content_vec, post_content)
    }
    post_density = post_content_vec / delta
    post_content_matrix = cbind(post_content_matrix, post_content_vec)
    post_density_matrix = cbind(post_density_matrix, post_density)
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
  #' @param grid represents the grid of values where the posterior is based off of.
  #' The other parameters match the descriptions from the paper.
  prior_matrix = c()
  midpt_grid_matrix = c()
  for(i in 1:p){
    midpt_grid = grid[,i][-length(grid[,i])] + diff(grid[,i])/2
    
    scale = sqrt(alpha02[i]/alpha01[i])*lambda0[i]
    reg_x = (midpt_grid - mu0[i])/scale
    y = dt(reg_x,2*alpha01[i])/scale
    
    prior_matrix = cbind(prior_matrix, y)
    midpt_grid_matrix = cbind(midpt_grid_matrix, midpt_grid)
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
  rbr_vector = c()
  for(k in 1:p){
    rbr_vals = post_content[,k] / prior_content[,k] 
    rbr_vector = cbind(rbr_vector, rbr_vals)
  }
  
  rbr_vector_mod = rbr_vector
  rbr_vector_mod[is.na(rbr_vector_mod)] = 0 # force NA to 0
  
  newlist = list("RBR" = rbr_vector, "RBR_modified" = rbr_vector_mod)
  return(newlist)
}

# changed below.
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

##################################################
# ELICITATION FOR THE POSTERIOR (VALUES)         #
##################################################

post_vals = sample_post_computations(N, Y = Y_data, p, mu0, lambda0)

post_xi = post_vals$xi
post_mu = post_vals$mu

test_weights = weights(N = N, p = p, 
                       mu = post_mu, xi = post_xi, 
                       mu0 = mu0, lambda0 = lambda0, 
                       sigma_ii = sigma_ii, alpha01 = alpha01, alpha02 = alpha02)

post_content_vals = posterior_content(N, p, 
                                      effective_range = eff_ran$grid,
                                      post_mu, 
                                      post_xi, 
                                      test_weights)

test_tru_prior = true_prior_comparison(p, alpha01, alpha02, mu0, lambda0, 
                                       grid = eff_ran$grid)

par(mfrow = c(1, 2))

column_number = 1

eff_range_min = eff_ran$x_range[,column_number][1]
eff_range_max = eff_ran$x_range[,column_number][2]

eff_range_min
eff_range_max

comparison_content_density_plot(prior_density = test_tru_prior$prior_matrix, 
                                post_density = post_content_vals$post_density, 
                                col_num = column_number, 
                                prior_grid = test_tru_prior$midpoint_grid_matrix,
                                post_grid = test_tru_prior$midpoint_grid_matrix,
                                min_xlim = eff_range_min, 
                                max_xlim = eff_range_max,
                                smooth_num = c(1,1), colour_choice = c("red", "blue"),
                                lty_type = c(2, 2), transparency = 0)

rbr_vals = relative_belief_ratio(p, 
                                 prior_content = test_tru_prior$prior_matrix, 
                                 post_content = post_content_vals$post_content)


# the relative belief ratio
content_density_plot(density = rbr_vals$RBR_modified, 
                     col_num = column_number, 
                     grid = test_tru_prior$midpoint_grid_matrix, 
                     type = "RBR",
                     min_xlim = eff_range_min, 
                     max_xlim = eff_range_max,
                     smooth_num = 1, colour_choice = "green",
                     lty_type = 2, transparency = 0)

