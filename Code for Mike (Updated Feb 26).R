# Code for Mike Evans (Prior Elicitation Only)

# install.packages(latex2exp) # if you haven't installed it yet
library(latex2exp) # for latex within graphs
library(MASS)

##################################################
# Inputs                                         #
##################################################

# Mandatory manual inputs

p = 3
gamma = 0.99
N = 1000 # monte carlo sample size
m = 400 # number of sub-intervals (oddly consistent all around)

# Manual input data version ############################

s1 = c(2, 2, 2)
s2 = c(10,10,10)
lower_bd = c(0,0,0)
upper_bd = c(50,50,50)
m1 = c(-5,-5,-5)
m2 = c(5,5,5)

# manually making the Y-data
mu = rep(2, p)
sigma = diag(p) 
n = 100
Y = mvrnorm(n = n, mu = mu, Sigma = sigma)
Y_data = as.data.frame(Y)
colnames(Y_data) = c("Y1", "Y2", "Y3")

Y_data = as.matrix(Y_data)

# reading from a text file version #####################

# WARNING: you'd need to change your working directory
setwd("C:/Users/AnnaH/OneDrive/Desktop/Stats RA/New Project")

data = read.csv("prior_elicitation_inputs.txt", sep = ",")

View(data) # to view the file format

s1 = data$s1
s2 = data$s2
lower_bd = data$lower_bd
upper_bd = data$upper_bd
m1 = data$m1
m2 = data$m2

Y_data = read.csv("Y_example.csv")
Y_data = as.matrix(Y_data)

############################################
# FUNCTIONS FOR THE PRIOR                  #
############################################

elicit_prior_sigma_function = function(p, gamma, s1, s2, upper_bd, lower_bd){
  
  vectors_of_interest = list(s1, s2, upper_bd, lower_bd)
  for(i in vectors_of_interest){
    if(length(i) != p){
      return("Error: there is a vector that doesn't have length p.")
    }
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
      if (abs(test-(1-gam)) <= eps) {
        break
      }
      if(test < 1 - gam){
        alphaup = alpha
      } else if (test > 1 - gam){ # later: see if the else if is causing an error to make the code efficient
        alphalow = alpha
      }
    }
    alpha01[j] = alpha
    alpha02[j] = beta
  }
  
  newlist = list("c1" = c1, "c2" = c2, "alpha01" = alpha01, "alpha02" = alpha02, "z0" = z0,
                 "s1" = s1, "s2" = s2)
  return(newlist)
}

elicit_prior_mu_function = function(p, gamma, m1, m2, s1, s2, alpha01, alpha02){
  
  vectors_of_interest = list(m1, m2, s1, s2, alpha01, alpha02)
  for(i in vectors_of_interest){
    if(length(i) != p){
      return("Error: there is a vector that doesn't have length p.")
    }
  }
  mu0 = (m1 + m2)/2 # multivariate mu_0
  
  lambda0 = (m2 - m1)/(2 * sqrt(alpha02/alpha01) * qt((1 + gamma)/2, df = 2 * alpha01))
  
  newlist = list("mu0" = mu0, "lambda0" = lambda0, "m1" = m1, "m2" = m2)
  return(newlist)
}

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

average_vector_values = function(vector, num_average_pts = 3){
  # num_average_pts: the number of density bins closely added to each other to get
  # a smoother density plot. (Reduce peaks.)
  if(num_average_pts %% 2 == 0){
    # Note: the even case is harder to code. For this instance, since the number of average points
    # will be pre-determined for the user (in terms of the plots), I have decided to not add
    # even functionality for now.
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

prior_content = function(N, p, m, mu, xi){
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
    quant_range = quantile(psi_val[,k], prob = c(0.005, 0.995))
    delta = (quant_range[2] - quant_range[1])/m # length of the sub intervals
    grid = seq(quant_range[1], quant_range[2], by = delta) # making a grid of the sub intervals
    # note: since we're looking at the effective range, the prior content doesn't sum to 1.
    
    effective_range = cbind(effective_range, grid)
    plot_grid = grid[-length(grid)] + diff(grid)/2
    plotting_grid = cbind(plotting_grid, plot_grid)
    
    # computing prior content
    prior_content_vec = c() # will be of length m
    for(i in 1:m){
      prior_content = 0
      for(j in 1:N){
        if(between(psi_val[,k][j], grid[i], grid[i+1])){ # CHANGES HERE
          prior_content = prior_content + 1
        }
      }
      prior_content_vec = c(prior_content_vec, prior_content)
    }
    prior_content_vec = prior_content_vec/N
    prior_density = prior_content_vec / delta
    prior_content_matrix = cbind(prior_content_matrix, prior_content_vec)
    prior_density_matrix = cbind(prior_density_matrix, prior_density)
  }
  newlist = list("plotting_grid" = plotting_grid,
                 "effective_range" = effective_range,
                 "prior_content" = prior_content_matrix,
                 "prior_density" = prior_density_matrix)
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

##################################################
# GRAPHS FOR THE PRIOR                           #
##################################################

# Plot for the prior elictation of sigma #########

graph_num = 1 # the index used for the graph

alpha = alpha01[graph_num]
beta = alpha02[graph_num]
low = c1[graph_num]
up = c2[graph_num]
x = low+(up-low)*c(0:1000)/1000
prob_z = round(pnorm(z0), 4)

# prior density of sigma only
x3 = sqrt(1/x)
dens3 = 2*(x^(3/2))*dgamma(x,alpha,beta)
plot(x3,dens3, main = TeX(paste("Prior Density of $\\sigma_{", graph_num, "}$")),
     xlab = TeX(paste("$\\sigma_{", graph_num, "}$")), ylab = "Prior Density", type = "l")

# prior density of sigma * z0 
x3 = z0*sqrt(1/x)
dens3 = (2/z0)*(x^(3/2))*dgamma(x, alpha, beta)
plot(x3, dens3,
     main = TeX(paste("Prior Density of $\\sigma_{", graph_num, '}\\cdot z_{0}$')),
     xlab = TeX(paste('$\\sigma_{', graph_num, '} \\cdot z_{0}$')),
     ylab = "Prior Density", type = "l")

# Plot for the prior elictation of mu ############

graph_num = 1 # the index used for the graph

x = -10+20*c(0:1000)/1000
y = dt(x,2*alpha01[graph_num])
scale = sqrt(alpha02[graph_num]/alpha01[graph_num])*lambda0[graph_num]
xnew = mu0[graph_num] + scale*x
ynew = y/scale

plot(xnew, ynew, lwd = 1, type="l", xlab = TeX(paste("Value of $\\mu_{", graph_num, "}$")),
     ylab = "Density", main = TeX(paste("Prior Density of $\\mu_{", graph_num, "}$")))

############################################
# SAMPLING THE PRIOR                       #
############################################

sample_prior_vals = sample_prior(N, p, alpha01, alpha02, mu0, lambda0)

sigma_ii = sample_prior_vals$sigma_ii

prior_content_vals = prior_content(N, p, m, 
                                   mu = sample_prior_vals$mu_matrix, 
                                   xi = find_inverse_alt(test$covariance_matrix[,,1]))

#prior_content_vals$effective_range

content_density_plot(density = prior_content_vals$prior_density, 
                     col_num = 1, 
                     grid = prior_content_vals$plotting_grid, 
                     min_xlim = -10, max_xlim = 10,
                     smooth_num = 1, colour_choice = "blue",
                     lty_type = 2, transparency = 0.4)

############################################
# FUNCTIONS FOR THE POSTERIOR              #
############################################

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

sample_post_computations = function(N, Y, p, mu0, lambda0){
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
    
    #S = (1/(n-1)) * t(Y - In%*%Ybar_t) %*% (Y - In%*%Ybar_t)
    S = (1/(n-1)) * t(Y - In%*%rowMeans(t(Y))) %*% (Y - In%*%rowMeans(t(Y))) 
  } else {
    return("Error: no data given.")
  }
  
  # NOTE: COPY AND PASTE THIS ELSEWHERE
  lambda0 = rep(max(lambda0), p)
  
  # instead of using solve, may need to move to an alt version (see helper functions)
  Sigma_Y = find_inverse_alt((S + n/(1 + n * lambda0^2) * (rowMeans(t(Y)) - mu0) %*% t(rowMeans(t(Y)) - mu0)))
  # seems like in the formula, there's a summation here but unsure what it represents
  mu_Y = ((n + 1/lambda0^2)^-1) * (mu0/lambda0^2 + n * rowMeans(t(Y)))
  mu_Sigma = ((n + 1/lambda0^2)^-1) * find_inverse_alt(Sigma_Y)
  
  xi = rWishart(n = N, df = (n - p - 1), Sigma = Sigma_Y)
  mu_xi = mvrnorm(n = N, mu = mu_Y, Sigma = mu_Sigma)
  
  return(list("xi" = xi, "mu_xi" = mu_xi))
}

k = function(p, mu, xi, mu0, lambda0, sigma_ii){

  Lambda0 = diag(lambda0)
  inv_Lambda0 = find_inverse_alt(Lambda0)
  #x1 = inv_Lambda0 %*% xi %*% inv_Lambda0 
  x1 = exp(-(1/2) * t(mu - mu0) %*% inv_Lambda0 %*% xi %*% inv_Lambda0 * (mu - mu0))
  x3 = exp(-alpha02/sigma_ii)
  x2 = 1
  for(i in 1:p){
    x2 = x2 * (1/sigma_ii[i])^(alpha01[i] + (p+1)/2)
  }
  return(x1 * x2 * x3)
}

weights = function(N, p, mu, xi, mu0, lambda0, sigma_ii){
  k_vector = c()
  for(i in 1:N){
    k_val = k(p, mu[i,], xi[,,i], mu0, lambda0, sigma_ii[i,])
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
  # first part: denote psi(mu, xi)
  # m: number of sub intervals 
  # (note: assumption is that the num of time intervals are the same for each mu1 - may need to change
  # this later if it poses an issue...
  
  psi_val = psi(mu, xi) # note: the user will need to manually change this
  post_content_matrix = c()
  post_density_matrix = c()
  
  for(k in 1:p){
    grid = effective_range[,k]
    delta = diff(effective_range[,k])[1]
    #print(grid)
    # computing post content
    post_content_vec = c() # will be of length m
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

comparison_content_density_plot = function(prior_density, post_density, col_num, grid,
                                           min_xlim = -10, max_xlim = 10,
                                           smooth_num = 1, colour_choice = c("red", "blue"),
                                           lty_type = c(2, 2), transparency = 0.4){
  prior_col_rgb = col2rgb(colour_choice[1])
  post_col_rgb = col2rgb(colour_choice[2])
  
  prior_area_col = rgb(prior_col_rgb[1]/255, prior_col_rgb[2]/255, prior_col_rgb[3]/255, 
                       alpha = transparency)
  post_area_col = rgb(post_col_rgb[1]/255, post_col_rgb[2]/255, post_col_rgb[3]/255, 
                      alpha = transparency)
  
  prior_density_vals = average_vector_values(prior_density[,col_num], smooth_num)
  post_density_vals = average_vector_values(post_density[,col_num], smooth_num)
  
  max_ylim = max(c(max(prior_density_vals), max(post_density_vals)))
  
  plot(grid[,col_num], prior_density_vals,
       xlim = c(min_xlim, max_xlim), ylim = c(0, max_ylim),
       col = colour_choice[1],
       main = TeX(paste("Prior & Posterior Density Histogram of $\\mu_{", col_num, "}$")),
       xlab = TeX(paste("Value of $\\mu_{", col_num, "}$")),
       ylab = "Density",
       type = "l", lty = lty_type[1], lwd = 2)
  
  lines(grid[,col_num], post_density_vals, 
        lty = lty_type[2], lwd = 2, col = colour_choice[2])
  
  polygon(grid[, col_num], prior_density_vals, col = prior_area_col, border = NA)
  polygon(grid[, col_num], post_density_vals, col = post_area_col, border = NA)
  
  legend("topleft", legend=c("Prior", "Posterior"),
         col= colour_choice, lty=lty_type, cex=0.8)
}

relative_belief_ratio = function(p, prior_content, post_content){
  
  rbr_vector = c()
  for(k in 1:p){
    rbr_vals = post_content[,k] / prior_content[,k] 
    rbr_vector = cbind(rbr_vector, rbr_vals)
  }
  
  # below is a vector where the NAs are zero - easier to plot.
  rbr_vector_mod = rbr_vector
  rbr_vector_mod[is.na(rbr_vector_mod)] = 0
  
  newlist = list("RBR" = rbr_vector, "RBR_modified" = rbr_vector_mod)
  return(newlist)
}

##################################################
# ELICITATION FOR THE POSTERIOR (VALUES)         #
##################################################

post_vals = sample_post_computations(N, Y = Y_data, p, mu0, lambda0)

post_xi = post_vals$xi
post_mu = post_vals$mu

#k(p, post_mu[1,], post_xi[,,1], mu0, lambda0, sigma_ii[1,])

test_weights = weights(N, p, post_mu, post_xi, mu0, lambda0, sigma_ii)

post_content_vals = posterior_content(N, p, prior_content_vals$effective_range,
                                      post_mu, post_xi, test_weights)

sum(post_content_vals$post_content[,1])

length(post_content_vals$effective_range[,1])

# Plot of the posterior density
content_density_plot(density = post_content_vals$post_density, 
                     col_num = 1, 
                     grid = prior_content_vals$plotting_grid, 
                     type = "Posterior",
                     min_xlim = -10, max_xlim = 10,
                     smooth_num = 1, colour_choice = "blue",
                     lty_type = 2, transparency = 0.4)

# Comparing the prior and the posterior
comparison_content_density_plot(prior_density = prior_content_vals$prior_density, 
                                post_density = post_content_vals$post_density, 
                                col_num = 1, grid = prior_content_vals$plotting_grid,
                                min_xlim = -10, max_xlim = 10,
                                smooth_num = 1, colour_choice = c("red", "blue"),
                                lty_type = c(2, 2), transparency = 0.3)

rbr_vals = relative_belief_ratio(p = 3, 
                                 prior_content = prior_content_vals$prior_content, 
                                 post_content = post_content_vals$post_content)

# the relative belief ratio
content_density_plot(density = rbr_vals$RBR_modified, 
                     col_num = 1, 
                     grid = prior_content_vals$plotting_grid, 
                     type = "RBR",
                     min_xlim = -10, max_xlim = 10,
                     smooth_num = 1, colour_choice = "blue",
                     lty_type = 2, transparency = 0.4)
