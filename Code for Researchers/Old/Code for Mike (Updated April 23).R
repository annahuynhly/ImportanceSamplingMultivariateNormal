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
Nprior = 50000
Npostimp = 50000
Npostsamp = 50000

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

lwbdinvsigma2 = prior_sigma_vals$lwbdinvsigma2 # This is (z0/s2)^2
upbdinvsigma2 = prior_sigma_vals$upbdinvsigma2 # This is (z0/s1)^2

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

sample_prior = function(Nprior, p, alpha01, alpha02, mu0, lambda0){
  #' This generates a sample of Nprior from the prior on (mu, Sigma).
  #' 1/sigmaii ~ gamma(alpha01, alpha02)
  #' R is a correlation matrix, R ~ uniform(pxp correlation matrices)
  #' Sigma = diag(sigmaii^1/2) x R x diag(sigmaii^1/2)
  #' mu|Sigma = multivariate_norm(mu0, diag(lambda0) x Sigma x diag(lambda0))
  #' @param Nprior represents the Monte Carlo sample size.
  #' @param p represents the number of dimensions.
  
  mu_mat = matrix(NA, nrow = Nprior, ncol = p)
  sigma_ii_mat = matrix(NA, nrow = Nprior, ncol = p)
  sigma_mat = vector("list", length = Nprior)
  covariance_mat = vector("list", length = Nprior)
  correlation_mat = vector("list", length = Nprior)
  
  for(i in 1:Nprior){
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

sample_prior_vals = sample_prior(Nprior, p, alpha01, alpha02, mu0, lambda0)

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
importance_sampler_computations = function(Npostimp, Ybar, S, p, mu0, lambda0, alpha01, alpha02){
  #' This generates a sample of N from the importance sampler on (mu, xi) from theorem 4 of the paper.
  #' It also computes the weights!
  #' @param Npostimp represents the Monte Carlo sample size.
  #' @param Y represents the row means of the observed sample.
  #' @param S represents the covariance matrix of the observed sample.
  #' @param p represents the number of dimensions.
  lambda02 = max(lambda0)^2
  Lambda0 = diag(lambda0)
  inv_L0 = diag(1/lambda0)
  
  Sigma_Y = find_inverse_alt((S + n/(1 + n * lambda02) * (Ybar - mu0) %*% t(Ybar - mu0)))
  xi = rWishart(Npostimp, df = (n - p - 1), Sigma = Sigma_Y) # See Eq 13
  mu_Y = ((n + 1/lambda02)^-1) * (mu0/lambda02 + n * Ybar)
  mu_xi = matrix(NA, nrow = Npostimp, ncol = p)
  
  k_vector = numeric(Npostimp)
  for(i in 1:Npostimp){
    Sigma_val = find_inverse_alt(xi[,,i])
    mu_Sigma = ((n + 1/lambda02)^-1) * Sigma_val
    mu_xi[i,] = mvrnorm(n = 1, mu = mu_Y, Sigma = mu_Sigma) # See Eq 13 (mu conditional on xi)
    # computing the k function
    sigma_ii = diag(Sigma_val) 
    logk = -(1/2) * t(mu_xi[i,] - mu0) %*% (inv_L0 %*% xi[,,i] %*% inv_L0 - (1/lambda02)*xi[,,i]) %*% (mu_xi[i,] - mu0)
    logk2 = sum(-(alpha01 + (p+1)/2) * log(sigma_ii) - (alpha02/sigma_ii))
    k_vector[i] = exp(logk + logk2)
  }
  weights_vector = k_vector / sum(k_vector)
  cum_weights = cumsum(weights_vector)
  
  return(list("xi" = xi, "mu_xi" = mu_xi, "weights_vector" = weights_vector, 
              "cum_weights" = cum_weights))
}

# new; not on the site.
SIR_algorithm = function(Npostsamp, cum_weights, p, mu_xi, xi){
  #' Using the SIR algorithm from Rubin, generates U ~ Uniform(0, 1) and samples
  #' a value of mu and xi. (Need a better description?)
  #' @param Npostsamp the size of the sample that will be generated.
  #' @param cum_weights the vector containing the cumulative weights.
  #' @param mu the mu matrix.
  #' @param xi the list containing xi's.
  U = runif(Npostsamp)
  i = findInterval(U, cum_weights) # shouldn't need a plus one?
  sample_mu_xi = mu_xi[i, ]
  sample_xi = xi[,,i]
  newlist = list("sample_mu_xi" = sample_mu_xi, "sample_xi" = sample_xi)
  return(newlist)
}

# Aside: this is a test to see whether or not findInterval needed a plus one.
# I don't think it does, after doing tests similar to this one:
#U = runif(10)
#cum_weights = seq(0, 1, by = 0.05)
#findInterval(U, cum_weights)

# Values #########################################

imp_vals = importance_sampler_computations(Npostimp = Npostimp, Ybar = Ybar, S = S, p = p, 
                                     mu0 = mu0, lambda0 = lambda0,
                                     alpha01 = alpha01, alpha02 = alpha02)

post_xi = imp_vals$xi
post_mu = imp_vals$mu_xi

post_SIR_sample = SIR_algorithm(Npostsamp = Npostsamp, cum_weights = post_vals$cum_weights, 
                                p = p, mu = post_mu, xi = post_xi)

##################################################
# PART 4: CHECKING THE RANGES FIRST              #
##################################################

column_number = 1 # user must denote

# sample from earlier
prior_psi_values = sample_prior_vals$mu_matrix[,column_number]

psi_lower_bd = min(prior_psi_values)
psi_upper_bd = max(prior_psi_values)
# user can manually denote - just made my own formula below for ease (while testing)
n_breaks = ceiling((psi_upper_bd - psi_lower_bd) * 10) + 1

delta_psi = (psi_upper_bd - psi_lower_bd) / (n_breaks - 1)
breaks = seq(psi_lower_bd, psi_upper_bd, by = delta_psi)

prior_psi = hist(prior_psi_values, breaks, freq = F)

##################################################
# PART 5: PLOTTING                               #
##################################################

column_number = 1 # user must denote

# sample from earlier
post_psi_values = post_SIR_sample$sample_mu_xi[,column_number]
weight_vector = imp_vals$weights_vector

psi_lower_bd = -6 # user must denote
psi_upper_bd = 1 # user must denote
# user can manually denote - just made my own formula below for ease (while testing)
n_breaks = (psi_upper_bd - psi_lower_bd) * 10

delta_psi = (psi_upper_bd - psi_lower_bd)/n_breaks 
breaks = seq(psi_lower_bd, psi_upper_bd, by = delta_psi)

# from Mike
post_psi_cdf = rep(0, n_breaks)
for(i in 2:n_breaks){
  for(j in 1:Npostimp){
    if(post_psi_values[j] <= breaks[i]){
      post_psi_cdf[i] = post_psi_cdf[i] + weight_vector[j]
    }
  }
}

# check interval covers posterior values of psi.
tolerance = 1e-10  # Define a small tolerance
if(abs(post_psi_cdf[n_breaks] - 1) > tolerance) {
  print("posterior sample outside of the interval!")
} else {print("posterior sample is valid!")}

# compute posterior density of psi
post_psi_dens = rep(0, (n_breaks-1))
for(i in (1:(n_breaks-1))){
  post_psi_dens[i] = post_psi_cdf[i+1] - post_psi_cdf[i]
}
post_psi_dens = post_psi_dens / delta_psi

# compute breaks
par(mfrow = c(1, 2))
prior_psi = hist(prior_psi_graph, breaks, freq = F)
post_psi = hist(post_psi_dens, breaks, freq = F)

# SMOOTHING time
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

prior_psi_dens = average_vector_values(prior_psi$density, 3)
post_psi_dens = average_vector_values(post_psi$density, 3)

# the rest is smoothing -> NEEDS to be re-implemented
RB_psi = post_psi_dens / prior_psi_dens

# side by side plots
mids = post_psi$mids 

par(mfrow = (c(1, 3)))
plot(mids, prior_psi$density, type = "l", xlab = "prior", ylab = "density", col = "red")
plot(mids, post_psi$density, type = "l", xlab = "post", ylab = "density", col = "blue")
plot(mids, RB_psi, type = "l", xlab = "?", ylab = "RB", col = "green")

# estimating plausible region and posterior content
RBest = numeric()
RBmax = max(RB_psi)
for(i in 1:length(mids)){
  if(RB_psi[i] == RBmax){RBest = mids[i]}
}

RBest

plaus_region = ifelse(RB_psi>1, 1, 0)
for(i in 1:length(mids)){
  if(plaus_region[i] == 1){plaus_region[i] = mids[i]}
}

plaus_region

