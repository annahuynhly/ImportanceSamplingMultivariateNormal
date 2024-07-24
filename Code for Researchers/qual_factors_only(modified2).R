# QUALITATIVE factors edition
#############################################################
# Part 0: Data Input and Sufficient Statistics Computations #
#############################################################

# libraries
library(MASS) # for mvnorm
library(plyr) # for rounding

# setting the seed so that computations can be repeated provided all relevant Parts are run consecutively
set.seed(1)

########################################################################################
#A Here is the code to generate some test data. Inputting data: TBA.                   #

l = c(2, 3) # input the number of levels per factor
m = length(l) # the number of factors.

k = prod(l) # letting this denote the possible number of combinations between the crossed factors 
n_vector = c(5, 5, 5, 5, 5, 5) # the size of n should be equal to k (if factors are crossed) 
n = sum(n_vector)
# If not, there's a problem.

# Generating Y (for testing purposes)

mu = c(2, 4, 6, 8, 10, 12)
Y = numeric(n)
for(i in 1:k){
  if(i == 1){
    Y[1:n[i]] = rnorm(n_vector[i], mean = mu[i], sd = 2)
  } else {
    Y[(cumsum(n_vector)[i-1]+1):cumsum(n_vector)[i]] = rnorm(n_vector[i], mean = mu[i], sd = 2)
  }
}

################################################################################
# Here is the code for computing X given the information above                 #
################################################################################

qual_generate_X = function(n_vector){
  #' Given the sample size per each combination, generates the X matrix.
  #' @param n a vector, which is the sample size per combination.
  #' @examples
  #' > qual_generate_X(c(1, 2, 1, 4))
  #'      [,1] [,2] [,3] [,4]
  #' [1,]    1    0    0    0
  #' [2,]    0    1    0    0
  #' [3,]    0    1    0    0
  #' [4,]    0    0    1    0
  #' [5,]    0    0    0    1
  #' [6,]    0    0    0    1
  #' [7,]    0    0    0    1
  #' [8,]    0    0    0    1
  X = matrix(0, nrow = length(n_vector), ncol = sum(n_vector))
  
  for(i in 1:length(n_vector)) {
    if(i == 1) {
      X[i, 1:n_vector[i]] = 1
    } else {
      start_idx = cumsum(n_vector)[i-1] + 1
      end_idx = cumsum(n_vector)[i]
      X[i, start_idx:end_idx] = 1
    }
  }
  return(t(X))
}

X = qual_generate_X(n_vector)

################################################################################
# Here is the code to compute the minimal sufficient statistics                #
################################################################################

qual_Y_metrics = function(X, Y){
  #' Given the values of the Y vector and the X matrix, computes the minimal
  #' sufficient statistics (b, s^2, and C).
  #' @param X a matrix.
  #' @param Y a vector containing the data.
  #' @param m represents the number of factors.
  #' @param l a vector that contains the number of levels per factor.
  b = solve(t(X) %*% X) %*% t(X) %*% Y
  s_2 = t(Y - X %*% b) %*% (Y - X %*% b)
  C = 1
  for(i in 1:m){
    Ci = cbind(rep(1, l[i]), contr.helmert(l[i])) # dropped the normalization
    C = C %x% Ci # kronecker product
  }
  newlist = list("b" = b, "s_2" = s_2, "C" = C)
  return(newlist)
}

results = qual_Y_metrics(X, Y)

b = results$b
s_2 = results$s_2
C = results$C

#####################################
# Part 1: Elicitation of the Prior  #
#####################################

# Here is the code to input for the elicitation manually #

p=1 # this is set
gamma = 0.99
s1 = c(1)
s2 = c(7)
lower_bd = c(0)
upper_bd = c(50)

m1 = c(0, 2, 4, 6, 8, 10)
m2 = c(4, 6, 8, 10, 12, 14)

elicit_prior_sigma_function = function(p, gamma, s1, s2, upper_bd, lower_bd){
  #' 1/sigma^2 ~ gamma(alpha01, alpha02). Here, we elicit the prior for sigma.
  #' We specify s1, s2 such that s1 <= sigma * z_{(1+gamma)/2} <= s2
  #' Then the values alpha01, alpha02 are solved for:
  #' Gamma(alpha01, 1, alpha02 * z^{2}_{(1+p)/2}/s^{2}_{1}) = (1+gamma)/2
  #' Gamma(alpha01, 1, alpha02 * z^{2}_{(1+p)/2}/s^{2}_{2}) = (1-gamma)/2
  #' Using the bisection method.
  #' @param p represents the number of dimensions.
  #' @param gamma represents the virtual certainty.
  sigmabounds_of_interest = list(s1, s2, upper_bd, lower_bd)
  for(i in sigmabounds_of_interest){
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

elicit_prior_beta0_function = function(p, gamma, m1, m2, alpha01, alpha02){
  #' We elicit the prior for beta0 and lambda0.
  #' @param p represents the number of dimensions.
  #' @param gamma represents the virtual certainty.
  #' @param m1 represents the lower bound for beta0.
  #' @param m2 represents the upper bound for beta0.
  #' @param alpha01 a vector generated from sigma, along with alpha02.
  
  beta0 = (m1 + m2)/2 
  gam = (1+gamma)/2
  lambda0 = (m2 - m1)/(2 * sqrt(alpha02/alpha01) * qt(gam, df = 2 * alpha01))
  
  newlist = list("beta0" = beta0, "lambda0" = lambda0, "m1" = m1, "m2" = m2)
  return(newlist)
}

prior_sigma_vals = elicit_prior_sigma_function(p, gamma, s1, s2, upper_bd, lower_bd)

prior_sigma_vals

alpha01 = prior_sigma_vals$alpha01
alpha02 = prior_sigma_vals$alpha02

z0 = prior_sigma_vals$z0 # this is the (1+gamma)/2 quantile of the standard normal dist.

lwbdinvsigma2 = prior_sigma_vals$lwbdinvsigma2 # This is (z0/s2)^2
upbdinvsigma2 = prior_sigma_vals$upbdinvsigma2 # This is (z0/s1)^2

prior_beta0_vals = elicit_prior_beta0_function(p, gamma, m1, m2, alpha01, alpha02)

lambda0 = prior_beta0_vals$lambda0
beta0 = prior_beta0_vals$beta0

###################################
# Part 2: Sampling from the prior #
###################################

# Nprior = size of sample from the prior

Nprior = 10000

qual_sample_prior = function(Nprior, k, alpha01, alpha02, beta0, lambda0){
  #' This generates a sample of Nprior from the prior on N(mu, Sigma).
  #' @param Nprior represents the Monte Carlo sample size.
  #' @param k represents the number of possible combinations between the factors.
  #' The values of alpha01, alpha02, beta0, and lambda0 are from the prior elicitation.
  prior_sigma_2_vector = rep(0, Nprior)
  prior_beta_matrix = matrix(NA, nrow = Nprior, ncol = k)
  for(i in 1:Nprior){
    prior_sigma_2_vector[i] = 1/(rgamma(1, alpha01, alpha02))
    prior_Sigma = diag(lambda0 * prior_sigma_2_vector[i])
    prior_beta_matrix[i,] = mvrnorm(mu = beta0, Sigma = prior_Sigma)
  }
  newlist = list("prior_sigma_2_vector" = prior_sigma_2_vector,
                 "prior_beta_matrix" = prior_beta_matrix)
  return(newlist)
}

sample_prior_vals = qual_sample_prior(Nprior = Nprior, k = k, alpha01 = alpha01, 
                                      alpha02 = alpha02, beta0 = beta0, lambda0 = lambda0)

prior_sigma_2_vector = sample_prior_vals$prior_sigma_2_vector
prior_beta_matrix = sample_prior_vals$prior_beta_matrix

#######################################
# Part 3: Sampling from the posterior #
#######################################

# Npost = size of sample from the posterior

Npost = 10000 # change this later

qual_sample_post = function(Npost, X, k, n, alpha01, alpha02, lambda0, beta0, b, s_2){
  #' This generates a sample of Npost from the posterior on N(mu, Sigma).
  #' @param Npost represents the Monte Carlo sample size.
  #' @param k represents the number of possible combinations between the factors.
  #' @param n represents the total sample size.
  #' The values of alpha01, alpha02, and beta0 are from the prior elicitation.
  #' The values of b and s_2 are computed from the minimal sufficient statistic.
  Lambda0 = diag(lambda0)
  inv_L0 = diag(1/lambda0)
  
  side_eqn = solve(Lambda0 + solve(t(X) %*% X))
  alpha02_y = alpha02 + s_2/2 + (t(b - beta0) %*% side_eqn %*% (b - beta0))/2
  
  Sigma_y = solve(t(X) %*% X + solve(Lambda0))
  beta_y = solve(t(X) %*% X + inv_L0) %*% (t(X) %*% X %*% b + inv_L0 %*% beta0)
  
  post_sigma_2_vector = rep(0, Npost)
  post_beta_matrix = matrix(NA, nrow = Npost, ncol = k)
  
  for(i in 1:Npost){
    post_sigma_2 = 1/rgamma(1, n/2 + alpha01, alpha02_y)
    post_sigma_2_vector[i] = post_sigma_2
    post_beta_matrix[i,] = mvrnorm(mu = beta_y, Sigma = post_sigma_2 * Sigma_y)
  }
  newlist = list("post_sigma_2_vector" = post_sigma_2_vector,
                 "post_beta_matrix" = post_beta_matrix)
  return(newlist)
}

sample_post_vals = qual_sample_post(Npost = Npost, X = X, k = k, n = n, alpha01 = alpha01, 
                                    alpha02 = alpha02, lambda0 = lambda0, 
                                    beta0 = beta0, b = b, s_2 = s_2)

post_sigma_2_vector = sample_post_vals$post_sigma_2_vector
post_beta_matrix = sample_post_vals$post_beta_matrix

#######################################
# Part 4: Relative Belief Ratio       #
#######################################

# SINCE l = c(2, 3) we have the following order:
# a11, a12, a21, a22, a31, a32

# Here, if the contrast is going to be from the C matrix generated from the minimal sufficient
# statistics. 

col_num = 2
contrast = C[,col_num] 

# Here is where you can manually write the contrasts - ensure it is the same size as k.
#contrast = c(1, 0, 0, 0, 0, 0)

prior_psi = prior_beta_matrix %*% contrast
post_psi = post_beta_matrix %*% contrast

smoother_function = function(psi_density, counts, smoother){
  #' A helper function that makes a smoothed plot of the density of psi.
  #' @param psi_density a vector containing the density of the histogram psi values.
  #' @param counts the counts associated with the histogram.
  #' @param smoother an odd number of points to average prior density values.
  psi_dens_smoothed = psi_density
  numcells = length(counts)
  halfm = (smoother-1)/2
  for(i in (1+halfm):(numcells-halfm)){
    sum = 0
    for (j in (-halfm):halfm){
      sum = sum + psi_density[i+j]
    }
    psi_dens_smoothed[i]=sum/smoother 
  }
  return(psi_dens_smoothed)
}

# note: this function was altered
psi_plot_vals = function(delta = 0.5, smoother = c(7, 7),
                         prior_psi, post_psi){
  #' Obtains the smoothed plot of the density of psi for both the prior and the posterior.
  #' @param smoother a vector containing an odd number of points to average prior density values.
  #' The first value is associated for the prior and the second is for the posterior.
  #' @param prior_psi the vector containing the prior psi values.
  #' @param post_psi the vector containing the posterior psi values.
  #' @param delta is the bin width of the histogram.
  
  lower_bd = round_any(min(prior_psi, post_psi), accuracy = 0.1, f = floor)
  upper_bd = round_any(max(prior_psi, post_psi), accuracy = 0.1, f = ceiling)
  
  breaks = seq(lower_bd, upper_bd, by = delta)
  if(breaks[length(breaks)] <= upper_bd){
    breaks = c(breaks, breaks[length(breaks)] + delta)
  }
  
  prior_psi_hist = hist(prior_psi, breaks, freq = F)
  psi_mids = prior_psi_hist$mids
  post_psi_hist = hist(post_psi, breaks, freq = F)
  
  prior_psi_dens_smoothed = smoother_function(prior_psi_hist$density, 
                                              prior_psi_hist$counts, 
                                              smoother[1])
  post_psi_dens_smoothed = smoother_function(post_psi_hist$density , 
                                             post_psi_hist$counts, 
                                             smoother[2])
  
  newlist = list("psi_mids" = psi_mids, "prior_psi_dens" = prior_psi_hist$density,
                 "prior_psi_dens_smoothed" = prior_psi_dens_smoothed,
                 "post_psi_dens" = post_psi_hist$density,
                 "post_psi_dens_smoothed" = post_psi_dens_smoothed,
                 "breaks" = breaks)
  return(newlist)
}

psi_hist_vals = psi_plot_vals(delta = 0.2, smoother = c(5, 5), 
                              prior_psi = prior_psi, post_psi = post_psi)

prior_psi_dens_smoothed = psi_hist_vals$prior_psi_dens_smoothed
hist_breaks = psi_hist_vals$breaks
post_psi_dens_smoothed = psi_hist_vals$post_psi_dens_smoothed

rbr_psi = function(prior_psi_dens_smoothed, post_psi_dens_smoothed, breaks){
  #' Obtain the relative belief ratio of psi based off of the prior and posterior values.
  #' @param prior_psi_dens_smoothed represents the prior psi values.
  #' @param post_psi_dens_smoothed represents the posterior psi values.
  #' @param breaks represents the breaks of the histogram.
  
  # Only need to focus on the max value due to endpoints
  numcells = length(breaks)-1
  RB_psi = rep(0, numcells)
  for (i in 1:numcells){
    if (prior_psi_dens_smoothed[i] != 0){
      RB_psi[i] = post_psi_dens_smoothed[i]/prior_psi_dens_smoothed[i]}
  }
  return(RB_psi)
}

rbr_psi_vals = rbr_psi(prior_psi_dens_smoothed, post_psi_dens_smoothed, 
                       breaks = hist_breaks)

##############################################################
# Estimate of true value of psi from the relative belief ratio
RBest = rbr_psi_vals[which.max(rbr_psi_vals)]
cat("RB estimate of psi = ", RBest,"\n")

##############################################################
# Using a function to estimate the rest of the values (will be mentioned below)

plausible_region_est = function(prior_psi_mids, RB_psi, post_psi_dens_smoothed,
                                delta_psi){
  # estimating plausible region
  plaus_region = ifelse(RB_psi > 1, prior_psi_mids, 0)
  
  # getting the interval instead
  nonzero_values = plaus_region[plaus_region != 0]
  plaus_interval = c(nonzero_values[1], nonzero_values[length(nonzero_values)])
  
  # getting the posterior content of the plausible region
  plaus_content = 0
  for(i in 1:length(prior_psi_mids)){
    if(RB_psi[i] > 1){
      plaus_content = plaus_content + post_psi_dens_smoothed[i]
    }
  }
  plaus_content = plaus_content * delta_psi
  
  newlist = list("plaus_region" = plaus_region,
                 "plaus_interval" = plaus_interval,
                 "plaus_content" = plaus_content)
  return(newlist)
}

psi_hypothesis_test = function(psi_0 = -2, prior_psi_mids, RB_psi, post_psi_dens_smoothed,
                               delta_psi){
  
  psi_0_index = which.min(abs(prior_psi_mids - psi_0))
  
  # evidence for or against psi0
  if(RB_psi[psi_0_index] > 1){
    psi_message = paste("RB of psi_0 = ",RB_psi[psi_0_index]," so there is evidence in favor of H_0 : psi = ",
                        psi_0, sep ="")
  }
  else if(RB_psi[psi_0_index] < 1){
    psi_message = paste("RB of psi_0 = ", RB_psi[psi_0_index]," so there is evidence against H_0 : psi = ",
                        psi_0, sep = "")
  }
  else if(RB_psi[psi_0_index] == 1){
    psi_message = paste("RB of psi_0 = ", RB_psi[psi_0_index],
                        " so there is no evidence either in favor of or against H_0 : psi = ",
                        psi_0, sep = "")
  }
  
  # compute the evidence concerning strength H_0 : psi = psi_0
  indices = which(RB_psi <= RB_psi[psi_0_index])
  # Compute the strength
  strength_psi_0 = sum(post_psi_dens_smoothed[indices]) * delta_psi
  strength_msg = paste("Strength of the evidence concerning H_0 : psi = psi_0 is given by ", strength_psi_0, sep = "")
  
  newlist = list("psi_message" = psi_message, "indices" = indices, "strength_message" = strength_msg)
  
  return(newlist)
}

inferences = plausible_region_est(prior_psi_mids = psi_hist_vals$psi_mids, 
                                  RB_psi = rbr_psi_vals, 
                                  post_psi_dens_smoothed = post_psi_dens_smoothed, 
                                  delta_psi = 0.5)

# estimating plausible region.the values of psi where the RB > 1
inferences$plaus_region

# getting the posterior content of the plausible region
inferences$plaus_content

#####################################################################################################
# assess hypothesis H_0 : psi = psi_0
psi_0 = 5

hypo_test = psi_hypothesis_test(psi_0 = psi_0, 
                                prior_psi_mids = psi_hist_vals$psi_mids, 
                                RB_psi = rbr_psi_vals, 
                                post_psi_dens_smoothed = post_psi_dens_smoothed,
                                delta_psi = 0.5)

# compute the evidence concerning strength H_0 : psi = psi_0
hypo_test$psi_message

# Compute the strength
hypo_test$strength_message

##########################################################################################
# accessing the hypothesis that a collection of contrasts are all 0

prior_alpha = prior_beta_matrix %*% C
post_alpha = post_beta_matrix %*% C

# want: person to write all of the alphas to be tested
alpha_list = c("alpha12","alpha13","alpha21","alpha22","alpha23") # this is hard coded
beta_list = create_beta_list_names(levels = l)

convert_alpha_to_beta = function(alpha_list){
  alpha_list = gsub("alpha", "b", alpha_list)
  return(alpha_list)
}

test = convert_alpha_to_beta(alpha_list)
find_position(test, beta_list)

max_of_contrasts = function(prior_alpha, post_alpha, delta, smoother, psi_0, contrasts = NA, 
                            levels = NA){
  #' Does a hypothesis test on the collection of contrasts to see if there's an interaction.
  #' @param prior_alpha is the result of t(C) %*% beta_prior
  #' @param post_alpha is the result if t(C) %*% beta_post
  #' @param delta is the bin width of the histogram.
  #' @param smoother a vector containing an odd number of points to average prior density values.
  #' The first value is associated for the prior and the second is for the posterior.
  #' @param psi_0 is the value of the null hypothesis used to access H_0 : psi = psi_0
  #' @param contrasts contains the alpha contrasts included in the hypothesis test.
  #' @param levels a vector containing the number of levels per factor.

  k = ncol(prior_alpha) # assumption is that ncol(prior) = ncol(post)
  Nprior = nrow(prior_alpha)
  Npost = nrow(post_alpha)
  max_alpha_prior = numeric(Nprior)
  max_alpha_post = numeric(Npost)
  
  if(is.na(contrasts[1]) == TRUE){
    contrast_indices_beta = 2:k
  } else {
    beta_version = convert_alpha_to_beta(contrasts)
    beta_list = create_beta_list_names(levels)
    contrast_indices_beta = find_position(beta_version, beta_list)
  }
  
  for(i in 1:Nprior){
    # ignoring the first column of alphas
    max_alpha_prior[i] = max(abs(prior_alpha[i,][contrast_indices_beta]))
  }
  for(j in 1:Npost){
    max_alpha_post[j] = max(abs(post_alpha[j,][contrast_indices_beta]))
  }
  
  plot_vals = psi_plot_vals(delta, smoother, prior_psi = max_alpha_prior, 
                            post_psi = max_alpha_post)
  
  prior_psi_dens_smoothed = plot_vals$prior_psi_dens_smoothed
  hist_breaks = plot_vals$breaks
  post_psi_dens_smoothed = plot_vals$post_psi_dens_smoothed
  rbr_vals = rbr_psi(prior_psi_dens_smoothed, post_psi_dens_smoothed, hist_breaks)
  
  inferences = plausible_region_est(prior_psi_mids = plot_vals$psi_mids, 
                                    RB_psi = rbr_vals, 
                                    post_psi_dens_smoothed = post_psi_dens_smoothed, 
                                    delta_psi = delta)
  
  hypo_test = psi_hypothesis_test(psi_0 = psi_0, 
                                  prior_psi_mids = plot_vals$psi_mids, 
                                  RB_psi = rbr_vals, 
                                  post_psi_dens_smoothed = post_psi_dens_smoothed,
                                  delta_psi = delta)
  
  
  # Estimate of true value of psi from the relative belief ratio
  RBest = rbr_psi_vals[which.max(rbr_psi_vals)]
  
  newlist = list("RBest" = RBest, "Plausible Region" = inferences$plaus_region,
                 "Posterior Content of the Plausible Region" = inferences$plaus_content,
                 "Evidence Concerning strength H_0 : psi = psi_0" = hypo_test$psi_message,
                 "Strength" = hypo_test$strength_message,
                 "prior_psi_dens_smoothed" = prior_psi_dens_smoothed,
                 "post_psi_dens_smoothed" = post_psi_dens_smoothed,
                 "rbr_vals" = rbr_vals,
                 "plot_mids" = plot_vals$psi_mids)
  return(newlist)
}

max_of_contrasts(prior_alpha, post_alpha, delta = 0.5, smoother = c(7, 7), psi_0 = 15)

max_of_contrasts(prior_alpha, post_alpha, delta = 0.5, smoother = c(7, 7), psi_0 = 15, 
                 levels = c(2, 3), contrasts = c("alpha21","alpha22","alpha23"))




 


