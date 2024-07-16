# QUALITATIVE factors edition
#############################################################
# Part 0: Data Input and Sufficient Statistics Computations #
#############################################################

# libraries
library(MASS)

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

elicit_prior_beta0_function = function(p, gamma, m1, m2, s1, s2, alpha01, alpha02){
  #' We elicit the prior for beta0 and lambda0.
  #' @param p represents the number of dimensions.
  #' @param gamma represents the virtual certainty.
  #' @param m1 represents the lower bound for beta0.
  #' @param m2 represents the upper bound for beta0.
  #' @param alpha01 a vector generated from sigma, along with alpha02.
  #' s1 and s2 are added as parameters for convenience.
  vectors_of_interest = list(m1, m2, s1, s2, alpha01, alpha02)
  
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

prior_beta0_vals = elicit_prior_beta0_function(p, gamma, m1, m2, s1, s2, alpha01, alpha02)

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

col_num = 1

prior_alpha = (t(C) %*% t(prior_beta_matrix))[col_num,]
post_alpha = (t(C) %*% t(post_beta_matrix))[col_num, ]

alpha_plot_vals = function(Nmontecarlo, smoother = 7, delta = 0.5, alpha_vals){
  #' Obtains the smoothed plot of the density of alpha (applicable to prior and
  #' posterior)
  #' @param Nprior represents the Monte Carlo sample size used for the prior.
  #' @param smoother an odd number of points to average prior density values.
  #' write more later....
  vals = abs(alpha_vals)
  alpha_upper_bd = max(vals)
  
  breaks = seq(0, alpha_upper_bd, by = delta)
  if (tail(breaks, n=1) <= alpha_upper_bd) {
    breaks = c(breaks, breaks[length(breaks)] + delta)
  } 
  
  alpha_hist = hist(vals, breaks, freq = F)
  alpha_mids = alpha_hist$mids
  alpha_density = alpha_hist$density 
  
  # making a smoothed plot of the density of alpba
  alpha_dens_smoothed = alpha_density
  numcells = length(alpha_hist$counts)
  halfm = (smoother-1)/2
  for(i in (1+halfm):(numcells-halfm)){
    sum = 0
    for (j in (-halfm):halfm){
      sum = sum + alpha_density[i+j]
    }
    alpha_dens_smoothed[i]=sum/smoother 
  }
  
  newlist = list("alpha_mids" = alpha_mids, 
                 "alpha_dens" = alpha_density,
                 "alpha_dens_smoothed" = alpha_dens_smoothed,
                 "breaks" = breaks)
  return(newlist)
}

prior_alpha_vals = alpha_plot_vals(Nmontecarlo = Nprior, smoother = 7, delta = 0.5, 
                                   alpha_vals = prior_alpha)

post_alpha_vals = alpha_plot_vals(Nmontecarlo = Npost, smoother = 7, delta = 0.5, 
                                  alpha_vals = post_alpha)

prior_alpha_dens_smoothed = prior_alpha_vals$alpha_dens_smoothed
prior_alpha_breaks = prior_alpha_vals$breaks

post_alpha_dens_smoothed = post_alpha_vals$alpha_dens_smoothed
post_alpha_breaks = post_alpha_vals$breaks

rbr_alpha = function(prior_alpha_dens_smoothed, prior_alpha_breaks, 
                     post_alpha_dens_smoothed, post_alpha_breaks){
  #' Obtain the relative belief ratio of psi based off of the prior and posterior values.
  #' @param prior_alpha_dens_smoothed represents the prior alpha values.
  #' @param prior_alpha_breaks represents the end points of the grid containing the prior alpha values.
  #' @param post_alpha_dens_smoothed represents the posterior alpha values.
  #' @param post_alpha_breaks represents the end points of the grid containing the post alpha values.
  
  # Only need to focus on the max value due to endpoints
  extra = abs(length(prior_alpha_breaks) - length(post_alpha_breaks))
  if(length(prior_alpha_breaks) > length(post_alpha_breaks)){
    post_alpha_dens_smoothed = c(post_alpha_dens_smoothed, rep(0, extra))
    rbr_breaks = prior_alpha_breaks
  } else {
    prior_alpha_dens_smoothed = c(prior_alpha_dens_smoothed, rep(0, extra))
    rbr_breaks = post_alpha_breaks
  }
  numcells = length(rbr_breaks)-1
  RB_alpha = rep(0, numcells)
  for (i in 1:numcells){
    if (prior_alpha_dens_smoothed[i] != 0){
      RB_alpha[i] = post_alpha_dens_smoothed[i]/prior_alpha_dens_smoothed[i]}
  }
  # getting the midpoints for rbr
  comp_delta = diff(rbr_breaks)[1]
  half_delta = comp_delta/2
  RB_mids = seq(from = rbr_breaks[1] + half_delta, 
                to = rbr_breaks[length(rbr_breaks)] - half_delta, 
                by = comp_delta)
  
  newlist = list("RB_breaks" = rbr_breaks, "RB_mids" = RB_mids,
                 "RB_alpha" = RB_alpha,
                 "post_alpha_dens_smoothed" = post_alpha_dens_smoothed,
                 "prior_alpha_dens_smoothed" = prior_alpha_dens_smoothed)
  return(newlist)
}

rbr_alpha_vals = rbr_alpha(prior_alpha_dens_smoothed, prior_alpha_breaks, 
                           post_alpha_dens_smoothed, post_alpha_breaks)

# Estimations for Psi ##########################################

# May need to change the name of the function arguments, but these are the same
# functions as they were for the other case.
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

plausible_region_est(prior_psi_mids = rbr_alpha_vals$RB_mids, 
                     RB_psi = rbr_alpha_vals$RB_alpha, 
                     post_psi_dens_smoothed = rbr_alpha_vals$post_alpha_dens_smoothed, 
                     delta_psi = 0.5)

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

psi_hypothesis_test(psi_0 = 1, 
                    prior_psi_mids = rbr_alpha_vals$RB_mids, 
                    RB_psi = rbr_alpha_vals$RB_alpha, 
                    post_psi_dens_smoothed = rbr_alpha_vals$post_alpha_dens_smoothed,
                    delta_psi = 0.5)


rbr_alpha_vals$RB_mids[which.max(rbr_alpha_vals$RB_alpha)]


psi_cust_plot = function(grid, density, colour_choice = "red",
                         lty_type = 2, transparency = 0.4, plot_title = "Prior",
                         plot_object = "$\\psi$", xlim_min = -10, xlim_max = 10){
  # TODO: implement col_num (column number) later.
  #' Creates a density plot, used for the prior, posterior, and relative belief ratio for psi.
  #' @param grid vector containing the x-axis values which corresponds to the density.
  #' @param density vector containing the density values.
  #' @param colour_choice the colour used for the line.
  #' @param lty_type the line type used for the plot (same values as base R plotting).
  #' @param transparency transparency percentage for the area of the density plot. 
  #'                     Set to 0 if you don't want the area highlighted.
  #' @param xlim_min smaller x-axis cutoff of the plot.
  #' @param xlim_max larger x-axis cutoff of the plot.
  
  if(xlim_min > min(grid) & xlim_max < max(grid)){
    xlim_interval = c(xlim_min, xlim_max)
  } else {xlim_interval = c(min(grid), max(grid))}
  
  col_rgb = col2rgb(colour_choice)
  area_col = rgb(col_rgb[1]/255, col_rgb[2]/255, col_rgb[3]/255, 
                 alpha = transparency)
  plot(grid, density, type = "l", lty = lty_type, 
       xlab = TeX("$\\psi$"), ylab = "Density", col = colour_choice, 
       main= TeX(paste("The", plot_title, "of", plot_object)),
       xlim = xlim_interval)
  polygon(grid, density, col = area_col, border = NA)
}

psi_priorpost_plot = function(grid, prior_density, post_density, plot_object = "$\\psi$",
                              colour_choice = c("red", "blue"), lty_type = c(2, 2),
                              transparency = 0.4, xlim_min = -10, xlim_max = 10){
  # TODO: implement col_num (column number) later.
  #' Creates a density plot, used for the prior, posterior, and relative belief ratio for psi.
  #' @param grid vector containing the x-axis values which corresponds to the density.
  #' @param prior_density vector containing prior density values.
  #' @param post_density vector containing the posterior density values.
  #' @param colour_choice vector containing colour for the density plot line.
  #'        The first value is for the prior, and the second is for the posterior.
  #' @param lty_type vector containing line type (same values as base R plotting).
  #'        The first value is for the prior, and the second is for the posterior.
  #' @param transparency transparency percentage for the area of the density plot. 
  #'                     Set to 0 if you don't want the area highlighted.
  #' @param xlim_min smaller x-axis cutoff of the plot.
  #' @param xlim_max larger x-axis cutoff of the plot.
  
  if(xlim_min > min(grid) & xlim_max < max(grid)){
    xlim_interval = c(xlim_min, xlim_max)
  } else {xlim_interval = c(min(grid), max(grid))}
  
  prior_col_rgb = col2rgb(colour_choice[1])
  post_col_rgb = col2rgb(colour_choice[2])
  
  prior_area_col = rgb(prior_col_rgb[1]/255, prior_col_rgb[2]/255, prior_col_rgb[3]/255, 
                       alpha = transparency)
  post_area_col = rgb(post_col_rgb[1]/255, post_col_rgb[2]/255, post_col_rgb[3]/255, 
                      alpha = transparency)
  
  max_val = plyr::round_any(max(c(prior_density, post_density)), 
                            accuracy = 0.05, f = ceiling)
  ylim_vals = c(0, max_val)
  
  plot(grid, prior_density, type = "l", lty = lty_type[1], col = colour_choice[1], 
       xlab = TeX(plot_object), ylab = "Density", 
       main = TeX(paste("The Prior and Posterior Density of", plot_object)), 
       ylim = ylim_vals, xlim = xlim_interval)
  
  lines(grid, post_density, lty = lty_type[2], col = colour_choice[2])
  
  polygon(grid, prior_density, col = prior_area_col, border = NA)
  polygon(grid, post_density, col = post_area_col, border = NA)
  
  legend("topleft", legend=c("Prior", "Posterior"),
         col = colour_choice, lty = lty_type, cex=0.8)
}


psi_priorpost_plot(grid = rbr_alpha_vals$RB_mids, 
                   prior_density = rbr_alpha_vals$prior_alpha_dens_smoothed, 
                   post_density = rbr_alpha_vals$post_alpha_dens_smoothed, 
                   plot_object = "$\\alpha_{0}$",
                   colour_choice = c("red", "blue"), lty_type = c(2, 2),
                   transparency = 0.4, xlim_min = 30, xlim_max = 60)

psi_cust_plot(grid = rbr_alpha_vals$RB_mids, 
              density = rbr_alpha_vals$prior_alpha_dens_smoothed, 
              colour_choice = "red", lty_type = 2, transparency = 0.4, 
              plot_title = "Prior", plot_object = "$\\alpha_{0}$",
              xlim_min = 30, xlim_max = 60)

psi_cust_plot(grid = rbr_alpha_vals$RB_mids, 
              density = rbr_alpha_vals$post_alpha_dens_smoothed, 
              colour_choice = "blue", lty_type = 2, transparency = 0.4, 
              plot_title = "Posterior",
              xlim_min = 30, xlim_max = 60)

psi_cust_plot(grid = rbr_alpha_vals$RB_mids, 
              density = rbr_alpha_vals$RB_alpha, 
              colour_choice = "green", lty_type = 2, transparency = 0.4, 
              plot_title = "Relative Belief Ratio",
              xlim_min = 30, xlim_max = 60)





