################################################################
# MAIN FUNCTIONS                                               #
################################################################

# Formatting ###################################################

important_post_reformat = function(N, p, post_mu, post_xi, weights){
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

SIR_sample_reformat = function(Npostsamp, p, mu_matrix, xi_matrices, Sigma_matrices){
  #' Reformats the data for the user to download.
  #' @param Npostimp represents the Monte Carlo sample size.
  #' @param p represents the number of dimensions.
  
  mu_title = paste("Mu_", 1:p, sep = "")
  weights_title = "Weights"
  
  xi_title = character(p*(p+1)/2)  # Preallocate memory for the vector
  Sigma_title = character(p*(p+1)/2)
  k = 1
  for(i in 1:p){
    for(j in i:p){
      xi_title[k] = paste("Xi_", i, j, sep = "")
      Sigma_title[k] = paste("Sigma_", i, j, sep = "")
      k = k + 1
    }
  }
  xi_matrix = matrix(NA, nrow = Npostsamp, ncol = length(xi_title))
  Sigma_matrix = matrix(NA, nrow = Npostsamp, ncol = length(Sigma_title))
  
  # indices should be the same every time
  indices = which(upper.tri(xi_matrices[,,1], diag=TRUE), arr.ind=TRUE)
  for(i in 1:Npostsamp){ 
    new_row = xi_matrices[,,i][indices[order(indices[,1]),]]
    xi_matrix[i,] = as.vector(new_row)
    
    new_row = Sigma_matrices[[i]][indices[order(indices[,1]),]]
    Sigma_matrix[i,] = as.vector(new_row)
  }
  
  mu_data = as.data.frame(mu_matrix)
  xi_matrix = as.data.frame(xi_matrix)
  Sigma_matrix = as.data.frame(Sigma_matrix)
  
  names(mu_data) = mu_title
  names(xi_matrix) = xi_title
  names(Sigma_matrix) = Sigma_title
  
  result = cbind(mu_data, xi_matrix, Sigma_matrix)
  rownames(result) = 1:Npostsamp
  
  return(result)
}

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
  newlist = list("n" = n, "Ybar" = Ybar, "S" = S)
  return(newlist)
}

# Computations #################################################

importance_sampler_computations = function(Npostimp, n, Ybar, S, p, mu0, lambda0, alpha01, alpha02){
  #' This generates a sample of Npostimp from the importance sampler on (mu, xi) from theorem 4 of the paper.
  #' It also computes the weights and the cumulative weights.
  #' @param Npostimp represents the Monte Carlo sample size.
  #' @param n represents the sample size of Y # note: not in the researcher's version...
  #' @param Y represents the row means of the observed sample.
  #' @param S represents the covariance matrix of the observed sample.
  #' @param p represents the number of dimensions.
  
  lambda0sq = max(lambda0)^2
  Lambda0 = diag(lambda0)
  inv_L0 = diag(1/lambda0)
  
  # generate xi matrices and corresponding Sigma = xi^{-1} matrices
  Sigma = vector("list", Npostimp)
  Sigma_Y = find_inverse_alt((S + n/(1 + n * lambda0sq) * (Ybar - mu0) %*% t(Ybar - mu0)))
  xi = rWishart(Npostimp, df = (n - p - 1), Sigma = Sigma_Y)
  for (i in 1:Npostimp){
    Sigma[[i]] = find_inverse_alt(xi[,,i])
  }
  
  # generate the mu values given the corresponding xi and also compute the importance sampling weights
  mu_Y = ((n + 1/lambda0sq)^-1) * (mu0/lambda0sq + n * Ybar)
  mu_xi = matrix(NA, nrow = Npostimp, ncol = p)
  k_vector = numeric(Npostimp)
  
  for(i in 1:Npostimp){
    mu_Sigma = ((n + 1/lambda0sq)^-1) * Sigma[[i]]
    mu_xi[i,] = mvrnorm(n = 1, mu = mu_Y, Sigma = mu_Sigma) # See Eq 13 (mu conditional on xi)
    # computing the k function
    sigma_ii = diag(Sigma[[i]]) 
    logk = -(1/2) * t(mu_xi[i,] - mu0) %*% (inv_L0 %*% xi[,,i] %*% inv_L0 - (1/lambda0sq)*xi[,,i]) %*% (mu_xi[i,] - mu0)
    logk2 = sum(-(alpha01 + (p+1)/2) * log(sigma_ii) - (alpha02/sigma_ii))
    k_vector[i] = exp(logk + logk2)
  }
  weights_vector = k_vector / sum(k_vector)
  cum_weights = cumsum(weights_vector)
  
  return(list("xi" = xi, "mu_xi" = mu_xi, "Sigma" = Sigma, "weights_vector" = weights_vector, 
              "cum_weights" = cum_weights))
}

SIR_algorithm = function(Npostsamp, cum_weights, p, mu_xi, xi, Sigma){
  #' Using the SIR algorithm from Rubin, values of i in {1, 2, ..., Npostimp}  
  #' and corresponding values of mu, xi and Sigma.   
  #' @param Npostsamp the size of the sample that will be generated.
  #' @param cum_weights the vector containing the cumulative weights.
  #' @param mu the mu matrix.
  #' @param xi the list containing xi's.
  U = runif(Npostsamp)
  i = findInterval(U, cum_weights)+1
  sample_mu_xi = mu_xi[i, ]
  sample_xi = xi[,,i]
  sample_Sigma = Sigma[i]
  newlist = list("sample_mu_xi" = sample_mu_xi, "sample_xi" = sample_xi, "sample_Sigma" = sample_Sigma)
  return(newlist)
}

psifn = function(muval, xival, Sigmaval, row_num, col_num = 1){
  #' @param muval is a matrix of means
  #' @param Sigmaval is a variance matrix
  #' @param xival is a precision matrix associated with variance matrix Sigmaval
  #' @param row_num is the row we're focusing for muval
  #' @param col_num is the column we're focusing for muval
  #' the code here depends on the psi function we wish to make inference about
  psi = muval[row_num,col_num]
  return(psi)
}

# Plots for Psi ################################################

prior_psi = function(Nprior, mu_prior, Sigma_prior, xi_prior, col_num = 1){
  #' Strictly obtains the prior density of psi.
  #' @param Nprior the size of the sample for mu_prior, Sigma_prior, and xi_prior.
  #' @param mu_prior a matrix that contains 
  
  # compute prior sample of psi values 
  prior_psi_vals = numeric(Nprior)
  for (i in 1:Nprior){
    prior_psi_vals[i] = psifn(mu_prior, xi_prior, Sigma, row_num = i, col_num)
  }
  
  newlist = list("prior_psi_vals" = prior_psi_vals,
                 "prior_psi_lower_bd" = min(prior_psi_vals),
                 "prior_psi_upper_bd" = max(prior_psi_vals))
  return(newlist)
}

prior_psi_plot_vals = function(numcells = 100, Nprior, mprior = 7, 
                               mu_prior, Sigma_prior, xi_prior, col_num){
  #' Obtains the smoothed plot of the prior density of psi.
  #' @param numcells represents the number of bins for the plot.
  #' @param Nprior represents the Monte Carlo sample size used for the prior.
  #' @param mprior an odd number of points to average prior density values.
  #' @param mu_prior represents the mu prior matrix to compute psi.
  #' @param Sigma_prior represents the sigma prior matrix to compute psi.
  #' @param xi_prior represents the xi prior matrix to compute psi.
  #' @param col_num the column of interest for the mu prior matrix.
  prior_vals = prior_psi(Nprior, mu_prior, Sigma_prior, xi_prior, col_num)
  prior_psi_vals = prior_vals$prior_psi_vals
  prior_psi_upper_bd = prior_vals$prior_psi_upper_bd
  prior_psi_lower_bd = prior_vals$prior_psi_lower_bd
  
  delta_psi = (prior_psi_upper_bd - prior_psi_lower_bd) /numcells 
  breaks = seq(prior_psi_lower_bd, prior_psi_upper_bd, by = delta_psi)
  
  prior_psi_hist = hist(prior_psi_vals, breaks, freq = F)
  prior_psi_mids = prior_psi_hist$mids
  prior_psi_density = prior_psi_hist$density 
  
  # smoothed plot of the prior density of psi
  prior_psi_dens_smoothed = prior_psi_density
  halfm = (mprior-1)/2
  for(i in (1+halfm):(numcells-halfm)){
    sum = 0
    for (j in (-halfm):halfm){
      sum = sum + prior_psi_density[i+j]
    }
    prior_psi_dens_smoothed[i]=sum/mprior  
  }
  newlist = list("prior_psi_mids" = prior_psi_mids, 
                 "prior_psi_dens_smoothed" = prior_psi_dens_smoothed,
                 "psi_breaks" = breaks,
                 "delta_psi" = delta_psi)
  return(newlist)
}

post_psi = function(Npostimp, numcells = 100, imp_mu, imp_Sigma, imp_xi, imp_weights,
                    breaks, col_num = 1){
  # TODO: RENAME THIS SHIT
  #' Obtains the smoothed plot of the prior density of psi.
  #' @param numcells represents the number of bins for the plot.
  #' @param Npostimp represents the Monte Carlo sample size used for the posterior.
  
  # compute sample of psi values generated by the importance sampler 
  imp_psi_vals = numeric(Npostimp)
  for (i in 1:Npostimp){
    imp_psi_vals[i] = psifn(imp_mu,imp_xi,imp_Sigma, row_num = i, col_num)
  }
  
  # compute the estimate of the posterior cdf of psi
  nbreaks = numcells+1
  post_psi_cdf = rep(0, nbreaks)
  for(i in 2:nbreaks){
    for(j in 1:Npostimp){
      if(imp_psi_vals[j] <= breaks[i]){
        post_psi_cdf[i] = post_psi_cdf[i] + imp_weights[j]
      }
    }
  }
  
  # check interval (prior_psi_lower_bd, prior_psi_upper_bd) covers importance sampling values of psi.
  post_psi_upper_bd = min(imp_psi_vals) - breaks[1] # should be positive
  post_psi_lower_bd = max(imp_psi_vals) - breaks[nbreaks] # should be negative
  
  newlist = list("post_psi_upper_bd" = post_psi_upper_bd, "post_psi_lower_bd" = post_psi_lower_bd,
                 "post_psi_cdf" = post_psi_cdf)
  
  return(newlist)
}

post_psi_plot_vals = function(Npostimp, numcells = 100, mpost = 5,
                              imp_mu, imp_Sigma, imp_xi, imp_weights, breaks,
                              delta_psi, col_num){
  
  post_psi_vals = post_psi(Npostimp, numcells, imp_mu, imp_Sigma, imp_xi, imp_weights, breaks, col_num)
  post_psi_cdf = post_psi_vals$post_psi_cdf
  
  # compute posterior density of psi
  post_psi_density = diff(post_psi_cdf)/delta_psi
  
  # smoothed plot of the posterior density of psi
  # if a plot is too rough smooth by averaging
  # mprior = an odd number of points to average prior density values, (mprior=1,3,5,...) by averaging
  # the density value, (mpost-1)/2 values to the left and (mpost-1)/2 values to the right
  post_psi_dens_smoothed = post_psi_density
  halfm = (mpost-1)/2
  for (i in (1+halfm):(numcells-halfm)){
    sum = 0
    for (j in (-halfm):halfm){
      sum = sum + post_psi_density[i+j]
    }
    post_psi_dens_smoothed[i] = sum/mpost  
  }
  
  return("post_psi_dens_smoothed" = post_psi_dens_smoothed)
}

rbr_psi = function(numcells = 100, prior_psi_dens_smoothed, post_psi_dens_smoothed){
  #' Obtain the relative belief ratio of psi based off of the prior and posterior values.
  #' @param numcells 
  RB_psi = rep(0, numcells)
  for (i in 1:numcells){
    if (prior_psi_dens_smoothed[i] != 0){
      RB_psi[i] = post_psi_dens_smoothed[i]/prior_psi_dens_smoothed[i]}
  }
  return(RB_psi)
}

# Estimations for Psi ##########################################

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
  strength_msg = paste("Strength of the evidence concerning H_0 : psi= psi_0 = ", strength_psi_0, sep = "")
  
  newlist = list("psi_message" = psi_message, "indices" = indices, "strength_message" = strength_msg)
  
  return(newlist)
}

################################################################
# GRAPH FUNCTIONS                                              #
################################################################

psi_cust_plot = function(grid, density, colour_choice = "red",
                        lty_type = 2, transparency = 0.4, plot_title = "Prior",
                        xlim_min = -10, xlim_max = 10){
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
       main= TeX(paste("The", plot_title, "of $\\psi$")),
       xlim = xlim_interval)
  polygon(grid, density, col = area_col, border = NA)
}

psi_priorpost_plot = function(grid, prior_density, post_density, 
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
                            accuracy = 0.1, f = ceiling)
  ylim_vals = c(0, max_val)
  
  plot(grid, prior_density, type = "l", lty = lty_type[1], col = colour_choice[1], 
       xlab = TeX("$\\psi$"), ylab = "Density", 
       main = TeX("The Prior and Posterior Density of $\\psi$"), 
       ylim = ylim_vals, xlim = xlim_interval)
  
  lines(grid, post_density, lty = lty_type[2], col = colour_choice[2])
  
  polygon(grid, prior_density, col = prior_area_col, border = NA)
  polygon(grid, post_density, col = post_area_col, border = NA)
  
  legend("topleft", legend=c("Prior", "Posterior"),
         col = colour_choice, lty = lty_type, cex=0.8)
}
