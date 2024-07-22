###########################################################################
# Part 4: Prior Density, Posterior Density and Relative Belief Ratio for  #
# the Parameter of Interest alpha                                         #
###########################################################################
# July 20, 2024

# Below we denote the quantity we want to estimate.

# Use this if the contrast is going to be from the C matrix generated from 
# the minimal sufficient statistics. 

col_num = 2
contrast = C[,col_num] 

# Here is where you can manually write the contrasts - ensure it is the same size as k.
#contrast = c(1, 0, 0, 0, 0, 0)

##########################################################################################################
# Obtain the prior and posterior density of alpha

prior_alpha = (t(contrast) %*% t(prior_beta_matrix))
post_alpha = (t(contrast) %*% t(post_beta_matrix))

smoother_function = function(alpha_density, counts, smoother){
  #' A helper function that makes a smoothed plot of the density of alpha.
  #' @param alpha_density a vector containing the density of the histogram alpha values.
  #' @param counts the counts associated with the histogram.
  #' @param smoother an odd number of points to average prior density values.
  #' 
  alpha_dens_smoothed = alpha_density
  numcells = length(counts)
  halfm = (smoother-1)/2
  for(i in (1+halfm):(numcells-halfm)){
    sum = 0
    for (j in (-halfm):halfm){
      sum = sum + alpha_density[i+j]
    }
    alpha_dens_smoothed[i]=sum/smoother 
  }
  return(alpha_dens_smoothed)
}

alpha_plot_vals = function(Nmontecarlo, delta = 0.5, smoother = c(7, 7),
                           prior_alpha, post_alpha){
  #' Obtains the smoothed plot of the density of alpha for both the prior and the posterior.
  #' @param Nprior represents the Monte Carlo sample size used for the prior.
  #' @param smoother a vector containing an odd number of points to average prior density values.
  #' The first value is associated for the prior and the second is for the posterior.
  #' @param prior_alpha the vector containing the prior alpha values.
  #' @param post_alpha the vector containing the posterior alpha values.
  
  lower_bd = round_any(min(prior_alpha, post_alpha), accuracy = 0.1, f = floor)
  upper_bd = round_any(max(prior_alpha, post_alpha), accuracy = 0.1, f = ceiling)
  
  breaks = seq(lower_bd, upper_bd, by = delta)
  if(breaks[length(breaks)] <= upper_bd){
    breaks = c(breaks, breaks[length(breaks)] + delta)
  }
  
  prior_alpha_hist = hist(prior_alpha, breaks, freq = F)
  alpha_mids = prior_alpha_hist$mids
  post_alpha_hist = hist(post_alpha, breaks, freq = F)
  
  prior_alpha_dens_smoothed = smoother_function(prior_alpha_hist$density, 
                                                prior_alpha_hist$counts, 
                                                smoother[1])
  post_alpha_dens_smoothed = smoother_function(post_alpha_hist$density , 
                                               post_alpha_hist$counts, 
                                               smoother[2])
  
  newlist = list("alpha_mids" = alpha_mids, "prior_alpha_dens" = prior_alpha_hist$density,
                 "prior_alpha_dens_smoothed" = prior_alpha_dens_smoothed,
                 "post_alpha_dens" = post_alpha_hist$density,
                 "post_alpha_dens_smoothed" = post_alpha_dens_smoothed,
                 "breaks" = breaks)
  return(newlist)
}

alpha_hist_vals = alpha_plot_vals(Nmontecarlo = Nprior, delta = 0.2, smoother = c(5, 5),
                                  prior_alpha = prior_alpha, post_alpha = post_alpha)

prior_alpha_dens_smoothed = alpha_hist_vals$prior_alpha_dens_smoothed
hist_breaks = alpha_hist_vals$breaks
post_alpha_dens_smoothed = alpha_hist_vals$post_alpha_dens_smoothed

####################################################################################################
# obtain the relative belief ratio of psi

rbr_alpha = function(prior_alpha_dens_smoothed, post_alpha_dens_smoothed, breaks){
  #' Obtain the relative belief ratio of psi based off of the prior and posterior values.
  #' @param prior_alpha_dens_smoothed represents the prior alpha values.
  #' @param post_alpha_dens_smoothed represents the posterior alpha values.
  #' @param breaks represents the breaks of the histogram.
  
  # Only need to focus on the max value due to endpoints
  numcells = length(breaks)-1
  RB_alpha = rep(0, numcells)
  for (i in 1:numcells){
    if (prior_alpha_dens_smoothed[i] != 0){
      RB_alpha[i] = post_alpha_dens_smoothed[i]/prior_alpha_dens_smoothed[i]}
  }
  return(RB_alpha)
}

rbr_alpha_vals = rbr_alpha(prior_alpha_dens_smoothed, post_alpha_dens_smoothed, 
                           breaks = hist_breaks)

#############################################################################################################
# The plots side-by-side
par(mfrow = (c(1, 3)))

plot(alpha_hist_vals$alpha_mids, prior_alpha_dens_smoothed, 
     type="l", xlab="alpha", ylab="density", col = "red",
     main="The prior density of alpha")
plot(alpha_hist_vals$alpha_mids, post_alpha_dens_smoothed,  
     type="l", xlab="psi", ylab="density", col = "blue",
     main="The post density of alpha")
plot(alpha_hist_vals$alpha_mids, rbr_alpha_vals,
     type = "l", xlab = "psi", ylab = "RBR", col = "green", 
     main="The relative belief ratio of psi")

