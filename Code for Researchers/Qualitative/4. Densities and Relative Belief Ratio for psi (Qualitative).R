###########################################################################
# Part 4: Prior Density, Posterior Density and Relative Belief Ratio for  #
# the Parameter of Interest psi                                           #
###########################################################################
# July 23, 2024

# Below we denote the quantity we want to estimate.

# Use this if the contrast is going to be from the contrast matrix C

col_num = 2
contrast = C[,col_num] 

# Here is where you can manually write the coefficients of psi = c'beta 
# Ensure it is the same size as k.
#contrast = c(1, 0, 0, 0, 0, 0)

##########################################################################################################
# Obtain the prior and posterior density of psi

prior_psi = prior_beta_matrix %*% contrast
post_psi = post_beta_matrix %*% contrast

smoother_function = function(psi_density, counts, smoother){
  #' A helper function that makes a smoothed plot of the density of psi.
  #' @param psi_density a vector containing the density of the histogram psi values.
  #' @param counts the counts associated with the histogram.
  #' @param smoother an odd number of points to average prior density values.
  #' 
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

psi_plot_vals = function(delta = 0.5, smoother = c(7, 7), prior_psi, post_psi){
  #' Obtains the smoothed plot of the density of psi for both the prior and the posterior.
  #' @param smoother a vector containing an odd number of points to average prior density values.
  #' The first value is associated for the prior and the second is for the posterior.
  #' @param prior_psi the vector containing the prior psi values.
  #' @param post_psi the vector containing the posterior psi values.
  
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

psi_hist_vals = psi_plot_vals(delta = 0.2, smoother = c(5, 5), prior_psi = prior_psi, 
                              post_psi = post_psi)

prior_psi_dens_smoothed = psi_hist_vals$prior_psi_dens_smoothed
hist_breaks = psi_hist_vals$breaks
post_psi_dens_smoothed = psi_hist_vals$post_psi_dens_smoothed

####################################################################################################
# obtain the relative belief ratio of psi

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

#############################################################################################################
# The plots side-by-side
par(mfrow = (c(1, 3)))

plot(psi_hist_vals$psi_mids, prior_psi_dens_smoothed, 
     type="l", xlab="psi", ylab="density", col = "red",
     main="The prior density of psi")
plot(psi_hist_vals$psi_mids, post_psi_dens_smoothed,  
     type="l", xlab="psi", ylab="density", col = "blue",
     main="The post density of psi")
plot(psi_hist_vals$psi_mids, rbr_psi_vals,
     type = "l", xlab = "psi", ylab = "RBR", col = "green", 
     main="The relative belief ratio of psi")

