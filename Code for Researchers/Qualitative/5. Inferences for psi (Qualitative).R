#############################
#Part 6. Inferences for psi #
#############################
# July 20, 2024

##############################################################
# Estimate of true value of alpha from the relative belief ratio
RBest = rbr_alpha_vals[which.max(rbr_alpha_vals)]
cat("RB estimate of alpha = ", RBest,"\n")

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

inferences = plausible_region_est(prior_psi_mids = alpha_hist_vals$alpha_mids, 
                     RB_psi = rbr_alpha_vals, 
                     post_psi_dens_smoothed = post_alpha_dens_smoothed, 
                     delta_psi = 0.5)

# estimating plausible region.the values of psi where the RB > 1
inferences$plaus_region

# getting the posterior content of the plausible region
inferences$plaus_content

#####################################################################################################
# assess hypothesis H_0 : alpha = alpha_0
alpha_0 = 5

hypo_test = psi_hypothesis_test(psi_0 = alpha_0, 
                                prior_psi_mids = alpha_hist_vals$alpha_mids, 
                                RB_psi = rbr_alpha_vals, 
                                post_psi_dens_smoothed = post_alpha_dens_smoothed,
                                delta_psi = 0.5)

# compute the evidence concerning strength H_0 : psi = psi_0
hypo_test$psi_message

# Compute the strength
hypo_test$strength_message

