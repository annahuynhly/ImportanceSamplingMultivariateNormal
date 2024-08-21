#############################
#Part 5. Inferences for psi #
#############################
# August 21, 2024

##############################################################
# Estimate of true value of psi from the relative belief ratio
RBest = rbr_psi_vals[which.max(rbr_psi_vals)]
cat("RB estimate of psi = ", RBest,"\n")

##############################################################
# Using a function to obtain the plausible region

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
                     delta_psi = delta)

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

