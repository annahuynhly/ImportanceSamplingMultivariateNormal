#############################
#Part 5. Inferences for psi #
#############################
# August 26, 2024

##############################################################
# Estimate of true value of psi from the relative belief ratio
RBest = psi_mids[which.max(rbr_psi_vals)]
cat("RB estimate of psi = ", RBest,"\n")
cat("Maximized RB value = ", which.max(rbr_psi_vals), "\n")

# Using a function to obtain the plausible region

plausible_region_est = function(psi_mids, RB_psi, post_psi_dens_smoothed,
                                delta){
  # Obtaining all values where the RB isn't equal to 0
  plaus_region = ifelse(RB_psi > 1, psi_mids, 0)
  # Obtaining the plausible interval
  nonzero_values = plaus_region[plaus_region != 0]
  plaus_interval = c(nonzero_values[1] - delta/2, 
                     nonzero_values[length(nonzero_values)] + delta/2)
  
  # Obtaining the posterior content of the plausible region
  plaus_content = 0
  for(i in 1:length(psi_mids)){
    if(RB_psi[i] > 1){
      # multiplied by delta to ensure the plaus_content does not exceed 1
      plaus_content = plaus_content + post_psi_dens_smoothed[i] * delta
    }
  }
  plaus_content = plaus_content 
  
  newlist = list("plaus_interval" = plaus_interval,
                 "plaus_content" = plaus_content)
  return(newlist)
}

inferences = plausible_region_est(psi_mids, RB_psi = rbr_psi_vals, 
                     post_psi_dens_smoothed, delta)

# estimating plausible region.the values of psi where the RB > 1
inferences$plaus_interval

# getting the posterior content of the plausible region
inferences$plaus_content

#####################################################################################################
# assess hypothesis H_0 : psi = psi_0
psi_0 = 3

psi_hypothesis_test = function(psi_0, psi_mids, RB_psi, post_psi_dens_smoothed,
                               delta){
  
  psi_0_index = which.min(abs(psi_mids - psi_0))
  
  # Evidence for or against psi0
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
  strength_psi_0 = sum(post_psi_dens_smoothed[indices]) * delta
  strength_msg = paste("Strength of the evidence concerning H_0 : psi = psi_0 is given by ", strength_psi_0, sep = "")
  
  newlist = list("psi_message" = psi_message, "indices" = indices, "strength_message" = strength_msg)
  
  return(newlist)
}

hypo_test = psi_hypothesis_test(psi_0, psi_mids, RB_psi = rbr_psi_vals, 
                                post_psi_dens_smoothed, delta)

# compute the evidence concerning strength H_0 : psi = psi_0
hypo_test$psi_message

# Compute the strength
hypo_test$strength_message

