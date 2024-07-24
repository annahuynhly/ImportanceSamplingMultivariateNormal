############################################################################
#Part 6. Accessing the hypothesis test a collection of contrasts are all 0 #
############################################################################
# July 24, 2024

#####################################################################################################
# assess hypothesis (of the interactions) H_0 : psi = psi_0

convert_alpha_to_beta = function(alpha_list){
  alpha_list = gsub("alpha", "b", alpha_list)
  return(alpha_list)
}

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

prior_alpha = prior_beta_matrix %*% C
post_alpha = post_beta_matrix %*% C

# This is for all contrasts except for alpha11
max_of_contrasts(prior_alpha, post_alpha, delta = 0.5, smoother = c(7, 7), psi_0 = 15)

# This is for a limited amount of contrasts (depends on what the user wants)
max_of_contrasts(prior_alpha, post_alpha, delta = 0.5, smoother = c(7, 7), psi_0 = 15, 
                 levels = c(2, 3), contrasts = c("alpha21","alpha22","alpha23"))

