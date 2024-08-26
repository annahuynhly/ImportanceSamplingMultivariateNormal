######################################################################################################
#Part 6. The Analysis of Variance: accessing the hypothesis that a collection of contrasts are all 0 #
######################################################################################################
# August 26, 2024

# Below, printing out the overall contrast matrix C and the beta_list for reference
C
beta_list

prior_beta_matrix
post_beta_matrix

prior_alpha_matrix = prior_beta_matrix %*% C
post_alpha_matrix = post_beta_matrix  %*% C

# Specify which main effects or order of interactions the user wants to test

# Printing number of factors and levels as a reminder
cat("Number of factors:", m)
cat("Number of levels per factor:", l)

# If the user wants to test the main effect of a factor, they can use the functions below

# Main effect of factor 1

find_factor = function(name_list, levels, main_effect){
  #' Given the vector of beta or alpha names (with their indices), a vector denoting
  #' the number of factors per level and which main effect we are testing for, returns
  #' a vector of betas needed for the hypothesis test.
  
  # getting the coefficient type
  coefficient_type = gsub("[0-9]", "", name_list[1])
  
  factors_before = main_effect - 1
  before_1 = paste(rep(1, factors_before), collapse = "")
  factors_after = length(levels) - main_effect
  after_1 = paste(rep(1, factors_after), collapse = "")
  factor_list = c()
  for(j in 2:levels[main_effect]){
    new_factor = paste(coefficient_type, before_1, j, after_1, sep = "")
    factor_list = c(factor_list, new_factor)
  }
  return(factor_list)
}

factors = find_factor(name_list = beta_list, levels = l, main_effect = 1)

# Main effect of factor 2

factors = find_factor(name_list = beta_list, levels = l, main_effect = 2)

# Denoting the positions of the vector such that we use the correct one.

positions = find_position(factors, beta_list)

# The user can also manually specify the order of interactions they want to test

positions = find_position(c("b12", "b13"), beta_list)


#####################################################################################################
# assess hypothesis (of the interactions) H_0 : psi = psi_0

prior_alpha = numeric(nrow(prior_alpha_matrix))
for(i in 1:nrow(prior_alpha_matrix)){
  prior_alpha[i] = max(abs(prior_alpha_matrix[i,][positions]))
}

post_alpha = numeric(nrow(post_alpha_matrix))
for(i in 1:nrow(post_alpha_matrix)){
  post_alpha[i] = max(abs(post_alpha_matrix[i,][positions]))
}

delta = 0.5 # The meaningful differences for the ANOVA case
upper_bd = delta * ceiling(max(c(prior_alpha, post_alpha))/delta) + 0.5 * delta
breaks_anova = c(c(0, delta/2), seq(delta/2 + delta, upper_bd, by = delta))

# Getting hist values
smoother = c(3, 3)

psi_anova_hist_vals = psi_plot_vals(delta, smoother, prior_psi = prior_alpha, post_psi = post_alpha,
              lower_bd = 0, upper_bd = breaks_anova[length(breaks_anova)], 
              breaks = breaks_anova, showplot = TRUE)

anova_rbr = rbr_psi(prior_psi_dens_smoothed = psi_anova_hist_vals$prior_psi_dens_smoothed, 
                    post_psi_dens_smoothed = psi_anova_hist_vals$post_psi_dens_smoothed, 
                    psi_mids = psi_anova_hist_vals$psi_mids)

# Estimate of true value of psi from the relative belief ratio
RBest_anova = psi_anova_hist_vals$psi_mids[which.max(anova_rbr)]
cat("RB estimate of psi = ", RBest_anova,"\n")
cat("Maximized RB value = ", which.max(anova_rbr), "\n")

inferences_anova = plausible_region_est(psi_mids = psi_anova_hist_vals$psi_mids, 
                                        RB_psi = anova_rbr, 
                                        post_psi_dens_smoothed = psi_anova_hist_vals$post_psi_dens_smoothed, 
                                        delta)

# estimating plausible region.the values of psi where the RB > 1
inferences_anova$plaus_interval

# getting the posterior content of the plausible region
inferences_anova$plaus_content

#####################################################################################################
# assess hypothesis H_0 : psi = psi_0
psi_0 = 0

hypo_test = psi_hypothesis_test(psi_0, 
                                psi_mids = psi_anova_hist_vals$psi_mids, 
                                RB_psi = anova_rbr, 
                                post_psi_dens_smoothed = psi_anova_hist_vals$post_psi_dens_smoothed, 
                                delta)

# compute the evidence concerning strength H_0 : psi = psi_0
hypo_test$psi_message

# Compute the strength
hypo_test$strength_message



