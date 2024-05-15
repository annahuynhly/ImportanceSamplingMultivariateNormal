# The user is expected to modify values for Parts 0, 2, and 4.

# Part 0: Set the working directory ***
setwd("/Users/annaly/Downloads") 

# Importing the files downloaded in the previous sections
prior = read.csv("prior_sample.csv")
imp = read.csv("importance_sample.csv")

# Part 1: Converting the data to be more user-friendly when getting the psi function
convert_for_psi = function(csv_data){
  #' Given a data frame, converts the data to be more usable when computing values for psi.
  
  mu_indices = grep("^mu_", names(csv_data), ignore.case = TRUE)
  p = length(mu_indices)
  mu_matrix = csv_data[,mu_indices]
  
  sigma_indices = grep("^sigma_", names(csv_data), ignore.case = TRUE)
  sigma_values = csv_data[,sigma_indices]
  
  xi_indices = grep("^xi_", names(csv_data), ignore.case = TRUE)
  xi_values = csv_data[,xi_indices]
  
  Sigma_matrix = xi_matrix = vector("list", nrow(sigma_values))
  for(k in 1:nrow(sigma_values)){
    new_sigma_matrix = new_xi_matrix = matrix(NA, nrow = p, ncol = p)
    # Fill in the symmetric matrix
    index = 1
    for (i in 1:p) {
      for (j in i:p) {
        if(i != j){
          new_sigma_matrix[i, j] = as.numeric(sigma_values[k,][index])
          new_sigma_matrix[j, i] = as.numeric(sigma_values[k,][index])
          new_xi_matrix[i, j] = as.numeric(xi_values[k,][index])
          new_xi_matrix[j, i] = as.numeric(xi_values[k,][index])
        } else {
          new_sigma_matrix[i, i] = as.numeric(sigma_values[k,][index])
          new_xi_matrix[i, i] = as.numeric(xi_values[k,][index]) 
        }
        index = index + 1
      }
    }
    Sigma_matrix[[k]] = new_sigma_matrix
    xi_matrix[[k]] = new_xi_matrix
  }
  
  newlist = list("mu_matrix" = mu_matrix, 
                 "Sigma_matrix" = Sigma_matrix,
                 "xi_matrix" = xi_matrix)
  return(newlist)
}

prior_val = convert_for_psi(prior)
imp_val = convert_for_psi(imp)

# Part 2: Modifying the Psi function ***
psifn = function(muval, Sigmaval, xival){
  #' @param muval is a vector containing a row of mu's from 1 to p
  #' @param Sigmaval is a singular variance matrix
  #' @param xival is a precision matrix associated with Sigmaval
  #' The user may change the psi function below, depending on what they wish to make inferences about.
  #' Below, commented out values of psi, are some suggestions:
  #psi = muval[1] # making inference for the first mu value
  #psi = Sigmaval[2, 2] # making inferences for the 2nd row, 2nd column of Sigmaval
  psi = xival[1,1] # making inference for the ijth value of the matrix xi
  return(psi)
}

# Part 3: Computing the psi values to be used for the density
compute_psi_vals = function(mu, Sigma, xi){
  #' Computing the values for psi to be used to make density histograms.
  #' @mu denotes the mu matrix
  #' @Sigma denotes a vector of Sigma matrices
  #' @xi denotes a vector of xi matrices
  N = nrow(mu)
  dens = numeric(N)
  for (i in 1:N){
    dens[i] = as.numeric(psifn(mu[i,], Sigma[[i]], xi[[i]]))
  }
  return(dens)
}

prior_psi_vals = compute_psi_vals(prior_val$mu_matrix, prior_val$Sigma_matrix, prior_val$xi_matrix)
imp_psi_vals = compute_psi_vals(imp_val$mu_matrix, imp_val$Sigma_matrix, imp_val$xi_matrix)

# Part 4: Saving the results in a .csv. This is to be uploaded! ***
write.csv(prior_psi_vals, "prior_psi_vals.csv")
write.csv(imp_psi_vals, "imp_psi_vals.csv")

