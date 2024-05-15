
setwd("/Users/annaly/Downloads")

prior = read.csv("prior_sample1.csv")

# Turning the downloaded files into something we're familar with
names(prior)

mu_indices = grep("^mu_", names(prior))
# getting the prior
mu_matrix = prior[,mu_indices]

# getting a list of sigma matrices
sigma_indices = grep("^sigma_", names(prior))
sigma_values = prior[,sigma_indices]



# TESTING
p = 5

Sigma_matrix = vector("list", nrow(sigma_values))
for(k in 1:nrow(sigma_values)){
  new_sigma_matrix = matrix(NA, nrow = p, ncol = p)
  # Fill in the symmetric matrix
  index = 1
  for (i in 1:p) {
    for (j in i:p) {
      if(i != j){
        new_sigma_matrix[i, j] = as.numeric(sigma_values[k,][index])
        new_sigma_matrix[j, i] = as.numeric(sigma_values[k,][index])
      } else {
        new_sigma_matrix[i, i] = as.numeric(sigma_values[k,][index])
      }
      index = index + 1
    }
  }
  Sigma_matrix[[k]] = new_sigma_matrix
}

Sigma_matrix

# getting xi









imp = read.csv("post_sample1.csv")

