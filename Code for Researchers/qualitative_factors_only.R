# QUALITATIVE factors edition
#############################################################
# Part 0: Data Input and Sufficient Statistics Computations #
#############################################################

# setting the seed so that computations can be repeated provided all relevant Parts are run consecutively
set.seed(1)

########################################################################################
#A Here is the code to generate some test data. Inputting data: TBA.                   #

l = c(2, 3) # input the number of levels per factor
m = length(l) # the number of factors.

k = prod(l) # letting this denote the possible number of combinations between the crossed factors 
n = c(5, 5, 5, 5, 5, 5) # the size of n should be equal to k (if factors are crossed) 
# If not, there's a problem.

# Generating Y (for testing purposes)

mu = c(2, 4, 6, 8, 10, 12)
Y = numeric(sum(n))
for(i in 1:k){
  if(i == 1){
    Y[1:n[i]] = rnorm(n[i], mean = mu[i], sd = 2)
  } else {
    Y[(cumsum(n)[i-1]+1):cumsum(n)[i]] = rnorm(n[i], mean = mu[i], sd = 2)
  }
}

################################################################################
# Here is the code for computing X given the information above                 #
################################################################################

qual_generate_X = function(k, n){
  X = matrix(0, nrow = k, ncol = sum(n))
  
  for(i in 1:k) {
    if(i == 1) {
      X[i, 1:n[i]] = 1
    } else {
      start_idx = cumsum(n)[i-1] + 1
      end_idx = cumsum(n)[i]
      X[i, start_idx:end_idx] = 1
    }
  }
  return(t(X))
}

X = qual_generate_X(k, n)

################################################################################
# Here is the code to compute the minimal sufficient statistics                #
################################################################################

qual_Y_metrics = function(X, Y){
  b = solve(t(X) %*% X) %*% t(X) %*% Y
  s_2 = t(Y - X %*% b) %*% (Y - X %*% b)
  
  C = 1
  for(i in 1:m){
    Ci = cbind(rep(1, l[i]), contr.helmert(l[i])) # dropped the normalization
    C = C %x% Ci # kronecker product
  }
  
  newlist = list("b" = b, "s_2" = s_2, "C" = C)
  return(newlist)
}

results = qual_Y_metrics(X, Y)

b = results$b
s_2 = results$s_2
C = results$C

#####################################
# Part 1: Elicitation of the Prior  #
#####################################

# Here is the code to input for the elicitation manually #

p=1
gamma = 0.99
s1 = c(1)
s2 = c(7)
lower_bd = c(0)
upper_bd = c(50)

m1 = c(0, 2, 4, 6, 8, 10)
m2 = c(4, 6, 8, 10, 12, 14)

elicit_prior_sigma_function = function(p, gamma, s1, s2, upper_bd, lower_bd){
  #' 1/sigma^2 ~ gamma(alpha01, alpha02). Here, we elicit the prior for sigma.
  #' We specify s1, s2 such that s1 <= sigma * z_{(1+gamma)/2} <= s2
  #' Then the values alpha01, alpha02 are solved for:
  #' Gamma(alpha01, 1, alpha02 * z^{2}_{(1+p)/2}/s^{2}_{1}) = (1+gamma)/2
  #' Gamma(alpha01, 1, alpha02 * z^{2}_{(1+p)/2}/s^{2}_{2}) = (1-gamma)/2
  #' Using the bisection method.
  #' @param p represents the number of dimensions.
  #' @param gamma represents the virtual certainty.
  vectors_of_interest = list(s1, s2, upper_bd, lower_bd)
  for(i in vectors_of_interest){
    if(length(i) != p){return("Error: there is a vector that doesn't have length p.")}
  }
  
  gam = (1+gamma)/2
  z0 = qnorm(gam,0,1)
  upbdinvsigma2 = (z0/s1)**2 
  lwbdinvsigma2 = (z0/s2)**2 
  
  alpha01 = numeric(p) 
  alpha02 = numeric(p)
  
  for(j in 1:p){
    # iterate until prob content of s1<= sigma*z0 <= s2 is within eps of p 
    eps = .0001
    maxits = 100
    alphalow = lower_bd[j]
    alphaup = upper_bd[j]
    for (i in 1:maxits){
      alpha = (alphalow + alphaup)/2
      beta = qgamma(gam, alpha, 1)/upbdinvsigma2[j]
      test = pgamma(beta*lwbdinvsigma2[j], alpha, 1)
      if (abs(test-(1-gam)) <= eps) { break }
      if (test < 1 - gam) { alphaup = alpha} 
      else if (test > 1 - gam) { alphalow = alpha }
    }
    alpha01[j] = alpha
    alpha02[j] = beta
  }
  
  newlist = list("lwbdinvsigma2" = lwbdinvsigma2, "upbdinvsigma2" = upbdinvsigma2, 
                 "alpha01" = alpha01, "alpha02" = alpha02, 
                 "z0" = z0, "s1" = s1, "s2" = s2)
  return(newlist)
}

elicit_prior_beta0_function = function(p, gamma, m1, m2, s1, s2, alpha01, alpha02){
  #' We elicit the prior for mu, which is given by:
  #' mu ~ mu0 + sqrt(alpha02/alpha01) (lambda0) (t_{2*alpha01}) 
  #' @param p represents the number of dimensions.
  #' @param gamma represents the virtual certainty.
  vectors_of_interest = list(m1, m2, s1, s2, alpha01, alpha02)

  beta0 = (m1 + m2)/2 
  gam = (1+gamma)/2
  lambda0 = (m2 - m1)/(2 * sqrt(alpha02/alpha01) * qt(gam, df = 2 * alpha01))
  
  newlist = list("beta0" = beta0, "lambda0" = lambda0, "m1" = m1, "m2" = m2)
  return(newlist)
}

prior_sigma_vals = elicit_prior_sigma_function(p, gamma, s1, s2, upper_bd, lower_bd)

prior_sigma_vals

alpha01 = prior_sigma_vals$alpha01
alpha02 = prior_sigma_vals$alpha02

z0 = prior_sigma_vals$z0 # this is the (1+gamma)/2 quantile of the standard normal dist.

lwbdinvsigma2 = prior_sigma_vals$lwbdinvsigma2 # This is (z0/s2)^2
upbdinvsigma2 = prior_sigma_vals$upbdinvsigma2 # This is (z0/s1)^2

prior_beta0_vals = elicit_prior_beta0_function(p, gamma, m1, m2, s1, s2, alpha01, alpha02)

lambda0 = prior_beta0_vals$lambda0
beta0 = prior_beta0_vals$beta0

###################################
# Part 2: Sampling from the prior #
###################################

# Nprior = size of sample from the prior

Nprior = 10000

sigma_2 = 1/(rgamma(1, alpha01, alpha02))
beta = rnorm(beta0, lambda0 * sigma_2)

qual_sample_prior = function(Nprior, k, alpha01, alpha02, beta0){
  # k here denotes the number of factors
  # todo: write more detailed documentation later
  prior_sigma_2_vector = rep(0, Nprior)
  prior_beta_matrix = matrix(NA, nrow = Nprior, ncol = k)
  for(i in 1:Nprior){
    prior_sigma_2_vector[i] = 1/(rgamma(1, alpha01, alpha02))
    prior_beta_matrix[i,] = rnorm(beta0, lambda0 * sigma_2)
  }
  newlist = list("prior_sigma_2_vector" = prior_sigma_2_vector,
                 "prior_beta_matrix" = prior_beta_matrix)
  return(newlist)
}

sample_prior_vals = qual_sample_prior(Nprior = Nprior, k = k, alpha01 = alpha01, 
                                      alpha02 = alpha02, beta0 = beta0)

prior_sigma_2_vector = sample_prior_vals$prior_sigma_2_vector
prior_beta_matrix = sample_prior_vals$prior_beta_matrix

#######################################
# Part 3: Sampling from the posterior #
#######################################

# Npost = size of sample from the posterior

Npost = 10000 # change this later

qual_sample_post = function(Npost, k, alpha01, alpha02, lambda0, beta0, b){
  # write the description later
  Lambda0 = diag(lambda0)
  inv_L0 = diag(1/lambda0)
  
  side_eqn = solve(Lambda0 + solve(t(X) %*% X))
  alpha02_y = alpha02 + s1^2/2 + (t(b - beta0) %*% side_eqn %*% (b - beta0))/2
  placeholder = 1 # this is bc I don't know the symbol that well lol (looks like n?)
  
  beta_y = solve(t(X) %*% X + inv_L0) %*% (t(X) %*% X %*% b + inv_L0 %*% beta0)
  
  post_sigma_2_vector = rep(0, Npost)
  post_beta_matrix = matrix(NA, nrow = Npost, ncol = k)
  
  for(i in 1:Npost){
    post_sigma_2 = 1/rgamma(1, placeholder/2 + lambda0, alpha02_y)
    post_sigma_2_vector[i] = post_sigma_2
    post_beta_matrix[i,] = rnorm(beta_y, post_sigma_2 * solve(t(X) %*% X + Lambda0))
  }
  newlist = list("post_sigma_2_vector" = post_sigma_2_vector,
                 "post_beta_matrix" = post_beta_matrix)
  return(newlist)
}

sample_post_vals = qual_sample_post(Npost = Npost, k = k, alpha01 = alpha01, 
                                    alpha02 = alpha02, lambda0 = lambda0, 
                                    beta0 = beta0, b = b)

post_sigma_2_vector = sample_post_vals$post_sigma_2_vector
post_beta_matrix = sample_post_vals$post_beta_matrix

#######################################
# Part 4: Relative Belief Ratio       #
#######################################

alpha = C[,1] %*% t(post_beta_matrix) #might be a typo here?

# a bit hard to read; will discuss later.










