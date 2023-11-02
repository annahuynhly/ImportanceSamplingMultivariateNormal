# Need to do: Importance Sampling for the Multivariate Normal Distribution
library(expm) # used for the onion method 
library(powerplus)
library(MASS)
library(matrixcalc)
library(stats)

#########
# Things I still need to do:
# change the notation: alphas -> alpha01, betas -> alpha02 (beta is used for something else)
# deltas -> turn into sigmas
#########

# First: sample from the prior

# Generate 1/delta^_{1}, ..., 1/delta_{p} and make a diag(delta_{1}, ...., delta_{p})

# 1/delta_{1} ~ gamma(aloha_{i}, beta_{i})

# this is the user input!! MAY CHANGE
alpha01 = c(1, 1, 1)
alpha02 = c(1, 1, 1)

deltas = c() # assumption: len(alpha01) == len(alpha02)
# also, this is 1/delta.
for(i in 1:length(alpha01)){
  gam = rgamma(1, shape = alpha01[i], scale = alpha02[i]) # need to VERIFY if it is 1/beta
  deltas = c(deltas, gam)
}
# making diag(delta_{1}, delta_{2}, ..., delta_{p})
diagtri = diag(length(alpha01))*deltas

# generate R (onion method) ##############
vnorm = function(x, t) {
  norm(matrix(x, ncol=1), t)
}

onion = function(dimension) {
  d = dimension + 1
  prev_corr = matrix(1, 1, 1)
  
  for(k in 2:(d-1)){
    # sample y = r^2 from a beta distribution, with alpha_1 = (k-1)/2 and alpha_2 = (d-k)/2
    y = rbeta(1, (k-1)/2, (d-k)/2)
    r = as.matrix(sqrt(y))
    
    # sample a unit vector theta uniformly from the unit ball surface B^(k-1)
    v = matrix(rnorm((k-1)), nrow=1)
    theta = v/vnorm(v, '1')
    
    w = r %*% theta # set w = r theta
    
    # set q = prev_corr^(1/2) w, q to the power of 0.5
    e = eigen(prev_corr)
    VV = e$vectors
    q_prep = VV %*% diag(sqrt(e$values)) %*% t(VV)
    q = (w%*% q_prep)
    
    next_corr = matrix(0, nrow=k, ncol=k)
    next_corr[1:(k-1), 1:(k-1)] = prev_corr # R_k-1
    next_corr[k, 1:(k-1)] = q
    next_corr[1:(k-1), k] = q
    diag(next_corr) = 1
    
    prev_corr = next_corr
  }
  return(next_corr)
}

# find sigma for sigma = diag(delta1, ..., deltap)^{1/2} R diag(delta1, ..., deltap)^{1/2}

sigma = diagtri^{1/2} * onion(length(alpha01)) * diagtri^{1/2}

# generate mu

# note: GIVEN: mu_0, sigma^{2}-{0}
mu_0 = 10
sigma_0 = 100

# generate normal rvs
z_vector = rnorm(n = length(alpha01), mean = 0, sd = 1)

# finally geenrating mu

#mu = mu_0 + sigma_0 * sigma^{1/2} * z_vector
# may need to use the below method instead.
mu = mu_0 + sigma_0 * diag(sigma^{1/2}) * z_vector
mu








####################################################

#trying to write code to generate for the posterior - separate for now.
# this is the sample size - user must input.
n = 100
# note: when writing the r-shiny site, ensure that the condition is ensured:
# n >= 2p (or else the rest does not apply.)

p = length(alpha01) # this will be computed and hardcoded based on alpha01, alpha02
mu = rep(0, p) #temporary dummy data
sigma = diag(p) # identity matrix, for now.
mu_0 = 0
sigma_0 = 1


# generating Y!!
Y = mvrnorm(n = n, mu = mu, Sigma = sigma)
# NOTE: in future versions, perhaps allow the user to input in Y as this is the data.
#deltas #just to refer to

Yprime = t(Y)
Ybar = rowMeans(Yprime)

In = matrix(t(rep(1, n))) # identity column; replace the name later perhaps.
Ybar_t = matrix(Ybar,nrow=1, ncol = 3) # transpose edition.
S = (1/(n-1)) * t(Y - In%*%Ybar_t) %*% (Y - In%*%Ybar_t)

# getting the A(Y) formula
# have n from earlier
AY = (n-1)*S + n*(n * sigma_0^2 + 1)^(-1) * ((Ybar - mu_0) %*% t(Ybar - mu_0))

# finding the inverse
# note: dimensions are okay, i guess there's smth wrong with the inputs lmaoo
#inverse_AY = solve(AY)

#trying to find a method that isn't from the inverse method.
#note: is.positive.definite(AY) == TRUE

# first attempt: spectral decomposition method
test = eigen(AY, symmetric = TRUE, only.values=FALSE)

Q = test$vectors
V_inv = diag(1/test$values)
Q_inv = t(test$vectors)

inverse_AY = Q %*% V_inv %*% Q_inv

is.positive.definite(inverse_AY)

is.positive.definite(round(inverse_AY, 10))

# second event: cholesky fractorization method

a = chol(AY) %*% solve(t(chol(AY)) %*% chol(AY))

a = solve(t(chol(AY)) %*% chol(AY))

is.positive.definite(a)
isSymmetric(a)

# to ensure positive definite: need to do a bit of rounding.
inverse_AY = round(inverse_AY, 18) # seens to work for the 19 case -> may need to generalise more in the future.
is.positive.definite(inverse_AY)
isSymmetric(inverse_AY)

N = 100 # monte carlo sample size - user input.
zetas = rWishart(n = N, df = n - p - 1, Sigma = inverse_AY)
# another issue: it has to be the case that n-p+1 has to be greater than dim(inverse_AY)!?

# need to iterate to get the SIGMA is.
temp_mean = (mu_0/(sigma_0^{2}) + n * Ybar)/(1/sigma_0^{2} +n) 
mui_matrix = c()
for(i in 1:N){
  SIGMA_i = solve(zetas[,,i])
  temp_variance = ((1/sigma_0^{2}) + n)^{-1} * SIGMA_i
  # need to do the rounding things again...
  mu_i = mvrnorm(n = 1, mu = temp_mean, Sigma = temp_variance)
  mui_matrix = rbind(matrix = mui_matrix, c(mu_i)) 
}

#########################################
# getting the k(SIGMA) function -> again, requires iteration
# evaluating the importance sampling estimator.
K_Sigma_vect = c()
for(i in 1:N){
  k_Sigma = 1
  sigma_ii = diag(solve(zetas[,,i])) # getting the diagonals
  for(i in 1:p){
    prod = (sigma_ii[i])^(-alpha01[i] - (p+1)/2) * exp(-alpha02[i]/sigma_ii[i])
    k_Sigma = k_Sigma * prod
  }
  K_Sigma_vect = c(K_Sigma_vect, k_Sigma)
}

# test for h: let h(mu, Sigma) = mu_1 (first coordinate of vector mu)

INh = sum(mui_matrix[, 1]*K_Sigma_vect)/sum(K_Sigma_vect)
# need to do a sanity check if the above expression works properly.







#sigma = diag(SIGMA)

# now need to generate mu_i



