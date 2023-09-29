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
alphas = c(1, 1, 1)
betas = c(1, 1, 1)

deltas = c() # assumption: len(alphas) == len(betas)
# also, this is 1/delta.
for(i in 1:length(alphas)){
  gam = rgamma(1, shape = alphas[i], scale = betas[i]) # need to VERIFY if it is 1/beta
  deltas = c(deltas, gam)
}
# making diag(delta_{1}, delta_{2}, ..., delta_{p})
diagtri = diag(length(alphas))*deltas

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

sigma = diagtri^{1/2} * onion(length(alphas)) * diagtri^{1/2}

# generate mu

# note: GIVEN: mu_0, sigma^{2}-{0}
mu_0 = 10
sigma_0 = 100

# generate normal rvs
z_vector = rnorm(n = length(alphas), mean = 0, sd = 1)

# finally geenrating mu

#mu = mu_0 + sigma_0 * sigma^{1/2} * z_vector
# may need to use the below method instead.
mu = mu_0 + sigma_0 * diag(sigma^{1/2}) * z_vector
mu


####################################################

#trying to write code to generate for the posterior - separate for now.
# this is the sample size - user must input.
n = 100
p = length(alphas) # this will be computed and hardcoded based on alpha01, alpha02
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


# getting the k(SIGMA) function
k_Sigma = 1
for(i in 1:p){
  deltas_inv = 1/deltas[i]
  prod = (deltas_inv)^(-alphas[1] - (p+1)/2) * exp(-betas[1]/deltas_inv)
  k_Sigma = k_Sigma * prod
}

# getting the A(Y) formula
# have n from earlier
AY = (n-1)*S + n*(n * sigma_0^2 + 1)^(-1) * ((Ybar - mu_0) %*% t(Ybar - mu_0))

# finding the inverse
# note: dimensions are okay, i guess there's smth wrong with the inputs lmaoo
inverse_AY = solve(AY)
# to ensure positive definite: need to do a bit of rounding.
inverse_AY = round(inverse_AY, 19) # seens to work for the 19 case -> may need to generalise more in the future.
is.positive.definite(inverse_AY)
isSymmetric(inverse_AY)

N = 100 # monte carlo sample size - user input.
zetas = rWishart(n = N, df = n - p + 1, Sigma = inverse_AY)
# another issue: it has to be the case that n-p+1 has to be greater than dim(inverse_AY)!?

SIGMA_matrices = 1/zetas
# need to iterate to get the sigma is.

temp_mean = (mu_0/(sigma_0^{2}) + n * Ybar)/(1/sigma_0^{2} +n) 
mui_vect = c()
for(i in 1:N){
  SIGMA_i = SIGMA_matrices[i]
  temp_variance = (1/sigma_0^{2} + n)^{-1} * SIGMA_i
  mu_i = mvrnorm(n = 1, mu = temp_mean, Sigma = temp_variance)
}


#sigma = diag(SIGMA)

# now need to generate mu_i

################ BELOW IS FOR ELICITATION OF THE PRIOR!

# blah blah blah
#library(MASS) # check if these are needed beforehand - from Luai's code.
#library(mvtnorm)

# 1) Specify virtual certainty
alpha = 0.999 #virtual certainty, tells you the accuracy if I recall correctly? 

# 2) Specify m_{1i}, m_{2i} such that m_{1i} < M_{i} < m_{2i} with virtual certainty, so

# Can think of the following as "bounds of mu". These are inputs.
#ai < mu_0i < bi
m1 = c(-5, -5, -5) #ai 
m2 = c(5, 5, 5) #bi

# multivariate mu_0
mu0 = (m1+m2)/2

alpha_0 = c(1, 1, 1)
beta_0 = c(1, 1, 1)
a_matrix = matrix(alpha_0, nrow=1) #alpha0
b_matrix = matrix(beta_0, nrow=1) #beta0           
x = as.matrix(rbind(a_matrix, b_matrix))#; rownames(x)<-c('a','b') # matrix of a,b
delta = x     # why not assign earlier? check.



# 3) Specifying s_{1i}, s_{2i}
# half-length, setting for upper and lower bound of s_1, s_2
# NOTE: commented the two lines below for now as it seems unnecessary to declare as numeric. may need to change later.
#s1 = numeric()  # half-length, lower bound            
#s2 = numeric()   # half-length, upper bound
# BELOW IS GIVEN
const = c(2,2,2) # range for upper s2, delta_i need to be contained in this range 
## (optimal found is between ? to be the lowest to contain delta_i (higher is possible))

for (i in 1:length(mu0)){
  s1 = c(s1, qnorm((1+alpha)/2))
  s2 = c(s2, const[i]*(qnorm((1+alpha)/2)))
}



# need to check notes more?



pchisq(23.6848, df = 14, ncp = 0, lower.tail = TRUE) - pchisq(6.57, df = 14, ncp = 0, lower.tail = TRUE)





