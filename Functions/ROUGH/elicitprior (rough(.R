################ BELOW IS FOR ELICITATION OF THE PRIOR!

#library(MASS) # check if these are needed beforehand - from Luai's code.
#library(mvtnorm)

# 1) Specify virtual certainty
alpha = 0.999 #virtual certainty, tells you the accuracy if I recall correctly? 

# 2) Specify m_{1i}, m_{2i} such that m_{1i} < M_{i} < m_{2i} with virtual certainty, so
# Can think of the following as "bounds of mu". These are inputs.
#ai < mu_0i < bi
m1 = c(-5, -5, -5) #ai # USER INPUTS
m2 = c(5, 5, 5) #bi # USER INPUTS

# multivariate mu_0
mu0 = (m1 + m2)/2 # NEED TO COMPUTE THIS/PART OF ALGO.


# I don't think the rest is necessary for now:

#alpha_0 = c(1, 1, 1)
#beta_0 = c(1, 1, 1)
#a_matrix = matrix(alpha_0, nrow=1) #alpha0
#b_matrix = matrix(beta_0, nrow=1) #beta0           
#x = as.matrix(rbind(a_matrix, b_matrix))#; rownames(x)<-c('a','b') # matrix of a,b
#delta = x     # why not assign earlier? check.



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



# basically we are trying to solve the equation








# users are inputting the following:
# gamma = virtual certainty
# mu_1, mu_2 -> this computes mu_0
# s1_i, s2_i -> this computes sigma_0
# need the delta, mu_0, and sigma_0 to solve for the normal.
# HOWEVER,
# alphas, betas -> this is to generate delta... but we don't have alphas, and betas.

# test run to see if we understand everything properly
mu_1 = c(0, 0, 0)
mu_2 = c(2, 2, 2)

mu_0 = (mu_1 + mu_2)/2

s1 = c(1, 1, 1)
s2 = c(2, 2, 2)

sigma_0 = (mu_2 - mu_1)/(2*s1)

# pretend we know alpha, beta for now
alpha = c(1, 1, 1)
beta = c(1, 1, 1)

gamma = 0.99
prob1 = (1+gamma)/2
prob2 = (1-gamma)/2
z_score1 = qnorm(p = prob1, mean = 1, sd = 0)
z_score2 = qnorm(p = prob2, mean = 1, sd = 0)
x = beta[1] * (z_score1)^{2} / s1[1]
y = beta[1] * (z_score2)^{2} / s2[1]

pgamma(x, alpha[1], 1)
pgamma(y, alpha[1], 1)

# when doing bisection method, start with some arbitrary value.

test_a = 1
c1 = (z_score1)^{2} / (s1[1])^{2}
c2 = (z_score2)^{2} / (s2[1])^{2}
test_b = qgamma((1+gamma)/2, test_a, 1)

pgamma(test_b * c2, test_a, 1)



# now starting what I predict the loop is.

# inputs
gamma = 0.8

mu_1 = c(-5,-5,-5)
mu_2 = c(5,5,5)
mu_0 <- (mu_1 + mu_2)/2

# half-length, setting for upper and lower bound of s_1, s_2
s1 = numeric()  # half-length, lower bound            
s2 = numeric()   # half-length, upper bound
const = c(2,2,2) # range for upper s2, delta_i need to be contained in this range 
## (optimal found is between ? to be the lowest to contain delta_i (higher is possible))

for(i in 1:length(mu_0)){ # make it length of p later
  s1 = c(s1, qnorm((1 + gamma)/2))
  s2 = c(s2, const[i]*(qnorm((1 + gamma)/2)))
}

p = length(mu_1) # assuming length of all vectors above are equal

# derived from inputs
prob1 = (1+gamma)/2
prob2 = (1-gamma)/2
z_score1 = qnorm(p = prob1, mean = 1, sd = 0)
z_score2 = qnorm(p = prob2, mean = 1, sd = 0)
c1 = (z_score1)^{2} / (s1)^{2}
c2 = (z_score1)^{2} / (s2)^{2} # MAY BE incorrect.

alphas = c()
betas = c()
for(i in 1:p){
  found = FALSE
  test_alpha = 1
  while(found == FALSE){
    test_beta = qgamma(prob1, test_alpha, 1)/c1[i] # revisit later; might be prob2
    value = pgamma(test_beta * c2[i], test_alpha, 1)
    #value = round(value, 4) # Give it around 4 decimal places. -> ROUND LATER
    cutoff = round((1-gamma)/2, 3)
    if(round(1-value, 3) == cutoff){ # experiment..
      found = TRUE
      alphas = c(alphas, test_alpha)
      betas = c(betas, test_beta)
    } else if(round(value, 3) < cutoff){
      test_alpha = test_alpha*2
    } else if(round(value, 3) > cutoff){
      test_alpha = test_alpha/2
    }
    print("...")
    print(round(value, 3))
    print(test_alpha)
    print(value)
    print(cutoff)
  }
  print("PROGRESS!")
}


# testing to see if it works
x = pgamma(betas[1] * z_score1^{2}/s1[1]^{2}, alphas[1], 1)
y = pgamma(betas[1] * z_score1^{2}/s2[1]^{2}, alphas[1], 1)

x-y
# note: want it to be 0.95

alpha = 0.0001220703
beta = qgamma(prob1, alpha, 1)/c1[1]
# testing to see if it works
x = pgamma(beta * z_score1^{2}/s1[1]^{2}, alpha, 1)
y = pgamma(beta * z_score1^{2}/s2[1]^{2}, alpha, 1)

x-y
# note: want it to be 0.95


round(1-x, 5)


