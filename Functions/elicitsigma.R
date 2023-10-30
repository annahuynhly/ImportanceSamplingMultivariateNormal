# determining the prior on 1/sigma**2

gamma = 0.99
s1 = 2
s2 = 10
alphaup = 50
alphalow = 0

elicit_sigma = function(gamma, s1, s2, alphaup, alphalow){
  # gamma: probability corresponding to virtual certainty
  # alphaup: bounds on alpha in the gamma_rate(alpha, beta) dist.
  p = gamma # will switch all notations later!
  gam = (1+p)/2
  z0 = qnorm(gam,0,1)
  up = (z0/s1)**2
  low = (z0/s2)**2
  
  # iterate until prob content of s1<= sigma*z0 <= s2 is within eps of p - HARDCODED for now.
  eps = .0001
  maxits = 100
  
  for (i in 1:maxits){
    alpha = (alphalow + alphaup)/2
    beta = qgamma(gam, alpha, 1)/up
    test = pgamma(beta*low, alpha, 1)
    if (abs(test-(1-gam)) <= eps) {
      break 
    }
    if(test < 1 - gam){
      alphaup = alpha
    } else if (test > 1 - gam){
      alphalow = alpha
    }
  }
  newlist = list("up" = up, "low" = low, "alpha" = alpha, "beta" = beta, "z0" = z0)
}

y= elicit_sigma(gamma, s1, s2, alphaup, alphalow)

low = y$low
up = y$up
alpha = y$alpha
beta = y$beta
z0 = y$z0

x = low + (up-low)*c(0:1000)/1000
dens1 = dgamma(x,alpha,beta)
plot(x,dens1,xlab="1/sigma^2",ylab="prior density",type="l")

x2=1/x
dens2= (x^2)*dgamma(x,alpha,beta)
plot(x2,dens2,xlab="sigma^2",ylab="prior density",type="l")

x3=sqrt(1/x)
dens3=2*(x^(3/2))*dgamma(x,alpha,beta)
plot(x3,dens3,xlab="sigma",ylab="prior density",type="l")

x3=z0*sqrt(1/x)
dens3=(2/z0)*(x^(3/2))*dgamma(x,alpha,beta)
plot(x3,dens3,xlab="sigma*z_gam",ylab="prior density",type="l")

# plots of prior densities
#par(mfrow=c(1,4))

# plot the prior density of 1/sigma^2
x=low+(up-low)*c(0:1000)/1000
dens1= dgamma(x,alpha,beta)
plot(x,dens1,xlab="1/sigma^2",ylab="prior density",type="l")
cat("interval for 1/sigma^2","\n")
cat(low,up,"\n")
# mode of prior for 1/sigma^2
if (alpha >1){
  cat((alpha-1)/beta)}

# plot the prior density of sigma^2
x2=1/x
dens2= (x^2)*dgamma(x,alpha,beta)
plot(x2,dens2,xlab="sigma^2",ylab="prior density",type="l")
#cat("interval for sigma^2","\n")
cat(1/up,1/low,"\n")
# mode of prior for sigma^2
beta/(alpha+1)

#plot the prior density of sigma
x3=sqrt(1/x)
dens3=2*(x^(3/2))*dgamma(x,alpha,beta)
plot(x3,dens3,xlab="sigma",ylab="prior density",type="l")
cat("interval for sigma","\n")
cat(sqrt(1/up),sqrt(1/low),"\n")
# mode of prior for sigma
sqrt(2*beta/(1+2*alpha))

#plot the prior density of sigma*z_gam
x3=z0*sqrt(1/x)
dens3=(2/z0)*(x^(3/2))*dgamma(x,alpha,beta)
plot(x3,dens3,xlab="sigma*z_gam",ylab="prior density",type="l")
cat("interval for sigma*z_gam","\n")
cat(s1,s2,"\n")
# mode of prior for sigma*z_gam
z0*sqrt(2*beta/(1+2*alpha))