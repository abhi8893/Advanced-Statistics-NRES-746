############################################################
#####################** Description **######################
# MLE confidence intervals
# Log Likelihood profile
# Instead of fixing nuisance params at MLE 
# and then examining the slice obtained for param of interest
# We instead take the maximum for each value of nuisance param
# and then examine the profiled likelihood obtained
# (where nuisance params have been profiled out)
############################################################

source("src/lecture-4/MLE_mtcars_functions.R")

# Data: mtcars
## Exploring relationship b/w mpg and displacement
## Exponential relationship, normally distributed errors
## params : a, b, c


params <- list(a=33, b=-0.002, c=1)

MLE <- optim(fn=LogLikFunction, par = unlist(params), 
             df=mtcars, yvar="mpg", xvar="disp",
             control = list(fnscale=-1))

# Let's assume variance c is known, a and b are the free params
# Visualize likelihood surface in 2-dimensions
# Basically we have partitioned our space into 500x500 points
# For each combination of a,b 
# we find the log-likelihood of a,b given the data and choice of the model
allvals <- list(a=seq(5, 50, length=500), b=seq(-1/300, -1/800, length=500))
loglikelihood_surface <- matrix(0, nrow=500, ncol=500)

newParams <- MLE$par

for (i in 1:length(allvals$a)){
  newParams['a'] <- allvals$a[i]
  for (j in 1:length(allvals$b)){
    newParams['b'] <- allvals$b[j]
    loglikelihood_surface[i, j] <- LogLikFunction(newParams, mtcars, "mpg", "disp")
  }
}

# Plot the surface
image(x=allvals$a, y=allvals$b, z=loglikelihood_surface, zlim=c(-100, -75), col=topo.colors(12))

# 95% ci
# 95% of the times our likelihood value at MLE must be 
# greater than LogLik_MLE (evaluated at original estimate) - (chi-sq-critical_0.95)/2
conf95 <- qchisq(0.95, 2)/2
contour(x=allvals$a, y=allvals$b, z=loglikelihood_surface, 
        levels=(MLE$value - conf95), add=T, lwd=3, col=gray(0.3))

profile <- list()
reasonable_parameter_values <- list()
conf.ints <- list()
# Param of interest: a
# Nuisance param: b
# Let's profile out b by taking max value of loglik accross the dimension of b
# loglikelihood_surface is axb dimension.

profile$a <- apply(loglikelihood_surface, 1, max)

# The True MLE parameter must be sampled from where the LL > MLE$value - qchisq(0.95, 1)/2
reasonable_parameter_values$a <- allvals$a[profile$a >= MLE$value - qchisq(0.95, 1)/2]
conf.ints$a <- range(reasonable_parameter_values$a)

# Plot out the profile likelihood
plot(allvals$a, profile$a, type="l", main="Log Likelihood profile", 
     xlab="Parameter slice for \'a\'", ylab="Log Likelihood", xlim=c(15, 50))
abline(v=MLE$par['a'], col="blue", lwd=3)
abline(v=conf.ints$a[1], col='blue', lwd=1)
abline(v=conf.ints$a[2], col="blue", lwd=1)
abline(h=MLE$value-qchisq(0.95, 1)/2, col="blue", lty=2)

# Make this into a function
plot.loglikelihood <- function(method=c('slice', 'profile')){
  
}

# Chug and repeat for parameter b
# Param of interest: b
# Nuisance param: b

profile$b <- apply(loglikelihood_surface, 2, max)

# The True MLE parameter must be sampled from where the LL > MLE$value - qchisq(0.95, 1)/2
reasonable_parameter_values$b <- allvals$b[profile$b >= MLE$value - qchisq(0.95, 1)/2]
conf.ints$b <- range(reasonable_parameter_values$b)

# Plot out the profile likelihood
plot(allvals$b, profile$b, type="l", main="Log Likelihood profile", 
     xlab="Parameter slice for \'b\'", ylab="Log Likelihood")
abline(v=MLE$par['b'], col="blue", lwd=3)
abline(v=conf.ints$b[1], col='blue', lwd=1)
abline(v=conf.ints$b[2], col="blue", lwd=1)
abline(h=MLE$value-qchisq(0.95, 1)/2, col="blue", lty=2)


# Compare Profile-likelihood v/s likelihood slice
par(mfrow=c(2, 2))



# Likelihood ratio test
# Deviance D is chi-sq distributed with df=number of parameters that have been fixed
# Basically you start with a simple model with n1 params
# Then you start adding complexity to the model by adding more params r
# So the complex model has params n2 = n1 + r
# H0: The Complex doesn't add any useful information
# i.e. The higher likelihood value obtained is just an artifact of random sampling 
# simple = restricted model, complex = full model
# D = -2(LL(simple) - LL(complex))

## The rule of 2
qchisq(0.95, 1)/2 # 1.92 (Close to 2)
