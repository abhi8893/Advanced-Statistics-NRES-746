############################################################
#####################** Description **######################
# Normal distribution MLE of mu and sigma
############################################################

# Negative Log-Lik function for normal distribution
NLL_normal <- function(obs.data, params){
  return(-sum(dnorm(obs.data, mean = params['mu'], sd = params['sigma'], log = TRUE)))
}

# Random sample
x <- rnorm(100, 1, 2)

# We know that MLE for mu and sigma are
mu.x <- mean(x)
n.x <- length(x)
sd.x <- sd(x)*(n.x-1)/n.x

# Initial param values - 
# Choose wisely in general - Best Guess - Eyeball
params.init <- list(mu=mu.x, sigma=sd.x)

# Optimize!
MLE <- optim(par=unlist(params.init), fn=NLL_normal, obs.data=x)$par

# Plot!
hist(x, ylim=c(0, 0.2), probability = TRUE)
curve(dnorm(x, MLE["mu"], MLE["sigma"]), add=T, col="green")
curve(dnorm(x, mu.x, sd.x), add=T, col="red")


