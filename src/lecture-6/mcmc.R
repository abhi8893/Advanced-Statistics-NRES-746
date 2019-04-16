############################################################
#####################** Description **######################
# Markov chain Monte Carlo (MCMC)

############################################################

# Why MCMC?
# In many cases we just won't have the computational power
# to partition our parameter space into discrete pixels
# and then for each pixel
# : Evaluate likelihood 
# : Multiply by prior to get weighted Likelihood 
#   i.e. get joint prob dens of param vector and data occuring
# : Finally sum the joint prob dens (weighted likelihoods)
#   to get the denominator

# Bi-variate normal sampling
# CHECK: Verify and check if assigning variable names
#        conflicting with function names causes any problems
# See bvn conditional distribution

# TODO: Check if the sigma assignment can occur in argument only
rbvn <- function(n, mean=c(0, 0), 
                 rho=sigma[1, 2]/sqrt(prod(diag(sigma))),
                 sigma=matrix(c(1,0,0,1), nrow=2)){
  if(missing(sigma) & !missing(rho)){ # Nice functionality to check args! 
    sigma[c(2, 3)] <- rho  # Implement function to get off-diagonal
  }

  x <- rnorm(n, mean[1], sqrt(sigma[1, 1]))
  mu.y <- mean[1] + rho*(sigma[2, 2]/sigma[1, 1])*(x - mean[1])
  y <- rnorm(n, rho*x, sqrt(1 - rho^2))
  
  return(cbind(x, y))
}
  
# TODO: Plot and analyze similiar to Bayes-2_brute-force.R
#       Implement some of it in helper_funcs
samples <- rbvn(1e4, rho=0.98)
par(mfrow=c(3, 2))
xlim <- c(-4, 4); ylim <- c(-4, 4)
plot(samples, col=1:1e4, xlim=xlim, ylim=ylim)
plot(samples, type="l", xlim=xlim, ylim=ylim)
plot(ts(samples[, "x"]), ylab="x", ylim=xlim)
plot(ts(samples[, "y"]), ylab="y", ylim=ylim)
hist(samples[, "x"], main="Marginal distribution", xlab="x", xlim=xlim)
hist(samples[, "y"], main="Marginal distribution", xlab="y", xlim=ylim)


par(mfrow=c(1,1))

library(mvtnorm)

metropolisHastings <-
  function(n,
           mean = c(0, 0),
           rho = sigma[1, 2] / sqrt(prod(diag(sigma))),
           init.vals=mean,
           sigma = matrix(c(1, 0, 0, 1), nrow = 2),
           thin=0, burn.in=0) {
    
    if(missing(sigma) & !missing(rho)){
      sigma[c(2, 3)] <- rho
    }
    
    samples <- matrix(nrow=n, ncol=2)
    old.x <- init.vals[1]
    old.y <- init.vals[2]
    old.prob <- dmvnorm(c(old.x, old.y), mean, sigma)
    
    for (i in 1:n){
      
      # This is the proposal dist. Do not get confused with bvnorm
      # Although may have some connection
      new.x <- rnorm(1, old.x, 0.5)
      new.y <- rnorm(1, old.y, 0.5)
      new.prob <- dmvnorm(c(new.x, new.y), mean, sigma)
      
      # Probaility of accepting the new point
      prob.accept <- min(1, new.prob/old.prob)
      
      # if prob.accept = 0.7
      # Then we if we sample a rand num from uniform distribution
      # 70% of the time it will be less than 0.7
      randn <- runif(1)
      if (randn < prob.accept){
        old.x <- new.x
        old.y <- new.y
        old.prob <- new.prob
      }
      
      samples[i, ] <- c(old.x, old.y)
    }
    
    # TODO: Check if doing the thinning and removing the initial
    #       burn.in vals at the end is better or in the loop
    # if (burn.in != 0){
    #   samples <- samples[-c(1:burn.in), ]
    # }
    # if (thin != 0){
    #   samples <- samples[c(rep(F, thin), T), ]
    # }
    return(samples)
  }

# TODO: This is repeating alot. 
#       Make it DRY
# TODO: Compare above and below plots with same xlim, ylim
# Or foo[seq(1, length(foo), thin)]
# Or foo[seq.int(1, length(foo), thin)]
# TODO: Experiment with init.vals
#       Generally tails are not being sampled
#       if init.vals are too close to maximum posterior
samples <- metropolisHastings(2e4, rho=0.98)
colnames(samples) <- c("x", "y")
par(mfrow=c(3, 2))
plot(samples, col=1:1e4, xlim=xlim, ylim=ylim)
plot(samples, type="l", xlim=xlim, ylim=ylim)
plot(ts(samples[, "x"]), ylab="x", ylim=xlim)
plot(ts(samples[, "y"]), ylab="y", ylim=ylim)
hist(samples[, "x"], main="Marginal distribution", xlab="x", xlim=xlim)
hist(samples[, "y"], main="Marginal distribution", xlab="y", xlim=ylim)

# Always reset the par
par(mfrow=c(1,1))

