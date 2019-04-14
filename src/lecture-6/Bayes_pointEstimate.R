############################################################
#####################** Description **######################
# Bayesian point estimate
############################################################

# WE DON'T TAKE MODE OF THE POSTERIOR AS POINT ESTIMATE
# Loss function: minimize abs distance => Median
# Loss function: minimize sqr distance => Mean
# Loss function: minimize ??what?? distance => Mode

# TIP: Restart R (Ctrl+shift+F10) for resetting par

# TODO: Make a separate helper function for this.
#       IDEA: Convert n param distr to 1 param distr,
#             Parse argument names. 
# Take a right skewed posterior example
# Values far away to the extreme right
# of the most likely value i.e. mode
# still have a high chance of occuring
curve(dlnorm(x, 4, 1), from=0.001, to=200, ylab="prob density")

posterior.mean <- mean(rlnorm(1e4, 4, 1))
posterior.mode <- optimise(dlnorm, meanlog=4, sdlog=1, lower=10, upper=40, 
                           maximum = T)$maximum
posterior.median <- qlnorm(0.5, 4, 1)
posterior.stats <- c(mean=posterior.mean, mode=posterior.mode, 
                     median=posterior.median)
stats.cols <- c(mean="red", mode="blue", median="green")
abline(v=posterior.stats, col=stats.cols, lty=2)

legend("topright",
  legend = names(posterior.stats),
  col = stats.cols,
  lty = rep(2, 3)
  
)
axis(3, at=posterior.stats, labels=round(posterior.stats, 2))

# Frog example
# Very close mean, mode and median
data <- 3
N <- 10
# Prior is beta(1, 1) uniform => posterior is beta(1 + data, 1 + (10 - data))
posterior_beta <- c( shape1=1+data, shape2=1 + (10 - data))
curve(dbeta(x, posterior_beta['shape1'], posterior_beta['shape2']), 
      from=0, to=1, xlab="param.space", ylab="prob density")

posterior.mean <- mean(rbeta(1e4, posterior_beta['shape1'], 
                             posterior_beta['shape2']))
posterior.mode <- optimise(dbeta, shape1=posterior_beta['shape1'], 
                           shape2=posterior_beta['shape2'], 
                           lower=0.2, upper=0.4, maximum = T)$maximum
posterior.median <- qbeta(0.5, posterior_beta['shape1'], 
                          posterior_beta['shape2'])
posterior.stats <- c(mean=posterior.mean, mode=posterior.mode, 
                     median=posterior.median)
stats.cols <- c(mean="red", mode="blue", median="green")
abline(v=posterior.stats, col=stats.cols, lty=2)

legend("topright",
       legend = paste(names(posterior.stats), "=", round(posterior.stats, 2)),
       col = stats.cols,
       lty = rep(2, 3)
)

# Bayesian parameter uncertainty
# You are 95% sure that the true value lies between the lower and upper bound
posterior_beta <- c( shape1=1+data, shape2=1 + (10 - data))
curve(dbeta(x, posterior_beta['shape1'], posterior_beta['shape2']), 
      from=0, to=1, xlab="param.space", ylab="prob density", col="blue")

# TODO: Shade the inside area
perc.95CI <- qbeta(c(0.025, 0.975), posterior_beta['shape1'], 
                   posterior_beta['shape2'])
names(perc.95CI) <- c("95% lower CI", "95% upper CI")
abline(v=perc.95CI, col=gray(0.5), lty=2, lwd=3)
axis(3, at=perc.95CI, labels=c("2.5%", "97.5%"))

# Highest Posterior Density (HPD) CI
# We need to find an interval such that every data point outside the interval
# has prob dens less than every point inside the interval
# i.e. we choose the 95% most likely values only
# TODO: Show this graphically as well as "calculatively"

# VERIFY: Find a way to verify if this is correct
# 1. Simulate some data from the posterior
sim.data <- rbeta(1e4, posterior_beta['shape1'], posterior_beta['shape2'])
# 2. Find the posterior density at the simulated data
sim.data.posterior <- dbeta(sim.data, posterior_beta['shape1'],
                            posterior_beta['shape2'])
# 3. Eyeball from the plot possible prob dens within which we could 
#    have a 95% coverage of the data
# Here quantile refers to the quantile w.r.t prob density
# i.e. quantile(p) = what is the percentage of data 
#                    which has prob density less than p
possible.probdens <- data.frame(probdens=seq(0.4, 2, by=1e-3), quantile = NA)

# TODO: Implement this in the way it's done in lecture-6/Bayes-2_brute-force.R
for(i in 1:nrow(possible.probdens)){
  # Index of posterior data points having prob dens less than
  # the compared prob dens
  # TODO: Verify by plotting sim.data[ndx] that above is indeed the case
  ndx <- which(sim.data.posterior<possible.probdens$probdens[i])
  # quantile = % of data less than the compared prob dens
  possible.probdens$quantile[i] <- length(ndx)/length(sim.data)
}
# % of data outside = 0.05 ==> % data contained = 0.95
# Find the closest point to 0.05
index.95p <- which.min(abs(possible.probdens$quantile - 0.05))
probdens.95pcritical <- possible.probdens[index.95p, "probdens"]
abline(h=probdens.95pcritical, col="red", lty=2, lwd=3)

legend("topright",
       legend = c("Percentile 95% CI", "HPD 95% CI"),
       col = c(gray(0.5), "red"),
       lty = c(2, 2),
       lwd = c(3, 3),
       cex = 0.8
)
# Find the prob dens at "each" point in the param space
param.space <- seq(0, 1, by=1e-4)
posterior <- dbeta(param.space,
                   shape1=posterior_beta['shape1'], 
                   shape2=posterior_beta['shape2'])
# Find the two points closest to the critical HPD prob dens [horizontal line]
# These will be 
HPD.endpoints.ndx <- order(abs(posterior - probdens.95pcritical))[1:2]
HPD.95CI <- sort(param.space[HPD.endpoints.ndx]) # sort only so that we get
                                                 # c(lower, upper) :NOBIGDEAL
names(HPD.95CI) <- c("95% lower CI", "95% upper CI")
# Compare lengths of CI
# HPD will be the shorter one
# DOUBT: WHY?
length.CI <- c(percentile=diff(as.numeric(perc.95CI)), 
               HPD=diff(as.numeric(HPD.95CI)))
length.CI

