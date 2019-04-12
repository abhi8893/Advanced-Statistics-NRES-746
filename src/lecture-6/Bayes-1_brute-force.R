############################################################
#####################** Description **######################
# Bayesian Analysis -1 
# Brute force method
############################################################

source("src/lecture-6/binomdata_bayesplot.R")

# Frog call example
# We observe a frog in 3 of the 10 visits
data <- 3 # CHECK: In R surprisingly conflicting names 
          # for variables/functions has caused any problems.
# Best choice of model (binomial)
# parameter p: True proportion of frogs in the park
#              Hence equal to probability of observing a frog
#              in one bernoulli trial.
# Here we have a binomial dist, i.e. 10 i.i.d bernoulli trials
# But it's possible to observe 3 frogs in N = 10 trials
# due to random chance even if true proportion p = 0.2 (or 2 frogs)
# Now define param space for p
tiny <- 1e-3
param.space <- seq(0, 1, by=tiny)

# Higher the prob (density) of that data|theta implies
# Higher the "likelihood" of theta|data
likelihood <- dbinom(data, 10, prob = param.space) # Calculate likelihood for each param.space

# Let's suppose we think every value of p is equally likely
# Uniform or beta (shape1=1, shape2=1)
prior <- dbeta(param.space, 1, 1)

# Now the JOINT prob density of observing the "data" AND param theta 
# given the 
#          choice of model for data (binomial)
#          set of hyperparameters (shape1, shape2) for the prior of theta vector (if more than 1)
# Choice of prior model? Or is it implicit when we say set of hyperparameters?

# Intuition for prior x Likelihood [NUMERATOR]
# 1. Joint events
#    P(data, theta) = P(theta) x P(data|theta) = prior x Likelihood
#    First (Choose a theta value) with a P(theta) = prior,
#    Now having chosen theta, find the prob (density) of data
#    i.e. prob density of data | theta = Likelihood
#    Multiply together to 
#    get the Joint prob density of data we observed and a specific value of theta

# 2. Weighted Likelihood
#    We can calculate Likelihood of each value of theta 
#    given the data we observed = Likelihood P(data|theta) = L(theta|data)
#    But how likely is that Likelihood? = P(theta)
#    Hence the weighted likelihood is L(theta|data) x P(theta)

weighted.likelihood <- likelihood*prior

# Normalization constant =
# EVIDENCE =
# DENOMINATOR =
# Unconditional Prob (density) of data
# if we had know with certainty the true value of theta
# then the prob (density) of data would have been Prob(data) (evaluated at true.theta)
# But there is uncertainty in theta,
# Hence the ways in which data can occur is
# We observe data and we observe theta_1
# We observe data and we observe theta_2
# .... and so on over all param space
# = Sum of weighted likelihoods = Sum of joint prob density P(data, theta_i)

normalization.constant <- sum(weighted.likelihood)

# Now finally
# POSTERIOR!
# What is the prob of a value of theta
# given that we have observed all of this data?
# In how many cases of observing the data,
# did we observe the value of theta and data together?
# = [Joint prob (density) of data AND the theta]
# divided by the prob (density) of data
# = weighted.likelihood/sum(weighted likelihood)

# This will sum to 1
posterior <- weighted.likelihood/normalization.constant

# Convert posterior from prob to prob density (we need to do this since we can only take discrete param space)
posterior.dens <- posterior/tiny

# TODO: Check the ranges of each of the plotting variables


# Plot it!
par(mai=c(1, 1, 0, 1))

# prior
plot(param.space, prior, ylim=c(0, 5), type='l', col='blue', 
     lwd=1, lty=2, ylab="Probability density", xlab="param.space")
# Likelihood
scaleby <- 5
points(param.space, likelihood*scaleby, type="l", col="red", lwd=1, lty=2, add=T)
axis(4, at=seq(0, 2, by=0.4), labels=seq(0, 2, by=0.4)/scaleby)
mtext("Likelihood", side=4, col="red", line=3)
# Posterior (density)
points(param.space, posterior.dens, col="blue", lwd=2, lty=1, type="l")

# Add a legend
legend("topright",
  legend = c("prior", "likelihood", "posterior"),
  col = c("blue", "red", "blue"),
  lty = c(2, 2, 1),
  lwd = c(1, 1, 2),
  pt.cex = 2,
  cex = 1,
  text.col = "black",
  horiz = F,
  inset = c(0.02, 0.02))

par(mfrow=c(1, 1))
# The peak of Likelihood matches exactly the peak of posterior!
# MLE = posterior mode (NOT BAYESIAN POINT ESTIMATE!)
# Uninformative prior
prior <- dbeta(param.space, 1, 1)
binomdata.bayesplot(3, 10, prior=prior)
# Now with an informative prior
prior <- dbeta(param.space, 15, 5)
binomdata.bayesplot(3, 10, prior=prior)

# Now with moredata
moredata <- c(3, 1, 6, 2, 3, 2, 6, 1, 3, 3)
binomdata.bayesplot(moredata, 10, prior=prior)

# Now with superinformative prior
prior <- dbeta(param.space, 150, 50)
binomdata.bayesplot(moredata, 10, prior=prior)


# Conjugate prior
# A conjugate prior is a distribution for a param or a param vector
# that matches the data-generating model i.e. it has the same form as the likelihood function.
# Data : binomial, conjugate prior: beta distribution ==> posterior: beta distribution
data <- 3
N <- 10
prior_beta <- c(shape1=15, shape2=5)
# PRIOR
curve(dbeta(x, prior_beta['shape1'], prior_beta['shape2']), ylim=c(0, 5), 
      ylab="Prob density", col="blue", lwd=1, lty=2, xlab="param.space")
# POSTERIOR
posterior_beta <- prior_beta + c(data, N - data)
curve(dbeta(x, posterior_beta['shape1'], posterior_beta['shape2']), col="blue", lwd=2, lty=1, add=T)

# Compare with brute-force calculation
prior <- dbeta(param.space, prior_beta['shape1'], prior_beta['shape2'])
binomdata.bayesplot(data, N, prior=prior, ylim=c(0, 5))

# SAME plot! 
# Analytical is better than brute-force here
# But we won't have an analytical solution most of the times
