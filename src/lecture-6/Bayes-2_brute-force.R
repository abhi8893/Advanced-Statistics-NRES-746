############################################################
#####################** Description **######################
# Brute-force bayesian posterior calculation
############################################################

# NOTE: Throughout my scipts, I generally subset list 
#       by [[]] rather than $, to emphasize that there must be some way
#       to use the apply family of functions.

# TODO: Refactor paramwidth to param.width (etc for all relevant objs)
#       Use param.names 

source('src/lecture-5/helper_funcs.R')
# Data: Myxomatosis data $titer
# Goal: We need to fit a gamma distribution to the titer data
# We have collected some data, so we could find the MLE of shape and scale
# Values of shape and scale which maximize 
# the "likelihood"/"prob (dens)" of observing the data
# But we have prior knowledge about the parameters shape and scale
library(emdbook)
MyxDat <- MyxoTiter_sum 
Myx <- subset(MyxDat, grade == 1)

params <- c(shape=40, scale=0.15)
# Plot and overlay an arbitrary gamma distribution
hist(Myx$titer, probability = T)
curve(dgamma(x, shape=params[['shape']], 
             scale=params[['scale']]), # Find a way
      add=T, col='red')                                        # To unpack
                                                               # a vector as arg

# Likelihood
# For every possible pair of shape and scale in the param space
# Find the L(theta | data) ; where theta = vector of shape and scale
# Ofcourse we can't find it every possible point 
# even in our limited param space 
# (Did we consider points outside to have 0 prob?)
# So we divide our space into equally sized grids/pixel
# But in many cases we just won't have the computational power
# to partition our parameter space into discrete pixels
# and then for each pixel
# : Evaluate likelihood 
# : Multiply by prior to get weighted Likelihood 
#   i.e. get joint prob dens of param vector and data occuring
# : Finally sum the joint prob dens (weighted likelihoods)
#   to get the denominator
param.names <- c("shape", "scale")
paramwidth <- c(shape=0.1, scale=0.001)
paramvec <- list(shape=seq(10, 100, paramwidth["shape"]), 
                 scale=seq(0.01, 0.3, by=paramwidth["scale"]))
pixelArea <-  prod(paramwidth) # For determining prob density
paramvec.ngrids <- unlist(lapply(paramvec, length))
cat('param space\'s dimension is', 
    paramvec.ngrids['shape'], 'x', 
    paramvec.ngrids['scale'], '(shape x scale)', fill = T)

# That's a lot of calculation
cat('We will be calculating the likelihood of each pair of params given',
    'the data for a total of', prod((paramvec.ngrids)), 'pairs!!!')

# Intitalize likelihood surface
likelihood2D <- matrix(nrow=paramvec.ngrids['shape'], 
                       ncol=paramvec.ngrids['scale'])

newparams <- c(shape=NULL, scale=NULL)

for (i in 1:paramvec.ngrids['shape']){
  newparams['shape'] <- paramvec[['shape']][i]
  for (j in 1:paramvec.ngrids['scale']){
    newparams['scale'] <- paramvec[['scale']][j]
    likelihood2D[i,j] <- Lik_gamma(Myx$titer, newparams, log=F)
  }
}

# Visualize the likelihood surface
image(x=paramvec[['shape']], y=paramvec[['scale']], z=likelihood2D,
      zlim=c(1e-70, 1e-17), col=topo.colors(12))
contour(x=paramvec[['shape']], y=paramvec[['scale']], z=likelihood2D,
      levels=c(1e-18, 1e-17), col=topo.colors(12), add=T)

# MLE estimate would be where likelihood2D is max
ndx <- which(likelihood2D == max(likelihood2D), arr.ind = T)[1,]
names(ndx) <- c("shape", "scale")
MLE <- c(shape=paramvec[['shape']][ndx['shape']],
         scale=paramvec[['scale']][ndx['scale']]) # TODO: Use lapply here

# Prior
# Suppose we have a uniform prior
# Every value in our param space is equally likely
prior2D <- matrix(1, nrow=paramvec.ngrids['shape'], 
                  ncol=paramvec.ngrids['scale'])
prior2D <- prior2D/length(prior2D) # Convert to prob
image(x=paramvec[['shape']], y=paramvec[['scale']], z=prior2D, col=rainbow(10))


# Now we find the joint prob of (for every pair of) theta and data
# i.e. the weighted likelihood
# Calculating joint prob for every pair in param space!
weighted.likelihood <- prior2D*likelihood2D

# The prob (dens) of data
# Unconditional prob (dens) of data
# Denominator
normalization.constant <- sum(weighted.likelihood)

# POSTERIOR
# Calculating posterior for every pair in param space!
posterior2D <- weighted.likelihood/normalization.constant

# Plot the posterior (density) 
# i.e. [Divide by pixelArea]
# i.e. [prob per unit divison of the space]
posterior2D.dens <- posterior2D/pixelArea
image(x=paramvec[['shape']], y=paramvec[['scale']], z=posterior2D.dens,
      zlim=c(0, 3), col=topo.colors(12))
contour(x=paramvec[['shape']], y=paramvec[['scale']], z=posterior2D.dens,
      levels=1:4, add=T, drawlabels=F)

# TODO: Find a way to see graphically the possible contours
#       May have to use lattice levelplot to see contours
# otherwise just use min to max
# Possible contours which
#                        : Contain 95% of the param space
#                        : OR have 5 % space outside [i.e. quantile(p) = 0.05]
possible.contours <- data.frame(contours=seq(0.13e-4, 1e-4, length=1e3),
                                quantile = NA)

for(i in 1:nrow(possible.contours)){
  ndx <- which(posterior2D < possible.contours$contours[i])
  possible.contours$quantile[i] <- sum(posterior2D[ndx])
  # Why sum the posterior2D probs?
  # Let's suppose you want to find the % of data 
  # with posterior prob less than max(posterior2D)
  # Logically this is 1.
  # In the same way we find the % of data
  # less than a specifc prob
}

# Find the point closest to 0.05
index.95p <- which.min(abs(possible.contours$quantile - 0.05))
# Plot the posterior2D (not dens this time) with different color levels
image(x=paramvec[['shape']], y=paramvec[['scale']], z=posterior2D,
      zlim=c(0.5e-11, 5e-4), col=topo.colors(12))
# Add that contour to the plot
contour(x=paramvec[['shape']], y=paramvec[['scale']], z=posterior2D,
        levels=possible.contours$contours[index.95p], col="red",
        drawlabels = FALSE, add = TRUE)


# Let's find the point estimate of the parameters
# Mean and mode point estimate
# TODO: Find the median estimate
stats.params <- list(mean=NULL, mode=NULL)
mode.ndx <- which(posterior2D == max(posterior2D), arr.ind = T)[1, ]
names(mode.ndx) <- c("shape", "scale")
for (param in c("shape", "scale")){
  stats.params$mean[[param]] <- sum(paramvec[[param]]*posterior2D)
  stats.params$mode[[param]] <- paramvec[[param]][mode.ndx[param]]
    }
  
points(stats.params$mean[['shape']], stats.params$mean[['scale']], 
       pch=20, cex=2, col="red")

points(stats.params$mode[['shape']], stats.params$mode[['scale']], 
       pch="X", cex=1.5, col="black")


title("Posterior probability")
legend("topright",
       legend=c("95% confidence interval", "Mean", "Mode"),
       lty=c(1,NA, NA),
       lwd=c(1,NA, NA),
       pch=list(NA,20, "X"), # TODO:BUG: Find a solution for this!
       col=c("red", "red", "black")
)

# Marginal distributions of parameters
# m for margin
parammargins <- c(shape=1, scale=2)
# Prob distribution of each param
# sum accross all mutually-exclusive possiblities [joint probs]
# of how a specific param value can occur in the param space
# CHECK: Compare with the method in lecture-6 and see the intuition behind it.
#        For me, my method seems more intuitive
marginal.dist <- lapply(as.list(parammargins), 
                        function (m) apply(posterior2D, m, sum))

# Verify both are valid prob distributions
all(lapply(marginal.dist, sum) == 1)

par(mfrow=c(2, 1))
for (param in param.names){
  plot(paramvec[[param]], marginal.dist[[param]]/paramwidth[[param]], 
       type="l", col="blue", xlab=param,
       lwd=2, ylab="prob dens", main=paste("Posterior density:", param))
  
  meanval <- stats.params$mean[[param]]
  abline(v=meanval)
  axis(3, at=meanval,  # TODO: create more space b/w margin and title
       labels=round(meanval, 2))
}

# TODO: Create a marginalized prior, likelihood and Posterior plot 
#       for both the params


# Sample parameters from the joint posterior P(shape, scale | data)
# We have many pairs (discrete pixels) of the param space
# Corresponding to each pixel (pair) we have a posterior prob now
# We can just simply sample()
# by specifying:
#               The pairs 
#                  AND
#               The associated posterior probabilities

# Make a general function to sample from n-dimensional posterior
# given the required arguments
SampleFromPosterior <- function(n){
  params.longform <- list()
  params.longform[['shape']] <- rep(paramvec[['shape']], 
                                    times=length(paramvec[['scale']]))
  params.longform[['scale']] <- rep(paramvec[['scale']], 
                                    each=length(paramvec[['shape']]))
  joint.params <- as.data.frame(params.longform)
  probs <- as.vector(posterior2D)
  
  # Which pairs/rows would we sample?
  samples <- sample(c(1:nrow(joint.params)), size=n, replace=T, prob = probs)
  
  return(joint.params[samples, ])
}

samples <- SampleFromPosterior(1e4)

# Function to get stats from the samples
get.stats.df <- function(name, 
                         probs=c(0.025, 0.5, 0.975)
){
  pvec <- samples[, name]
  res <- quantile(pvec, probs = probs)
  res['mean'] <- mean(pvec)
  res['mode'] <- paramvec[[name]][which.max(marginal.dist[[name]])]
  
  names(res)[names(res) == "50%"] <- "median"
  return(res)
}

cols <- setNames(colnames(samples), colnames(samples))
stats.df <- t(as.data.frame(lapply(cols, get.stats.df)))

stats.cols <- c(mean="red", mode="blue", median="green")
stats.cols[c("2.5%", "97.5%")] <- "gray"
stats.cols <- stats.cols[colnames(stats.df)]

# TODO: Adjust panel plot spacing
par(mfrow=c(4, 2), mar=c(2.5, 1.5, 1.5, 1.5))
layout.matrix <- matrix(c(1, 2, 3, 5, 4, 6, 7, 7), 4, 2, byrow=T)
layout(mat = layout.matrix)
layout.show(7) # Numbers correspond to order of plotting

plot(samples, col=1:1e4)
plot(samples, type="l")


for (param in param.names){
  plot(ts(samples[, param]), ylab=param)
  hist(samples[, param], 30, ylab="density", prob=T, xlab=param, 
       main=paste("Marginal Distribution:", param))
  lines(density(samples[, param]), col='red', lwd=2)
  abline(v=stats.df[param, ], 
         col=stats.cols, lty=c(2, 1, 2, 1, 1), lwd=2)
}

plot(1, type="n", axes=FALSE, xlab="", ylab="")

legend("center", inset=0, 
       legend=c("95% CI", "mean", "mode", "median"), 
       col = stats.cols[c("2.5%", "mean", "mode", "median")],
       lwd = 3, cex = 1, horiz=TRUE
)

# Source a single function from a script
# 1. Define an empty function first
# named.list <- function(x){numeric(0)}
# This is not working!
#insertSource("src/lecture-1/t-test_1.R", functions = "named.list")

