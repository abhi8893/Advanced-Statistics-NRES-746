############################################################
#####################** Description **######################
# OPTIMIZATION!!
## Brute-force 
############################################################

source("src/lecture-5/helper_funcs.R")
library(emdbook)
# Data: MyxoTiter_sum
# Myxomatosis dataset
MyxDat <- MyxoTiter_sum
Myx <- subset(MyxDat, grade == 1)
y <- Myx$titer

# Some stats
mu.y <- mean(y)
n.y <- length(y)
var.y <- var(y)*(n.y-1/n.y)

# Now shape k = mean(y)/theta, theta = Var/mean
# TODO: Check why is this not fitting well?
scale = var.y/mu.y
shape = mu.y/scale

# Plot it
hist(Myx$titer, freq=FALSE)

# We want to fit a gamma distribution to it
guess.params <- list(shape=40, scale=0.15)
curve(dgamma(x, shape=guess.params$shape, scale=guess.params$scale), add=T, col="red")

# Optimize to find the MLE
MLE <- suppressWarnings(
  optim(fn=Lik_gamma, par = guess.params, obs.data=y, method="BFGS", control = list(fnscale=-1))
)

bestparams <- MLE$par
# Plot the gamma curve with MLE params on the histogram
curve(dgamma(x, shape = bestparams['shape'], scale = bestparams['scale']), add=T, col='green')


# BRUTE-FORCE method
# Define a 2-d param space

shapevec <- seq(10, 100, by=10^(-1))
scalevec <- seq(0.01, 0.3, by=10^(-3))

surface2D <- matrix(nrow=length(shapevec), ncol=length(scalevec))

newparams <- bestparams
for (i in 1:length(shapevec)){
  newparams['shape'] <- shapevec[i]
  for (j in 1:length(scalevec)){
    newparams['scale'] <- scalevec[j]
    surface2D[i, j] <- Lik_gamma(y, newparams)
  }
}

image(x=shapevec, y=scalevec, z=surface2D, zlim=c(-1000, -30), col=topo.colors(12))
contour(x=shapevec, y=scalevec, z=surface2D, levels=c(-30, -40, -80, -500), add=T)

# At which point in param space is the likelihood maximum?
ndx <- which(surface2D == max(surface2D), arr.ind=T)

MLE.bf <- unlist(list(shape=shapevec[ndx[, 1]], scale=scalevec[ndx[, 2]]))

cat('The MLE estimates by optim are:')
bestparams
cat('The MLE estimates by brute force are')
MLE.bf
