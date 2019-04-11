############################################################
#####################** Description **######################
# Derivative based method
# Newton Raphson method
############################################################

# CHECK: All parameters estimates using all methods are
#        different in my calculations than in lecture
source("src/lecture-5/helper_funcs.R")
library(emdbook)

# Myxomatosis dataset
MyxDat <- MyxoTiter_sum
Myx <- subset(MyxDat, grade == 1)
y <- Myx$titer

# TODO: Refactor params to MLE

params <- get.MLE.titer()$par # MLE

# Find slope of the gamma function
# Let's imagine we know one parameter
# and we are trying to estimate the other
# TODO : Add optional/multiple args using ... and parse var name
Slopefunc <- function(guess, tiny=1e-3){
  diff.wrt <- names(guess) # Differentiate w.r.t
  other.param <- params[names(params) != diff.wrt]
  params[diff.wrt] <- guess # Surprisingly this doesn't change global params
  delta <- c(tiny, 0)
  names(delta) <- c(diff.wrt, other.param)
  h <- Lik_gamma(obs.data=y, params + delta)
  l <- Lik_gamma(obs.data=y, params - delta)
  slope <- (h-l)/(tiny*2)
  
  return(slope)
  
}

# Visualize slope of the curve
param.vec <- seq(10, 100, by=0.1)
names(param.vec) <- rep('shape', length(param.vec))

# TODO: Find a way to use sapply to take names
# directly from single value vector p
func.apply <- function(p){
  params["shape"] = p
  Lik_gamma(y, params)
}
diffparam.name <- unique(names(param.vec))
surface1D <- sapply(param.vec, func.apply)
plot(surface1D~param.vec, type="l", xlab=diffparam.name)

newparams <- params
newparams[diffparam.name] <- 30
point <- Lik_gamma(y, newparams)
slope <- Slopefunc(unlist(list(shape=30)), tiny=0.001)
lines(newparams[diffparam.name] + c(-10, 10), 
      c(point+slope*(-10), point+slope*(10)), col="red")

# 2nd derivative
# Curvature func

CurvatureFunc <- function(guess, tiny=1e-3){
  h <- Slopefunc(guess+tiny, tiny)
  l <- Slopefunc(guess-tiny, tiny)
  curvature <- (h-l)/(tiny*2)
  return(curvature)
}

# TODO: Find a way to use sapply to take names
# directly from single value vector p
func.apply <- function(p, param.name, tiny){
  names(p) <- param.name
  Slopefunc(p, tiny)
}

plot(firstderiv~param.vec, xlab=diffparam.name, type="l")
abline(h=0, col="red")


# Let's find approximately where our derivative was 0
# Since it would not be exactly, we first find value closest to zero
interp <- approx(param.vec, firstderiv, n=10^4) # Pairwise linear interp
names(interp) <- c(diffparam.name, "firstderiv")

mle.derivZero <- interp[[diffparam.name]][(which(interp[["firstderiv"]] 
                                == min(abs(interp[['firstderiv']]))))]

cat("The MLE of", diffparam.name, 
    "calculated by brute-force where derivative is 0", 
    mle.derivZero)
cat("The MLE of", diffparam.name, 
    "calculated by optim (default method) is", 
    params[diffparam.name])

# The Newton-Raphson method
# 1.Pick a guess
# 2.Compute the 1st deriv
# 3.Compute the 2nd deriv
# 4.Assuming linearity, find where 1st deriv == 0
# 5.Set that as the new guess
# 6.Repeat until 1st abs(deriv) falls below tolerance = 0.000001

NewtonMethod <- function(guess, tiny=1e-3, tolerance=1e-7){
  oldguess <- guess
  firstderiv <- Slopefunc(guess, tiny)
  curvature <- CurvatureFunc(guess, tiny)
  
  
  counter=0
  while(abs(firstderiv) >tolerance){
  newguess <- oldguess - firstderiv/curvature
  firstderiv <- Slopefunc(newguess, tiny)
  curvature <- CurvatureFunc(newguess, tiny)
  oldguess <- newguess
  counter=counter + 1 # Good practice for counters?
}
MLE <- list()
MLE$estimate = newguess
newparams <- params
newparams[names(guess)] <- newguess
MLE$loglikelihood = Lik_gamma(y,newparams)
MLE$iterations <- counter

return(MLE)
}

newton.MLE <- NewtonMethod(unlist(list(shape=30)))
newton.MLE
