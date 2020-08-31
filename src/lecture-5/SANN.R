############################################################
#####################** Description **######################
# OPTIMIZATION
# Simulated Annealling (SANN)
############################################################

source('src/lecture-5/helper_funcs.R')

# Myxomatosis dataset
MyxDat <- MyxoTiter_sum
Myx <- subset(MyxDat, grade == 1)
y <- Myx$titer

# Steps
# 1.Pick a guess and compute LogLik
# 2.Pick a random new point near the old point
# 3.Compute LogLik at new point
# 4.If the LogLik diff > 0; accept the new point
# 5.Else i).Pick a random number from [0,1] :randnum
#       ii).If randnum <= exp(LogLik diff/k); accept it.
# NOTE: Higher k => Harder to accept bad points
# 6.Repeat
# 7.After every N_lowerK, lower the value of k by lowerk



Lik_gamma_onearg <- function(params){
  return(Lik_gamma(y, params))
}

startingVals <- c(shape=80, scale=0.15)
get.newguess <- function(oldguess, 
                     maxjump=c(shape=5, scale=0.05)){

    jump <- sapply(names(oldguess), 
           function(v.name) runif(1, -maxjump[v.name], maxjump[v.name]))
    
    add.v <- c(oldguess, jump) # Combined vector
    newguess <- tapply(add.v, names(add.v), sum) # Add by names
    newguess <- newguess[names(oldguess)]
    return(newguess)
    }

# CHECK: Did the lecture SANN do it in less iterations
#        Even though it also uses 10000 as the counter?
# TODO: Experiment with chi-sq criterion
SANN <- function(startingVals, fn=Lik_gamma_onearg, N=10^4,
                 k=100, N_lowerk=100, lower_k_by=0.8, return.guesses=T){
  
  guesses <- matrix(nrow=N, ncol=length(startingVals))
  colnames(guesses) <- names(startingVals)
  
  oldguess <- startingVals
  for(i in 1:N){
    newguess <- get.newguess(oldguess)
    loglikdif <- fn(newguess) - fn(oldguess)
    if (loglikdif > 0){
      oldguess <- newguess
    }else{
      rand <- runif(1)
      if (rand <= exp(loglikdif/k)){
        oldguess <- newguess
      }
    }
    guesses[i, ] <- oldguess
    if (i%%N_lowerk == 0){
      k <- k*lower_k_by
  }
  }
  
  MLE <- list()
  MLE$vals <- oldguess
  MLE$loglik <- fn(oldguess)
  MLE$iterations <- N
  if (return.guesses){
    MLE$guesses <- guesses
  }
  return(MLE)
}

startingVals <- c(shape=80, scale=0.15)
MLE.sann <- SANN(startingVals)

# TODO: Find a way to just dump these objects from another script to a data folder
shapevec <- seq(10, 100, by=10^(-1))
scalevec <- seq(0.01, 0.3, by=10^(-3))

surface2D <- matrix(nrow=length(shapevec), ncol=length(scalevec))

newparams <- c(shape=NULL, scale=NULL)
for (i in 1:length(shapevec)){
  newparams['shape'] <- shapevec[i]
  for (j in 1:length(scalevec)){
    newparams['scale'] <- scalevec[j]
    surface2D[i, j] <- Lik_gamma(y, newparams)
  }
}


# Visualize the steps
# CHECK: Is it talking too many wrong moves?
# TODO: Experiment with starting vals
image(x=shapevec, y=scalevec, z=surface2D, zlim=c(-1000, -30), col=topo.colors(12))
contour(x=shapevec, y=scalevec, z=surface2D, levels=c(-30, -40, -80, -500), add=T)
lines(MLE.sann$guesses, col="red")
points(MLE.sann$vals[1], MLE.sann$vals[2], col="green", pch=20, cex=3)


