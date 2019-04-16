############################################################
#####################** Description **######################
# Myxomatosis posterior gamma distribution fitting
############################################################


library(emdbook)
par(mfrow=c(1,1))
y <- subset(MyxoTiter_sum, grade == 1)$titer


# Overlay an arbitrary gamma curve
params <- c(shape=40, scale=0.15)
hist(y, probability = T)
curve(dgamma(x, shape=params["shape"],  # Find a way to use with function
             scale=params["scale"]), add=T, col="red") # to unpack params

# Define the likelihood function
Lik_gamma <- function(obs.data, params, log=T){
  agg.func <- ifelse(log, sum, prod)
  return(agg.func(dgamma(obs.data, shape=params['shape'], 
                         scale=params['scale'], 
                         log=log)))
}

# Find the MLE of shape and scale
MLE <- optim(fn=Lik_gamma, par=params, obs.data=y, control=list(fnscale=-1))

# Calculate the Likelihood2D surface for a discrete pixel space
param.width <- c(shape=1e-1, scale=1e-3)
param.vec <- list(shape=seq(3, 100, by=param.width['shape']),
                  scale=seq(0.01, 0.5, by=param.width['scale']))
param.size <- unlist(lapply(param.vec, length))

Likelihood2D <- matrix(nrow=param.size['shape'], 
                       ncol=param.size['scale'])

newparams <- c(shape=NULL, scale=NULL)

for(i in 1:param.size['shape']){
  newparams['shape'] <- param.vec[['shape']][i]
  for(j in 1:param.size['scale']){
    newparams['scale'] <- param.vec[['scale']][j]
    Likelihood2D[i, j] <- Lik_gamma(y, newparams, log=T)
  }
}

image(x=param.vec[['shape']], y=param.vec[['scale']], z=Likelihood2D,
      zlim=c(-1000, -30), col=topo.colors(12), xlab="shape", ylab="scale")
contour(x=param.vec[['shape']], y=param.vec[['scale']], z=Likelihood2D,
      levels=c(-30, -40, -80, -500), add=T)


# Get the joint prior prob density of
# a given pair of params [shape, scale]
# Both are assumed to have a gamma prior
# Don't get confused with the gamma prior!
# It might as well have been normal or unif or whatever.
prior_gamma <- function(params, prior.params=list()){
  param.names <- names(params)
  if(missing(prior.params)){
    prior.params[[param.names[1]]] <- c(shape=0.01, scale=100)
    prior.params[[param.names[2]]] <- c(shape=0.001, scale=1000)
  } else{
    
  }
  prior.prob <- c()
  for (p in param.names){
  prior.prob[p] <- dgamma(params[p], 
                          shape=prior.params[[p]]['shape'], 
                          scale=prior.params[[p]]['scale'])
  }
return(prod(prior.prob))
}

PosteriorRatio <- function()
  
22/+
  