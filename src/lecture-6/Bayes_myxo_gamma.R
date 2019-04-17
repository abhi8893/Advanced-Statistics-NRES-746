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
MLE <- suppressWarnings(
  optim(fn=Lik_gamma, par=params, obs.data=y, control=list(fnscale=-1)))

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
    Likelihood2D[i, j] <- Lik_gamma(y, newparams, log=F)
  }
}

image(x=param.vec[['shape']], y=param.vec[['scale']], z=log(Likelihood2D),
      zlim=c(-1000, -30), col=topo.colors(12), xlab="shape", ylab="scale")
contour(x=param.vec[['shape']], y=param.vec[['scale']], z=log(Likelihood2D),
      levels=c(-30, -40, -80, -500), add=T)


# Get the joint prior prob density of
# a given pair of params [shape, scale]
# Both are assumed to have a gamma prior
# Don't get confused with the gamma prior!
# It might as well have been normal or unif or whatever.

# DOUBT: If I want to vectorize a function, should I keep the unneccessary
#        conditions to the minimum and tailor make the function?
# TODO: Implement and check above with time profiling.
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

# The prior2D surface for n-discrete pixel space
prior2D <- matrix(nrow=param.size['shape'], 
                       ncol=param.size['scale'])

newparams <- c(shape=NULL, scale=NULL)

for(i in 1:param.size['shape']){
  newparams['shape'] <- param.vec[['shape']][i]
  for(j in 1:param.size['scale']){
    newparams['scale'] <- param.vec[['scale']][j]
    prior2D[i, j] <- prior_gamma(newparams)
  }
}

# The 2D Posterior density surface -brute-force
weighted.likelihood <- prior2D*Likelihood2D
normalization.constant <- sum(weighted.likelihood)
Posterior2D <- weighted.likelihood/normalization.constant
pixel.area <- prod(param.width)
Posterior2D.dens <- Posterior2D/pixel.area

# TODO: Implement an arg to check for non-permissible range
get.newguess <- function(oldguess){
  # How to determine good sd.jump vals?
  # TODO: Check and implement random jump steps approach
  sd.jump <- c(shape=4, scale=0.07)  

  # Make this approach standard instead of for loop
  # or repeating code
  # If possible find a way to do this in one line without too much clutter
  # Don't just combine the two lines
  # NOTE: In apply function; making sure that guess is not negative.
  #       /either the Likelihood or Prior is not NaN
  # IDEA: When it's sufficiently close to 0, 
  #       [determined by the standard dev],
  #       reduce the std dev and make a new guess
  # DOUBT: What should be the new sdev?
  newguess <- lapply(names(sd.jump), 
                    function (n) rnorm(1, oldguess[n], sd.jump[n]))
  newguess <- setNames(unlist(newguess), names(sd.jump))
  is.negative <- names(which(newguess < 0))
  while(length(is.negative) != 0){
    newguess[is.negative] <- unlist(lapply(is.negative, 
                                    function (n) rnorm(1, oldguess[n], 
                                                       sd.jump[n])))
    
    is.negative <- names(which(newguess[is.negative] < 0))
  }

  return(newguess)
}

PosteriorRatio <- function(oldguess, newguess){
  
  guesses <- list(old=oldguess, new=newguess)
  posterior.probs <- lapply(guesses, 
                            function(g) 
                              prod(c(Lik=Lik_gamma(y, g, log=F),
                                     prior=prior_gamma(g))))
  posterior.probs <- setNames(unlist(posterior.probs), names(guesses))
  posterior.probs['old'] <- max(1e-90, posterior.probs['old'])
  post.rat <- as.numeric(posterior.probs['new']/posterior.probs['old'])


  # TIP: I think the last thing to be returned should not have any
  #      operations as if I had to add anymore code to the function;
  #      I don't make any inadvertant mistakes.
  return(post.rat)
}

# CHECK:BUG: The results don't seem correct.
#            Vary too much with init.vals.
# Choose a starting point
init.vals <- c(shape=40, scale=0.13)
n.samples <- 3e4
samples <- matrix(nrow=1e4, ncol=2)
colnames(samples) <- names(param.vec)
new.params <- c(shape=NULL, scale=NULL)

oldguess <- init.vals
for (i in 1:n.samples){
  newguess <- get.newguess(oldguess)
  post.rat <- PosteriorRatio(oldguess, newguess)
  prob.accept <- min(1, post.rat)
  rand.n <- runif(1)
  if (rand.n < post.rat){
    oldguess <- newguess
  }
  samples[i, ] <- oldguess
}



# Tired of using the [[]] notation. 
# TODO: Find a way to unpack args from a list
image(x=param.vec$shape, y=param.vec$scale, z=Posterior2D.dens,
      col=topo.colors(12), xlab="shape", ylab="scale", 
      xlim=c(20, 80), ylim=c(0.05, 0.3))
lines(samples, col="red")

# Compare Brute-force vs Metropolis MCMC
# TODO: Get name of dim by dim number
marginalized <- lapply(c(1, 2), 
                       function(m) apply(Posterior2D, m, sum))
names(marginalized) <- names(param.vec)

marginalized.dens <- lapply(names(marginalized), 
                            function (n) marginalized[[n]]/param.width[n])
names(marginalized.dens) <- names(param.vec)

par(mfrow=c(2, 1))
lapply(names(marginalized), 
       function(n) 
         plot(param.vec[[n]], marginalized[[n]], type='l')
      )


## CHECK:BUG: Brute-force is not matching MCMC
par(mfrow=c(2, 1))
i=0
main.title <- "Posterior Metrolopolis MCMC vs brute-force"
for (p in names(marginalized)){
  hist(samples[, p], probability = T, breaks=20, xlab=p,
       main=ifelse(i ==0, main.title, ""), 
       ylim=c(0, max(marginalized.dens[[p]])*1.2))
  lines(density(samples[, p]), col='green', lty=2)
  lines(param.vec[[p]], marginalized.dens[[p]], type='l', col='red')

  i = i + 1 # R doesn't have ++ or +=
  
}


# TODO: Implement above using layout matrix
# par(mfrow=c(4, 2), mar=c(2.5, 1.5, 1.5, 1.5))
# layout.matrix <- matrix(c(1, 2, 3, 5, 4, 6, 7, 7), 4, 2, byrow=T)
# layout(mat = layout.matrix)
# layout.show(7) # Numbers correspond to order of plotting