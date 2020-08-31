############################################################
#####################** Description **######################
# Deviance sampling distribution
############################################################


Lik_normal <- function(obs.data, mu, sigma, log = T){
  return(prod(sum(obs.data, mu, sigma, log=T)))
}
true.params <- list(mu=0, sigma=1)

obs.data <- rnorm(1000, true.params$mu, true.params$sigma)

sim.data <- matrix(0, nrow = 1000, ncol = 100)

get.mle.params <- function(){
  MLE <- optim(fn = Lik_normal, par = unlist())
}
nreps <- 1000
new.params <- true.params
for (i in 1:nreps){
  sim.data[i, ] <- rnorm(100, true.params$mu, true.params$sigma)
}

