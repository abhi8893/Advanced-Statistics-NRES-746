############################################################
#####################** Description **######################
# Sampling distribution of deviance
############################################################


LL_pois <- function(obs.data , lambda){
  return(sum(dpois(obs.data, lambda, log=T)))
}

params <- list(lambda=10)
sim.data <- rpois(100, params$lambda)

nreps <- 1000
sample.size <- length(sim.data)
sim_vals <- matrix(0, nreps, sample.size)

for (i in 1:nreps){
  sim_vals[i, ] <- rpois(100, params$lambda)
}

deviance.vals <- apply(sim_vals, MARGIN = 1, 
                       function(d) -2*(LL_pois(d, 10) - LL_pois(d, mean(d))))

hist(deviance.vals, probability = T)
curve(dchisq(x, 1), add=T)


