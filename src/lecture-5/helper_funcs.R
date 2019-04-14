############################################################
#####################** Description **######################
# Helper functions for optimization
############################################################



Lik_gamma <- function(obs.data, params, log=T){
  agg.func <- ifelse(log == T, sum, prod)
  agg.func(dgamma(obs.data, shape=params['shape'], 
                  scale=params['scale'], log=log))
}

get.MLE.titer <- function(){
  ## How to overcome this?
  ## If I keep this outside
  ## when I source it, it will overwrite existing variables.
  ## If I don't, it will not be DRY.
  library(emdbook)
  MyxDat <- MyxoTiter_sum
  Myx <- subset(MyxDat, grade == 1)
  y <- Myx$titer
  guess.params <- unlist(list(shape=40, scale=0.15))
  MLE <- suppressWarnings(optim(fn=Lik_gamma, par=guess.params, 
                         obs.data=y, control = list(fnscale=-1)))
  return(MLE)
}

  
