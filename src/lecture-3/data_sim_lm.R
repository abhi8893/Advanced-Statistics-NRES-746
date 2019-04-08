############################################################
#####################** Description **######################
# Linear regression data generation simulation
############################################################

# Deterministic componenent
## Assuming linear relationship with x

deterministic_component <- function(x, a, b, trans.func=NULL){
  if (!is.null(trans.func)){
    x <- trans.func(x)
  }
  return(a + b*x)
}

# Stochastic component
## Assuming the noise if normally distributed around the expected value

stochastic_component <- function(x, variance, add.noise=T, params=NULL){
  sdev <- sqrt(variance)
  stoch.noise <- rnorm(length(x), 0, sdev)
  if (add.noise){
    return(deterministic_component(x, params$a, params$b) + stoch.noise)
  } else
    return(stoch.noise)
}



