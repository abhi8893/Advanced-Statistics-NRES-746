############################################################
#####################** Description **######################
# Analysis using the result of bootstrapped samples
# Suited to adapt output of functions in bootstrap_lmfit.R
############################################################

get.pval <- function(x){
  mu.x  <- mean(x)
  n.x <- length(x)
  x <- x - mu.x
  p.val <- length(which(abs(x) >= abs(mu.x)))/ n.x
  
}
