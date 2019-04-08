############################################################
#####################** Description **######################
# Analysis using the result of bootstrapped samples
############################################################

get.pval <- function(x){
  mu.x  <- mean(x)
  n.x <- length(x)
  x <- x - mu.x
  p.val <- length(which(abs(x) >= abs(mu.x)))/ n.x
  
}
