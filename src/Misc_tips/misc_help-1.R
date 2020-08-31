############################################################
#####################** Description **######################
# Miscellaneous helper functions
############################################################

my.source <- function(env){
  f <- rstudioapi::getSourceEditorContext()$path
  rm(list = ls(envir = env), envir = env)
  source(f, local=env)
}

# How to do recursion in R +
# repeat a function 1000 times

f <- function(...){
  rand.n <- runif(1)
  if (rand.n < 0.5){
    rand.n <- f()
  }
  rand.n
}

vals <- unlist(lapply(1:1e3, f))
if(all(vals > 0.5)) {
  hist(vals, xlim = c(0, 1))
  abline(v=0.5, col='red', lwd=3, lty=2)
  axis(1, at=0.5, labels = 0.5)
}
# Make sure they are all greater than 0.5