############################################################
#####################** Description **######################
# Likelihood
# Plausability of data under a model over a parameter space
############################################################

# Data: mtcars
# Exploring relationship b/w mpg and displacement

# Deterministic component

deterministic_component <- function(x, a, b){
  yexp <- a*exp(b*x)
  return(yexp)
}

# Stochastic component
# Maybe revise this to be separate and 
# make a separate data generating function

stochastic_component <- function(x, variance, add.noise=F, params=NULL){
  sdev <- sqrt(variance)
  stoch.noise <- rnorm(length(x), 0, sdev)
  if (add.noise){
    yexp <- deterministic_component(x, params$a, params$b)
    yvals <- yexp + stoch.noise
    return(yvals)
  } else{
    return(stoch.noise)
  }
  
}

PlausibleData <- function(xvals, variance, params, nreps=1){
  sample.size <- length(xvals)
  results <- array(0, dim=c(sample.size, nreps))
  
  for (i in 1:nreps){
    yvals <- deterministic_component(xvals, params$a, params$b) +
             stochastic_component(xvals, variance)
    results[,i] <- yvals
  }
  return(results)
}

ArrayBoxPlot <- function(arr, xvals,
                         main.text="Plausible data under this model", 
                         ylab="mpg", xlab="Displacement", 
                         bycol=T){
  if (bycol){
    arr <- t(arr)
  }
  boxplot(arr, xaxt="n", at=xvals, boxwex=6, main=main.text, ylab=ylab, xlab=xlab)
  cleanseq <- seq(0, max(round(xvals/100)), length=(max(round(xvals/100))) + 1)*100
  axis(1, at = cleanseq, labels = cleanseq)
}

PlotCompareRealandPlausible <- function(xvals, yreal, variance, params, nreps=1000){
  res <- PlausibleData(xvals, variance, params, nreps=nreps)
  ArrayBoxPlot(res, xvals)
  points(xvals, yreal, pch=20, cex=3, col = "green")
  
}
