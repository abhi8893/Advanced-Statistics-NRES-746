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

stochastic_component <- function(x, variance){
  sdev <- sqrt(variance)
  stoch.noise <- rnorm(length(x), 0, sdev)
  return(stoch.noise)
  }


# Total component
# Maybe this function is redundant 
# as PlausibleData with reps=1 is essentially the same

generate_data <- function(x, params){
  for (var.name in names(params)){
    assign(var.name, params[[var.name]])
  }
  y_exp <- deterministic_component(x, a, b)
  y_noise <- stochastic_component(x, variance = c)
  
  y_sim <- y_exp + y_noise
  names(y_sim) <- xvals
  return(y_sim)
}

PlausibleData <- function(x, params, nreps=10^3){
  sample.size <- length(x)
  results <- array(0, dim=c(sample.size, nreps))
  
  for (i in 1:nreps){
    yvals <- generate_data(x, params)
    results[,i] <- yvals
  }
  return(results)
}

ArrayBoxPlot <- function(arr, x,
                         main.text="Plausible data under this model", 
                         ylab="mpg", xlab="Displacement", 
                         bycol=T){
  if (bycol){
    arr <- t(arr)
  }
  boxplot(arr, xaxt="n", at=x, boxwex=6, main=main.text, ylab=ylab, xlab=xlab)
  cleanseq <- seq(0, max(round(x/100)), length=(max(round(x/100))) + 1)*100
  axis(1, at = cleanseq, labels = cleanseq)
}

PlotCompareRealandPlausible <- function(x, yreal, params, nreps=10^3, main.text="Plausible data under this model"){
  res <- PlausibleData(x, params, nreps=nreps)
  ArrayBoxPlot(res, x, main.text = main.text)
  points(x, yreal, pch=20, cex=3, col = "green")
  
}
