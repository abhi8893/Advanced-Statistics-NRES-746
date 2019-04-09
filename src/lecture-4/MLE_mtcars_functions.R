############################################################
#####################** Description **######################
# Functions to use with mtcars data MLE example
############################################################

source("src/lecture-4/mtcars_plausible.R")

# Data: mtcars
## Exploring relationship b/w mpg and displacement
## Exponential relationship, normally distributed errors
## params : a, b, c

LogLikFunction <- function(params, df, yvar, xvar){
  LogLikelihood <- sum(dnorm(df[,yvar], 
                             mean=deterministic_component(df[, xvar], params['a'], params['b']),
                             sd=sqrt(params['c']), log=TRUE))
  return(LogLikelihood)
}
