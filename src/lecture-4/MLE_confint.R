############################################################
#####################** Description **######################
# MLE confidence intervals
############################################################

source("src/lecture-4/MLE_mtcars_functions.R")

# Data: mtcars
## Exploring relationship b/w mpg and displacement
## Exponential relationship, normally distributed errors
## params : a, b, c


params <- list(a=33, b=-0.002, c=1)

MLE <- optim(fn=LogLikFunction, par = unlist(params), 
             df=mtcars, yvar="mpg", xvar="disp",
             control = list(fnscale=-1))

# Estimating parameter uncertainty
## Likelihood (log) slice
