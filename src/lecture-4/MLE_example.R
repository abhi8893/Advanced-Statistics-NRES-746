############################################################
#####################** Description **######################
# Maximum Likelihood Estimation
## Optimize the log-likelihood function
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

# Choose initial values of params for the optimization algorithm
# Eyeball it using the plot (a bit hit and trial)
# Sometimes plotting only the deterministic component can give a good idea
# But choose reasonable starting vals
params = list(a=33, b=-0.002, c=1)
PlotCompareRealandPlausible(mtcars[, "disp"], mtcars[, "mpg"], params)

# Okay! Above values seem nice
## Couple (or more) of things to note!

## 1.) params must always be a vector so make your function accordingly
##     i.e. no separate args for a, b, c
##
## 2.) Always name your param vector
##
## 3.) Default is to minimize so specify control=list(fnscale=-1)

MLE <- optim(fn=LogLikFunction, par=unlist(params), 
             df=mtcars, yvar="mpg", xvar="disp", control=list(fnscale=-1))

paste("Parameter values:")
MLE$par
paste("Log-Likelihood of the params at MLE given the choice of the model and data we observed")
MLE$value

# Now see the fit of the data at MLE
bestParams <- MLE$par
PlotCompareRealandPlausible(mtcars[, "disp"], mtcars[, "mpg"], bestParams,
                            main.text = "Model with MLE parameters")


