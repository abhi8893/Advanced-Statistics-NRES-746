############################################################
#####################** Description **######################
# MLE confidence intervals
# Log Likelihood slice
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
## Holding params a and c constant at MLE
## See the likelihood slice of b
## TODO: Find a way use sapply in this scenario

upperval <- -1/550
lowerval <- -1/350
allvals <- seq(lowerval, upperval, length=1000)
newParams <- MLE$par



loglik_slice <- numeric(length(allvals))

for (i in 1:length(allvals)){
  newParams["b"] <- allvals[i]
  loglik_slice[i] <- LogLikFunction(newParams, mtcars, "mpg", "disp")
}

# Plot the log-likelihood of b for all different values of b
plot(loglik_slice~allvals, type="l", main="Log Likelihood slice", 
     xlab="Parameter slice for \'b\'", ylab="Likelihood")

# Indicate the MLE of b
abline(v=MLE$par["b"], col="blue", lwd=3)

# Since MLE is an estimate, it has certain variance associated with it
# If we sample our data many many times, MLE of each simulated data would be different
# Plus each time producing a different LogLik at MLE
# Turns out Deviance follows a chi-sq distribution with 3 - 2 = 1 df
# Here D = -2(logLik - logLik_MLE) ~ chi-sq(df=1)
# 95% confidence interval for D
# D <= chi-sq_critical_0.95
# => logLik > logLik_MLE - chi-sq_critical_0.95/2
critical_val <- qchisq(0.95, 1)/2 # 1.92 (THE RULE OF 2)
# 95 % our LogLik at MLE (for sim data) will be > Original_loglik_MLE - 2 #(or 1.92)
# That means our MLE estimates will vary accordingly
## I AM NOT ENTIRELY CLEAR!
abline(h=(MLE$value - 2), lty=2)

reasonable_parameter_values <- allvals[loglik_slice >=(MLE$value -2)]
paste("95% CI for MLE of parameter b is:")
ci <- range(reasonable_parameter_values)
names(ci) <- paste("95%", c("lower", "upper"), "CI")
ci

# Plot the ci
for (ci.val in ci){
  abline(v=ci.val, lwd=1, col="blue")
}