############################################################
#####################** Description **######################
# Undertstanding likelihood
############################################################

## TODO : FIND A WAY TO UNPACK ARGUMENTS LIKE PYTHON from lists and vectors

source("src/lecture-4/mtcars_plausible.R")

# Data: mtcars
# Exploring relationship b/w mpg and displacement

# Extract a single data point
obs.data <- mtcars[1, c("mpg", "disp")]

x <- obs.data$disp
y <- obs.data$mpg

# Assume a model (exponential model)
# and set some parameters a, b and c
params <- list(a=33, b=-0.002, c=1)

# Expected value
# of that observation (mpg) for the
# given x (displacement), assuming the exponential model
# for the paramaters a and b
attach(params)
expected_val <- deterministic_component(x, a, b)

# Now about that expected_val the observation(mpg)
# is normally distributed with
mu.obs = expected_val
sd.obs = sqrt(c)

# Let's plot out the error distribution 
# and where we actually observed the data point
# given the choice of model (exponential)
# for the parameters a, b and c

range.ofplot <- mu.obs + sd.obs*c(-3, 3) # Just plotting +- 3 stdevs
curve(dnorm(x, mu.obs, sd.obs), range.ofplot[1], range.ofplot[2], 
      xlab = "Plausible values for mpg under the specified model", 
      ylab = "Probability density", main=paste("Displacement =", x))

# Finally plot the actual observed data point
abline(v=y, col="red", lwd=2)

# Add the likelihood value text to the plot
get.txtpos <- function(xrel, yrel){
  pu <- par()$usr
  txt.xpos <- sum(pu[1:2]*c(1-txt.xrel, txt.xrel))
  txt.ypos <- sum(pu[3:4]*c(1-txt.yrel, txt.yrel))
  
  return(c(txt.xpos, txt.ypos))
}

# Calculate the
# Likelihood of a, b, c being equal to params
# Given the model, and the data
# Alternatively calculate 
# the prob (density) of data given the model and a,b,c set to params
# Closer values to the mean have higher prob density (unimodal)
# Think analogous to p-values (??Maybe different analyses??) <== TODO : IMPLEMENT THIS!
likelihood <- dnorm(y, mu.obs, sd.obs)
text(txt.xpos, txt.ypos, paste("Likelihood:", format(likelihood, scientific = T)))


# Now for n data points
n <- 4
obs.data <- mtcars[sample(1:nrow(mtcars), n), c("mpg", "disp")] # Randomly take n obs

plot.config <- list("2"=c(1, 2), "3"=c(1, 3), "4"=c(2, 2)) # Panel plots
par(mfrow=plot.config[[as.character(n)]])

likelihoods <- numeric(nrow(obs.data))
for (i in 1:nrow(obs.data)){
  x <- obs.data$disp[i]
  y <- obs.data$mpg[i]
  expected_val <- deterministic_component(x, a, b)
  
  mu.obs <- expected_val
  sd.obs <- sqrt(c)
  
  range.ofplot <- mu.obs + sd.obs*c(-3, 3) 
  curve(dnorm(x, mu.obs, sd.obs), range.ofplot[1], range.ofplot[2],
        xlab="mpg", y="probability density", main=paste("Displacement =", x))
  abline(v=y, col="red", lwd=2)
  txt.pos <- get.txtpos(0.8, 0.8)
  
  likelihoods[i] <- dnorm(y, expected_val, sqrt(c))
  text(txt.pos[1], txt.pos[2], paste("L:", format(likelihoods[i], scientific = T)))
}

# Since these data points are independent "events"
# The probability (density) of a given set of data points
# is the product of the probabilities (densities) (JOINT)
# Or in other words
# The likelihood of a, b, c set to params
# given the data points ("events") we observed
# and the choice of the model
# Lower the likelihood, lower the probability (density)
# to observe the data points
# So we intend to chose the value of params such that
# the joint probability (density) of observing the data is maximized
# ofcourse, given the choice of the model

# Now for all the data
obs.data <- mtcars
likelihoods <- dnorm(obs.data$mpg, 
                          mean=deterministic_component(obs.data$disp, a, b), 
                          sd=sqrt(c))

likelihood.alldata <- prod(likelihoods) # Likelihoods of params not DATA!
                                        # given we observed all of that data
                                        # is what I mean here
paste("The likelihood of the parameters,", 
      "a =",a, "b =", b, "c =", c, 
      "is", format(likelihood.alldata, scientific = T))

lowertail.area <- pnorm(obs.data$mpg, 
                        mean=deterministic_component(obs.data$disp, a, b), 
                        sd=sqrt(c))
p.vals <- sapply(lowertail.area, function(i) ifelse(i >0.5, (1-i)*2, i*2))

paste(mean(p.vals >= 0.05)*100, "% observations are inside the 95 % confidence region")

# But more common is log-likelihood since
# likelihood values can be very very small
logliks <- dnorm(obs.data$mpg, 
                   mean=deterministic_component(obs.data$disp, a, b), 
                   sd=sqrt(c), log = T)
loglik.alldata <- sum(logliks)
paste("The log likelihood of the parameters,", 
      "a =",a, "b =", b, "c =", c, 
      "is", round(loglik.alldata, 5))

# Note: Actually more common is the negative likelihood
# as functions in R have default option of minimizing.
