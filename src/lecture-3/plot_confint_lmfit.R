############################################################
#####################** Description **######################
# Analaysis on linear regression
############################################################

source("src/lecture-3/data_sim_lm.R")

# Simulate some data
xvals <- 1:10
expected_vals <- deterministic_component(xvals, 1, 2)
sim_vals <- stochastic_component(xvals, variance = 5, add.noise = T)

# Fit a linear model
lm.fit <- lm(sim_vals~xvals)

arg.v <- c(range(xvals), 0.05)
names(arg.v) <- c("from", "to", "by")
new.xvals <- do.call(seq, as.list(arg.v))

conf_interval <- predict(lm.fit, newdata=data.frame(xvals=new.xvals), interval="confidence", level=0.95)

# Plot
plot(xvals, sim_vals)
abline(lm.fit)
matlines(new.xvals, conf_interval[, 2:3], col='blue', lty=2)
