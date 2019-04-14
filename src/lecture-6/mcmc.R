############################################################
#####################** Description **######################
# Markov chain Monte Carlo (MCMC)

############################################################

# Why MCMC?
# In many cases we just won't have the computational power
# to partition our parameter space into discrete pixels
# and then for each pixel
# : Evaluate likelihood 
# : Multiply by prior to get weighted Likelihood 
#   i.e. get joint prob dens of param vector and data occuring
# : Finally sum the joint prob dens (weighted likelihoods)
#   to get the denominator