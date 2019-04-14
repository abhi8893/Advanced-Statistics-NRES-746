############################################################
#####################** Description **######################
# Intuition behind Bayesian inference
############################################################

# TIP: I think the gap between the classical Bayesian thinking
#      considering the problem of +ve/-ve Tests with D/ND Truths
#      and Bayesian inference is causing the problem in the intuition.

# Intuition: Consider, D = Disease, + = positive test result
#            Analogously consider C = condition = parameter, + = Data
#            This condition C could be either D or ND
#            i.e. the param space of C is {D, ND}
#            if we many types of diseases, C = {D1, D2, .....Dn,ND}
#            We could consider a continous parameter theta like this
#            We need to find the probability that the patient actually has
#            the disease given that the test result came out to be positive
#            i.e. P(D|+)
#            We start by finding the number of cases 
#            where the test results were 
#            OR the % of cases where the test results were +
#            OR the prob of being tested +
#            OR the unconditional prob of being tested positive
#               i.e. P(+) = P(+, D) + P(+, ND) -- eq 1
#               i.e. P(+) = P(D)P(+|D) + P(ND)P(+|ND) -- eq 2
#               i.e. P(+) = P(D)L(D|+) + P(ND)L(ND|+) -- eq 3
#               Now, consider this as, we could be tested in two ways
#               Either we have the Disease and test positive,
#               OR we don't have the Disease and test positive -- ref eq 1
#               OR in more general case, considering C as the parameter
#               we are summing the weighted likelihoods of C over the
#               param space to give us the Probability of data -- ref eq3

# theta : parameter : {theta_1, theta_2.....} [continous-infinite]
# A specific value of theta_1 and the data occured 
# with a prob dens P(theta_1, data)
# But the data can occur in the following different ways:
# Either we have theta_1 and observe data
# OR     we have theta_2 and observe data
# OR     we have theta_3 and observe data
#        .... and so on over all the param space
# So this gives us the P(theta_1 | data) as 
# the number of cases or % cases or prob dens
# where theta_1 and data occured jointly (both occured)
# out of the number of cases or % cases or prob dens where the data occured

# But how can we observe both theta_1 and data
# First we observe theta_1 with a prob P(theta_1) ==> Prior
# then having observed theta_1 we observe the data 
# i.e. P(data|theta_1) = L(theta_1|data) ==> Likelihood
# i.e. or can we viewed as the joint probability of observing
#      all the data points in the data for a specific value of theta (theta_1)