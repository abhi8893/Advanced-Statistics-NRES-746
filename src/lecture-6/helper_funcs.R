############################################################
#####################** Description **######################
# Helper functions 
############################################################


# Return prob density based on param value and name
# May have to use approxfun or something in the function
p.param <- function(x, param){
  
  pvec <- paramvec[[param]]
  if (x < min(pvec) | x > max(pvec)){
    return(0)
  }
  ndx <- paramvec[[param]] == x
  prob.dens <- ifelse(any(ndx),
                      marginal.dist[[param]][ndx]/paramwidth[[param]],
                      0)
  return(prob.dens)
}


