############################################################
#####################** Description **######################
# Bayesian posterior calculation and plotting for
# Binomially distributed data
############################################################

# TODO: Separate out analyses and plotting routines
# TODO: Derive labels on the 4th axis for likelihood
# TODO: Return scaleby etc so that plot comparison is easy
# TODO: Make a plot comparison routine based on : priors, moredata etc
# TODO: Find a way to convey that Likelihood of any one param decreases
#       as we collect more and more data.
binomdata.bayesplot <- function(data, N, ylim=NULL, prior=NULL,tiny=1e-3){


  if (is.null(prior)){
    param.space <- seq(0, 1, by=tiny)
    prior <- dbeta(param.space, 1, 1)
  } else {
    tiny <- 1/(length(prior) - 1)
    param.space <- seq(0, 1, by=tiny)
  }

  likelihood <- sapply(param.space, function(p) prod(dbinom(data, N, p)))
  
  weighted.likelihood <- prior*likelihood
  normalization.constant <- sum(weighted.likelihood)
  
  posterior <- weighted.likelihood/normalization.constant
  posterior.dens <- posterior/tiny
  
  par(mai=c(1, 1, 0, 1))
  
  if (is.null(ylim)){
    ylim <- c(0, as.integer(max(posterior.dens)*2))
  }
  
  
  # prior
  plot(param.space, prior, ylim=ylim, type='l', col='blue', 
       lwd=1, lty=2, ylab="Probability density", xlab="param.space")
  # Likelihood
  scaleby <- as.integer((max(posterior.dens)/max(likelihood))/2)
  points(param.space, likelihood*scaleby, type="l", col="red", lwd=1, lty=2)
  axis(4, at=seq(0, 2, by=0.4), labels=seq(0, 2, by=0.4)/scaleby)
  mtext("Likelihood", side=4, col="red", line=3)
  # Posterior (density)
  points(param.space, posterior.dens, col="blue", lwd=2, lty=1, type="l")
  
  # Add a legend
  legend("topright",
         legend = c("prior", "likelihood", "posterior"),
         col = c("blue", "red", "blue"),
         lty = c(2, 2, 1),
         lwd = c(1, 1, 2),
         pt.cex = 2,
         cex = 1,
         text.col = "black",
         horiz = F,
         inset = c(0.02, 0.02))
  
}
