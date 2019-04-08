############################################################
#####################** Description **######################
# Linear regression R-squared and coefficients 
# Bootstrapped confidence interval
############################################################


# Function to calculate statistics on the bootsample
# Returns an output df with columns: 
# rsquared: Rsquared value (explained variance)
# estimate: coefficient value corresponding to covariate chosen
# coeff: cofficient name

statfunc <- function(df, responsevar="Volume"){
  response <- df[, responsevar]
  names <- names(df)
  names <- names[names != responsevar]
  stat.df <- data.frame(matrix(NA, ncol = 3, nrow = 2))
  
  i = 1
  for (pred.name in names){
    predictor <- df[, pred.name]
    model.sum <- summary(lm(response~predictor))
    rsq <- model.sum$r.square
    estimate <- model.sum$coefficients["predictor", ]["Estimate"]
    stat.df[i, ] <- list(rsq, estimate, pred.name)
    
    i = i + 1
  }
  colnames(stat.df) <- c("rsquared", "estimate", "coeff")
  return(stat.df)
}

# Function to bootstrap samples and apply a statfunc function
# returns an output df equivalent to statfunc but for n.samples

boot_sample <- function(df, statfunc=statfunc, n.samples=1000, n.stats=3, responsevar="Volume"){
  
  indices <- 1:nrow(df)
  output <- data.frame(matrix(NA, nrow=n.samples*2, ncol=n.stats))
  
  for (i in 1:n.samples){
    boot_rows <- sample(indices, length(indices), replace = T)
    new_df <- df[boot_rows, ]
    stat.df <- statfunc(new_df, responsevar)
    output[(2*i-1):(2*i), ] <- stat.df
  }
  colnames(output) <- colnames(stat.df)
  return(output)
}
