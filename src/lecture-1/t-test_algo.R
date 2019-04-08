############################################################
#####################** Description **######################
# t-test ALGORITHMIC APPROACH
############################################################

lots <- 10^6

# Parametric Bootstrap
## 1.) Both samples are drawn from the same population - 
## 2.) Samples are drawn from populations with same mean, but different variance
## 3.) Samples are drawn from populations with diff mean, and different variance


t.test.bootp <- function(a, b, mean.equal=T, var.equal=T, ci.type="percentile", reps=10^6){
  
  a <- a[!is.na(a)]
  b <- b[!is.na(b)]

  mu.a <- mean(a)
  sd.a <- sd(a)
  n.a <- length(a)
  
  mu.b <- mean(b)
  sd.b <- sd(b)
  n.b <- length(b)
  
  obs.diff <- (mu.a - mu.b)

  df <- data.frame(group = rep(c("a", "b"), each = c(n.a, n.b)),
                   value = c(a, b))
  
  null_difs <- numeric(reps)
  
  if (mean.equal){
    mu.a = mu.b = mean(df[,"value"])
  }
  if (var.equal){
    # df.tdist <- n.a + n.b - 2
    # var.a <- sd.a^2
    # var.b <- sd.b^2
    # 
    # sd.a = sd.b = sqrt((var.a*(n.a -1) + var.b*(n.b - 1))/(df.tdist))
    sd.a = sd.b = sd(df[, "value"])
  }
  
  for (i in 1:reps){
    sample.a <- rnorm(n.a, mean = mu.a, sd = sd.a)
    sample.b <- rnorm(n.b, mean = mu.b, sd = sd.b)
    null_difs[i] <- mean(sample.a) - mean(sample.b)
  }
  
  if (!all(c(mean.equal, var.equal))){
    null_difs <- null_difs - obs.diff
  }
  
  p.val <- length(which(abs(null_difs) >= abs(obs.diff)))/ reps
  
  # TODO: ADD match.arg for ci.type
  
  if (ci.type == "percentile"){
    ci <- quantile(null_difs, c(0.025, 0.975)) + obs.diff
  }
  res <- named.list(null_difs, p.val, ci)
  
  return(res)
}


# Non Parametric bootstrap


t.test.bootnp <- function(a, b, same.pop = T, ci.type="percentile", permut = F, reps=10^6){
  
  
  a <- a[!is.na(a)]
  b <- b[!is.na(b)]
  
  n.a <- length(a)
  n.b <- length(b)
  
  df <- data.frame(group = rep(c("a", "b"), times = c(n.a, n.b)),
                   value = c(a, b))
  
  if (same.pop){
    a.pop = b.pop = c(a, b)
  } else {
    a.pop <- a
    b.pop <- b
  }
  
  null_difs <- numeric(reps)
  
  if (!permut){
    for (i in 1:reps){
      sample.a <- sample(a.pop, size = n.a, replace = T)
      sample.b <- sample(b.pop, size = n.b, replace = T)
      null_difs[i] <- mean(sample.a) - mean(sample.b)
      
    }} else if (same.pop){
      
      for (i in 1:reps){

        boot.grp <- sample(df$group)
        sample.a <- df$value[boot.grp == "a"]
        sample.b <- df$value[boot.grp == "b"]
        null_difs[i] <- mean(sample.a) - mean(sample.b)
      } 
    }
    
    obs.diff <- mean(a) - mean(b)
    
    if (!same.pop){
      null_difs <- null_difs - obs.diff
    }
    
    p.val <- length(which(abs(null_difs) >= abs(obs.diff)))/ reps
    
    if (ci.type == "percentile"){
      ci <- quantile(null_difs, c(0.025, 0.975)) + obs.diff
    }
    res <- named.list(null_difs, p.val, ci)  
    
}



