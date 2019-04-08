############################################################
#####################** Description **######################
# Two sample t-test function with two options:
# Equal variance and unequal variance.
############################################################
 
# How to use column names without strings to a function
# Best practice : Should I use data or df arg?

test.func <- function(a, b, data=NULL){
  a <- eval(substitute(a), data, parent.frame())
  b <- eval(substitute(b), data, parent.frame())
  cat(a) 
  cat(b)
}

# How to create a named list from variable names

named.list <- function(...){
  l <- setNames(list(...), as.character(match.call()[-1]))
  return(l)
}
# Create a separate function for
# Variance of the difference in means and deg of freedom

get.var_and_df <- function(a, b = NULL, var.equal = FALSE){
  var.a <- var(a)
  n.a <- length(a)
  
  if (is.null(b)){
    return(list(var.meandist=var.a/n.a, df.tdist=n.a-1))
  }else {
    
    var.b <- var(b)
    n.b <- length(b)
    
    if (var.equal){
      df.tdist <- n.a + n.b - 2
      var.pop <- (var.a*(n.a -1) + var.b*(n.b - 1))/(df.tdist)
      var.diffmean <- (var.pop)*(1/n.a + 1/n.b) 
    } else {
      var.diffmean <- var.a/n.a + var.b/n.b
      df.tdist <- (var.diffmean^2)/(((var.a/n.a)^2)/(n.a - 1) + ((var.b/n.b)^2)/(n.b - 1))
    }
  
  return(list(var.meandist=var.diffmean, df.tdist=df.tdist))
  }
}

# How to match arguments partly

test.match <- function(alternative=c("two.sided", "greater", "less")){
  alternative <- match.arg(alternative)
  return(alternative)
}

# Function to get variable names from the arguments passed

test.argname <- function(a, b){
  l <- as.list(match.call()[-1])
  l
}

# get.t.stat <- function(x, x.mu, x.var){
#   return((x - x.mu)/(sqrt(x.var)))
# }

# Function to pretty cat the output of t-test

pprint.ttest <- function(ttest.result){
  for (n in names(ttest.result)){
    assign(n, ttest.result[[n]])
  }
  
  hyp.stat <- paste("true", test.for, "is", type.hyptest, mu)
  v <- c(mu.a, mu.b)
  names(v) <- paste("mean of", c(a.name, b.name))

  cat(method, fill = T)
  
  cat(paste("data:", c(a.name), "and", c(b.name)), fill=T)
  
  cat(paste(c("t", "df", "p-value"), "=", 
              paste0(round(c(t.stat, df.tdist, p.val), 4), ",")), fill = T)
  cat(
  paste("alternative hypothesis:", hyp.stat),
  paste(conf.level*100, "percent confidence interval:"), 
  sep="\n", fill = T)
  
  cat(cint, fill = T)
  cat("sample estimates:", fill = T)
  print(v)
}


# t test function

calc.t.test <- function(a, b = NULL,
                        alternative = c("two.sided", "less", "greater"),
                        mu = 0, var.equal = FALSE, 
                        conf.level = 0.95){
  
  a <- a[!is.na(a)]
  b <- b[!is.na(b)]
  
  options(warn = -1)
  arg.call <- as.list(match.call()[-1])
  
  for (var.name in c("a", "b")){
    assign(paste0(var.name, ".name"), arg.call[[var.name]])
  }
  
  mu.a <- mean(a)
  
  if (is.null(b)){
    obs.diff <- mu.a  - mu
    n.sample = "One"
    test.for <- "mean"
  } else {
    mu.b <- mean(b)
    obs.diff <- (mu.a -  mu.b) - mu
    n.sample = "Two"
    test.for <- "difference in means"
  }
  
  method <- paste(if (!var.equal) "Welch", n.sample, "Sample t-test")
  
  
  l <- get.var_and_df(a, b, var.equal=var.equal)
  for(stat.name in names(l)){
    assign(stat.name, l[[stat.name]])
  }
  
  t.stat <- obs.diff/sqrt(var.meandist)
  
  alternative <- match.arg(alternative)
  
  if (alternative == "two.sided"){
    type.hyptest <- "not equal to"
    p.val <- pt(abs(t.stat), df = df.tdist, lower.tail = FALSE)*2
    alpha <- 1- conf.level
    critical.vals <- qt(1 - alpha/2, df.tdist)
    cint <- c(-critical.vals, critical.vals)
    # cint <- t.stat + c(-critical.vals, critical.vals)
  } else {
    type.hyptest <- paste(alternative, "than")
    
    if (alternative == "less"){
      p.val <- pt(t.stat, df=df.tdist, lower.tail = TRUE)
      cint <- c(-Inf, qt(conf.level, df.tdist))
      # cint <- c(-Inf, t.stat + qt(conf.level, df.tdist))
      
    } else if (alternative == "greater" ){
      p.val <- pt(t.stat, df=df.tdist, lower.tail = TRUE)
      cint <- c(qt(1-conf.level, df.tdist), Inf)
      # cint <- c(t.stat - qt(conf.level, df.tdist), Inf)
    }
  }
    
  cint <- obs.diff + mu + cint*sqrt(var.meandist)
  # cint <- mu + cint*(sqrt(var.meandist))
  
  ttest.result <- named.list(
    test.for,
    type.hyptest,
    mu,
    method,
    a.name,
    b.name,
    t.stat,
    df.tdist,
    p.val,
    conf.level,
    cint,
    mu.a,
    mu.b
  )
  
  pprint.ttest(ttest.result)
}

