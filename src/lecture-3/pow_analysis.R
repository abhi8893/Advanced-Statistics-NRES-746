############################################################
#####################** Description **######################
# An example of power analysis
############################################################


# Function to calculate number of observed/detected animals

NumObserved <- function(TrueN=1000, surveyors=1, days=3, probPerPersonDay=0.02){
  
  probPerDay <- 1- (1-probPerPersonDay)^surveyors
  probPerSurvey <- 1- (1-probPerDay)^days
  
  nobs <- rbinom(1, size=TrueN, prob = probPerSurvey)
  return(nobs)
}

# Function to calculate current year abundance assuming a linear trend

ThisYearAbund <- function(LastYearAbund=1000, trend=-0.03){
  CurAbund <- floor(LastYearAbund*(1+trend))
  return(CurAbund)
}

# Simulate observed/detected animal data

SimulateMonitoringData <- function(initabund=1000, trend=-0.03, years=25, surveyors=1, 
                                   days=3, survint=2, probPerPersonDay=0.02){
  prevabund <- initabund
  detected <- numeric(years)
  
  for (i in 1:years){
    thisabund <- ThisYearAbund(prevabund, trend)
    nobs <- NumObserved(thisabund, surveyors, days, probPerPersonDay)
    detected[i] <- nobs
    
    prevabund <- thisabund
  }
  
  surveyed <- c(1:years)%%survint == 0
  detected[!surveyed] <- NA
  
  return(detected)
  
}

# Function to check if the detected decline (if at all) was significant or not

IsDecline <- function(monitoringData, alpha = 0.05){
  time <- 1:length(monitoringData)
  lm.fit <- lm(monitoringData~time)
  
  p.val <- summary(lm.fit)$coefficients["time", "Pr(>|t|)"]
  trend.estimate <- summary(lm.fit)$coefficients["time", "Estimate"]
  isdecline <- ifelse(trend.estimate < 0, TRUE, FALSE)
  sig_decline <- ifelse((p.val <= 0.05) & (isdecline), TRUE, FALSE)
  
  return(sig_decline)
}

# Function to get Power of the test
# TODO : Make a list/function for argument names 
#        specific to each funcion, and parse them 
#        from the function call of GetPower
## The below function is WRONG!!

GetPower <- function(initabund=1000, trend=-0.03, years=10, 
                     surveyors=1, days=7, survint=2, alpha=0.05, ProbPerPersonDay=0.02, 
                     nreps=10^4){
  
  sigtrend.detected <- numeric(nreps)
  
  for (i in 1:nreps){
    monitoringData <- SimulateMonitoringData(initabund, trend, years, surveyors, 
                                             days, survint, ProbPerPersonDay)
    
    sigtrend.detected[i] <- IsDecline(monitoringData, alpha)
    
  }
  return(mean(sigtrend.detected))
}
