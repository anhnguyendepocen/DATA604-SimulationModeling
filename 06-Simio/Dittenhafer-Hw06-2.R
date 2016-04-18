RNGkind("Mersenne-Twister")
set.seed(020275)
maxCustomers <- 60

# Create a data frame of the new customers and their jobs
newJobs <- data.frame(customer=seq(1, maxCustomers), 
                      iaMins=rexp(maxCustomers, rate=1/10), 
                      arrivalMins=rep(0, maxCustomers),
                      svcTimeMins=rexp(maxCustomers, rate=1/7),
                      timeSvcBegin=rep(0, maxCustomers),
                      queueWaitMins=rep(0, maxCustomers),
                      timeSvcEnd=rep(0, maxCustomers),
                      timeInSystem=rep(0, maxCustomers),
                      svrIdleTime=rep(0,maxCustomers))
# Determine overall arrival times
newJobs$arrivalMins <- cumsum(newJobs$iaMins) 
# Join the existing and new jobs into one table
simTable <- newJobs
# Loop over the rows the compute the various activity and clock times
for(i in seq(1, nrow(simTable)))
{
  if(i == 1)
  {
    simTable[i,]$timeSvcBegin <- simTable[i,]$arrivalMins  
    simTable[i,]$svrIdleTime <- simTable[i,]$arrivalMins
  }
  else
  {
    simTable[i,]$timeSvcBegin <- max(simTable[i,]$arrivalMins, simTable[i-1,]$timeSvcEnd)  
    simTable[i,]$svrIdleTime <- ifelse(simTable[i,]$arrivalMins == simTable[i,]$timeSvcBegin, 
                                       simTable[i,]$timeSvcBegin - simTable[i-1,]$timeSvcEnd, 0)
  }
  
  simTable[i,]$queueWaitMins <- simTable[i,]$timeSvcBegin - simTable[i,]$arrivalMins
  simTable[i,]$timeSvcEnd <- simTable[i,]$timeSvcBegin + simTable[i,]$svcTimeMins
  simTable[i,]$timeInSystem <- simTable[i,]$timeSvcEnd - simTable[i,]$arrivalMins
}

# Show the table
simTable

totalMin <- max(simTable$arrivalMins)
totalMin
svrIdleMin <- sum(simTable$svrIdleTime)
svrIdleMin
sysUtilRate <- (totalMin - svrIdleMin) / totalMin
sysUtilRate
expectedTimeInSystem <- mean(simTable$timeInSystem)
expectedTimeInSystem