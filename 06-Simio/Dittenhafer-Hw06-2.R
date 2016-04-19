RNGkind("Mersenne-Twister")
set.seed(020275)
maxCustomers <- 1000
iaRate <- 1/10
svcTimeRate <- 1/7

# Create a data frame of the new customers and their jobs
newJobs <- data.frame(customer=seq(1, maxCustomers), 
                      iaMins=rexp(maxCustomers, rate=iaRate), 
                      arrivalMins=rep(0, maxCustomers),
                      svcTimeMins=rexp(maxCustomers, rate=svcTimeRate),
                      timeSvcBegin=rep(0, maxCustomers),
                      queueWaitMins=rep(0, maxCustomers),
                      timeSvcEnd=rep(0, maxCustomers),
                      timeInSystem=rep(0, maxCustomers),
                      svrIdleTime=rep(0,maxCustomers),
                      custInQueue=rep(0,maxCustomers))
# Determine overall arrival times
newJobs$arrivalMins <- cumsum(newJobs$iaMins) 
# Join the existing and new jobs into one table
simTable <- newJobs
# Loop over the rows the compute the various activity and clock times
for(i in seq(1, nrow(simTable)))
{
  if(i == 1)
  {
    # Special handling for first row
    simTable[i,]$timeSvcBegin <- simTable[i,]$arrivalMins  
    simTable[i,]$svrIdleTime <- simTable[i,]$arrivalMins
  }
  else
  {
    # Initialize for > first customer
    simTable[i,]$timeSvcBegin <- max(simTable[i,]$arrivalMins, simTable[i-1,]$timeSvcEnd)  
    simTable[i,]$svrIdleTime <- ifelse(simTable[i,]$arrivalMins == simTable[i,]$timeSvcBegin, 
                                       simTable[i,]$timeSvcBegin - simTable[i-1,]$timeSvcEnd, 0)
  }
  
  simTable[i,]$queueWaitMins <- simTable[i,]$timeSvcBegin - simTable[i,]$arrivalMins
  simTable[i,]$timeSvcEnd <- simTable[i,]$timeSvcBegin + simTable[i,]$svcTimeMins
  simTable[i,]$timeInSystem <- simTable[i,]$timeSvcEnd - simTable[i,]$arrivalMins
}

# Show the results table
summary(simTable)

totalMin <- max(simTable$arrivalMins)
totalMin
svrIdleMin <- sum(simTable$svrIdleTime)
svrIdleMin
sysUtilRate <- (totalMin - svrIdleMin) / totalMin
sysUtilRate
expectedTimeInSystem <- mean(simTable$timeInSystem)
expectedTimeInSystem
#
# Analytic Solution
#
# System Utilization Rate
rho <- iaRate / svcTimeRate
rho
# Variance
sigma2 <- 1 / svcTimeRate^2
sigma2
# Expected System Time
w <- (1/svcTimeRate) + (iaRate * (sigma2 + sigma2)) / (2 * (1 - rho))
w
# Expected number of customers in the queue
Lq <- (rho^2 *  (1 + sigma2 * svcTimeRate^2)) / (2 * (1 - rho))
Lq
                        
                        
                        
                        
                        