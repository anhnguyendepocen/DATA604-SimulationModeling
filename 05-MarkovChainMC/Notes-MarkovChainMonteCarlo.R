
markovChainStep <- function(initialState, tranMatrix, steps)
{
  n <- steps
  step <- initialState
  for(i in 1:n)
  {
    step <- step %*% tranMatrix
  }
  return (step)
}

## Traveling Salesmen Problem (from Lecture Slides)

p <- matrix(c(0, 0.5, 0.5, 0.75, 0, 0.25, 0.75, 0.25, 0), nrow=3, byrow=TRUE)
p

markovChainStep(p, p, 1)

#        [,1]   [,2]   [,3]
#[1,] 0.7500 0.1250 0.1250
#[2,] 0.1875 0.4375 0.3750
#[3,] 0.1875 0.3750 0.4375

# What is the Traveling Salesman's expected long-run average profit?
f <- c(1000, 1200, 1250)
u <- c(1, 0, 0)
n = 100
u %*% markovChainStep(p, p, n) %*% f

#         [,1]
#[1,] 1128.571

# Long run Markov Chain Probabilities Video
# https://youtu.be/IYaOMor9qvE
isv <- c(0.1, 0.9)
tm <- matrix(c(0.6, 0.4, 0.15, 0.85), nrow=2, byrow=TRUE)
isv %*% tm %*% tm
# [,1]    [,2]
# [1,] 0.23775 0.76225


