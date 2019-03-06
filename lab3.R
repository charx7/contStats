# Clear the workspace
rm(list=ls())
cat("\014") 

library(MASS)
# Set random seed for reproducibility
set.seed(47)

# Parameters for generation of data
N           = 100
beta_0      = 2
beta_1      = 0.5
betas       = c(beta_0, beta_1)
epsilons    = rnorm(N,0,0.5)
X           = rnorm(N,0,1)
ones_vector = seq(1,1,length.out=N)
# Simple linear model
Y = beta_0 + beta_1 * X + epsilons

# Data
data = cbind(ones_vector,X)

############ Bootstrap ###################
# Init the sigmas vector as empty
beta0Vector = numeric()
beta1Vector = numeric()
bootTrials = 1000
for(i in 1:bootTrials) {
  set.seed(47 + i)
  # Sample from your data with replacement
  index       = sample(N, replace = TRUE)
  currSample  = data[index,]
  currSampleY = Y[index]
  
  # Betas Vector 
  currBetasVector = t(ginv(currSample %*% t(currSample)) %*% currSample) %*% currSampleY
  # Compute the sample variances for the current Bootstrap Sample
  #currSigma = (1/(N-2)) * t((currSample %*%  betas) - Y) %*% ((currSample %*% betas) - Y)
  
  # Compute the variance vector from the estimators
  #currBetaSigma =  currSigma[1,1] * solve(t(currSample) %*% currSample)
  
  # append to the sigmas vector
  beta0Vector = append(currBetasVector[1], beta0Vector)
  beta1Vector = append(currBetasVector[2], beta1Vector)
  #beta1_sigmas_vector = append(currBetaSigma[2,2], beta1_sigmas_vector)
}

# Compute a histogram of the test statistic
hist(beta0Vector)
hist(beta1Vector)

# Compute the quantiles
print('############ RESULTS ##########################')
print('The quantiles for the var(b0) and var(b1) are: ')
quantile(beta0Vector, probs = c(0.025, 0.975))
quantile(beta1Vector, probs = c(0.025, 0.975))

var(beta0Vector)
var(beta1Vector)
