# Clear the workspace
rm(list=ls())
cat("\014") 

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
beta0_sigmas_vector = numeric()
beta1_sigmas_vector = numeric()
bootTrials = 1000
for(i in 1:bootTrials) {
  set.seed(47 + i)
  # Sample from your data with replacement
  currSample = data[sample(N, replace = TRUE),]
  
  # Compute the sample variances for the current Bootstrap Sample
  currSigma = (1/(N-2)) * t((currSample %*%  betas) - Y) %*% ((currSample %*% betas) - Y)
  
  # Compute the variance vector from the estimators
  currBetaSigma =  currSigma[1,1] * solve(t(currSample) %*% currSample)
  
  # append to the sigmas vector
  beta0_sigmas_vector = append(currBetaSigma[1,1], beta0_sigmas_vector)
  beta1_sigmas_vector = append(currBetaSigma[2,2], beta1_sigmas_vector)
}

# Compute a histogram of the test statistic
hist(beta0_sigmas_vector)
hist(beta1_sigmas_vector)
# Compute the quantiles
print('############ RESULTS ##########################')
print('The quantiles for the var(b0) and var(b1) are: ')
quantile(beta0_sigmas_vector, probs = c(0.025, 0.975))
quantile(beta1_sigmas_vector, probs = c(0.025, 0.975))
