# Clear the workspace
rm(list=ls())
cat("\014") 

# Load packages
library(gtools)

# Set random seed for reproducibility
set.seed(47)

# Permutations test Null hypothesis each permutation is equially likely, that would imply 
# independance, so we permute a lot of times and calculate the statistic

# Number of generated points
N=10
M=10

# take samples from the gaussians N(0,1), N(1,1) to generate data
# Generate N and M data points
X1 = rnorm(N, 0, 1)
X2 = rnorm(M, 1, 1)

# Compute the mean of X1 and X2
meanX1 = mean(X1)
meanX2 = mean(X2)

# Calculate original t-test
orig_t_test = t.test(X1, X2)$statistic

# Combine X1 and X2 to calculate Z(N+M)
Z_NM = c(X1, X2)

t_test_vector = numeric()
for (i in 1:1000){
  #set.seed(i)
  
  # Permute Z_NM
  perm_Z_NM = permute(Z_NM)
  
  # Split the permuted sample
  X1_perm = perm_Z_NM[1:10]
  X2_perm = perm_Z_NM[11:20]
  
  # Calculate the new means and difference of the permuted sample
  meanX1_perm    = mean(X1_perm)
  meanX2_perm    = mean(X2_perm)
  current_t_test = t.test(X1_perm, X2_perm, alternative = "two.sided")$statistic
  t_test_vector  = append(t_test_vector, current_t_test)
}

# Compute a histogram of the test statistic
hist(t_test_vector)

# Compute the quantiles
quantile(t_test_vector, probs = c(0.025, 0.975))
orig_t_test