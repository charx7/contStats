# Clear the Environment Variables
rm(list = ls())

# Set random seed for reproducibility
set.seed(47)

# Frequencies vector
# Note that the 0 frequency is just a place-holder we will use on the EM-algo
freqs = c(99,46,87,53,40,9,2)
names(freqs) = c(
  "0-positive-tests", 
  "1-positive-tests", 
  "2-positive-tests",
  "3-positive-tests",
  "4-positive-tests",
  "5-positive-tests",
  "6-positive-tests"
)

# Positively test probands value
pos_test_probands = 237

# MLE calculation function
get_mle = function(freqs, current_pi) {
  # Get the MLE estimator for pi
  # Loop over the freqs
  num       = 0
  denom     = 0
  for (i in 0:(length(freqs)-1)){
    num       = num + i*freqs[1+i]
    denom     = denom + freqs[1+i] * 6
  }
  
  pi_mle = num / denom
  return(pi_mle)
}

# Expectation function calculation
get_expectation = function(pi) {
  # Caculate the probability (gamma) that X_{j} = 0
  gamma = dbinom(0, size=6, prob=pi) 
  # Calculate the compliment of gamma
  gamma_comp = 1 - gamma
  # Now we calculate the Expected value of the Neg Binomial for the 237 diseased probands
  neg_bin_expectation = pos_test_probands / gamma_comp
  # Now we use that nunmber to calculate the frequency of the 0-positive test that will be plugged to get a ML for pi
  zero_freq_count = neg_bin_expectation - pos_test_probands
  return(zero_freq_count)
}

############### Initialization of the EM ##############
# Give an initial (educated guess) value of pi 
current_pi = 0.24

############### EM algo ###############################
current_pi_mle = 0
epsilon        = 0.01
iteration      = 0
maxIters       = 20
for (i in 1:maxIters) {
  iteration  = iteration + 1
  # Get the expectation 
  exp_zero_freq_count = get_expectation(current_pi)
  # Insert that value into the freqs vector 
  freqs["0-positive-tests"] = exp_zero_freq_count
  
  # Get mle
  current_pi_mle = get_mle(freqs, current_pi)
  diff = current_pi_mle - current_pi
  current_pi = current_pi_mle
  
  # Print iteration and pi value
  cat(sprintf("The current pi value is:  %#.5f for iteration: %#.2i \n", current_pi_mle, iteration))
  
  if (diff < epsilon) break
}
