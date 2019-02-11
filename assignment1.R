print('Im working!')

# Set random seed for reproducibility
set.seed(42)

# Generate 10 data points which are N(0,1)
X1 = rnorm(10, 0, 1);

# Generate the epsilons vector
epsilons = rnorm(10,0,1)

# Specify the tau parameter
tau = 0.5

# The U parameter for the generation of X2
U = rnorm(10,0,tau^2)

# Calculate X2
X2 = (sqrt(1-tau^2) * X1) + U

epsilons
