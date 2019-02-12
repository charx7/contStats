print('Im working!')
# Clear the Environment Variables
rm(list = ls())

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

# Concatenate the data to get bold X
data = cbind(X1, X2)

# Generate the response vector Y
Y = X1 - X2 + epsilons

# Set up the lamda parameter
lamda = 0.5

# lamda matrix
lamdaMatrix = lamda * diag(2)

# Calculate t(X) * X
# Note solve(arg) calculates the inverse of a sqr matrix
betas_ridge = (solve((t(data) %*% data) + lamdaMatrix) %*% t(data)) %*% Y

# Calculate Average squared error of the estimators
Y_pred = data %*% betas_ridge
errors_vector = (Y - Y_pred) ^ 2
MSE = sum(errors_vector)/10
