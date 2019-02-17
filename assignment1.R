# Imports
# install via cran: "install.packages("pracma", repos="http://R-Forge.R-project.org")"
library('pracma')

# Clear the Environment Variables
rm(list = ls())

# Set random seed for reproducibility
set.seed(47)

# Set length of the simulation (number of data points)
N = 10

# Generate 10 data points which are N(0,1)
X1 = rnorm(N, 0, 1);

# Generate the epsilons vector
epsilons = rnorm(N,0,1)

# Specify the tau parameter
tau = 0.7

# The U parameter for the generation of X2
U = rnorm(N,0,tau^2)

# Calculate X2
X2 = (sqrt(1-tau^2) * X1) + U

# Concatenate the data to get bold X
data = cbind(X1, X2)

# Generate the response vector Y
Y = X1 - X2 + epsilons

# Func that calculates the ridge parameters
get_ridge_estimators = function(data, Y, lamda){
  # lamda matrix
  lamdaMatrix = lamda * diag(2)
  
  # Calculate t(X) * X
  # Note solve(arg) calculates the inverse of a sqr matrix
  betas_ridge = solve((t(data) %*% data) + lamdaMatrix) %*% t(data) %*% Y
  
  # Return
  return(betas_ridge)
}

# Func that calculates the MSE
calculate_mse = function(betas_ridge, data, Y){
  # Calculate Average squared error of the estimators
  Y_pred        = data %*% betas_ridge
  errors_vector = (Y - Y_pred) ^ 2
  MSE           = sum(errors_vector) / length(Y)
  return(MSE)
}

# Generate a linspace for the lamdas
lamdas_vector = linspace(0,100,1000)

# Grid Search over the lamdas linspace
best_lamda = numeric()
best_mse   = 10000

for (i in 1:length(lamdas_vector)){
  # Get the current lamda from the linspace
  current_lamda = lamdas_vector[i]
  
  # Declare a empty vector where the X-val errors are going to be saved
  current_mse_vector = numeric()
  # Get the MSE error over LOOCV loop 
  for (j in 1:(dim(data)[1])){
    # Remove the ith example
    current_data = data[-j,1:2]
    current_Y    = Y[-j]
    
    # Generate the estimators
    current_betas = get_ridge_estimators(current_data, current_Y, current_lamda)
    # Test the error on the point you leaved out
    current_point = data[j,1:2]
    point_Y       = Y[j]
    current_error = calculate_mse(current_betas, current_point, point_Y)
    
    # Append to the error vector for the current lamda
    current_mse_vector = append(current_mse_vector, current_error)
  }
  # Reduce the error vector
  avg_error = sum(current_mse_vector)/N
  
  if (i %% 100 == 0) {
    # Print Results, NOTE: you need to use cat() because sprintf doesnt print in non-interactive mode
    cat(sprintf("The current average MSE is: %#.5f for the lamda value: %#.2f \n", avg_error, current_lamda))
  }
  
  # Best lamda check
  if (avg_error < best_mse) {
    best_mse   = avg_error
    best_lamda = current_lamda
  }
}

# plot_ly(x=X1, y=X2, z=Y, type="scatter3d", mode="markers")
# Calculate the best betas
best_betas = get_ridge_estimators(data, Y, best_lamda)
sprintf("The best lamda was: %#.2f with an MSE of %#.5f. The best betas are: %#.2f, %#.2f", 
          best_lamda, best_mse, best_betas[1], best_betas[2])
