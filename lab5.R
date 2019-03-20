# Clear the workspace
rm(list=ls())
cat("\014") 
# Set random seed for reproducibility
set.seed(100)

# Sample from the continous uniform 
X = runif(100, min=0, max=1) 
Z = runif(100, min=0, max=1)

# Stack one and the other (matrix form)
data = cbind(X,Z)

# Get the true labels according to the real rule if X < Z => -1, +1 otherwise 
labels = numeric()
for(i in 1:100) {
  if( data[i,1] < data[i,2]) {
    labels = c(labels, -1)
  } else {
    labels = c(labels, +1)
  }
}

# Sort to loop 
X_sorted = sort(X, decreasing = FALSE)
Z_sorted = sort(Z, decreasing = TRUE)

# Missclasifications function
scoreClassifier = function(true_y, pred_y) {
  missclassifications = numeric()
  # Compute missclassification for this rule
  for (i in 1:100) {
    if (true_y[i] != pred_y[i]) {
      missclassifications = c(missclassifications, 1)
    } else {
      missclassifications = c(missclassifications, 0)
    }
  }
  # Calculate the number of missclassifications
  num_missclassifications = sum(missclassifications)
  return(num_missclassifications)
}

# Classifier function
classifyHorizontal = function(data, constant) {
  predictions = numeric()
  for (i in 1:100) {
    if (data[i,1] > constant) {
      predictions = c(predictions, +1)
    } else {
      predictions = c(predictions, -1)
    }
  }
  # return the predictions vector 
  return (predictions)
}
# The other line
classifyVertical = function(data, constant) {
  predictions = numeric()
  for (i in 1:100) {
    if (data[i,1] > constant) {
      predictions = c(predictions, -1)
    } else {
      predictions = c(predictions, +1)
    }
  }
  # return the predictions vector 
  return (predictions)
}

## Attention the vertical and horizontal funcs are reversed
# Fit model func
fitModel = function(data) {
  # Initialization of params
  bestScore  = 100000
  bestParam  = 0
  vertical   = FALSE
  horizontal = FALSE
  bestPredictions = runif(100, min=0, max=1)
  currentParametersArray = c(bestScore, bestParam, vertical, horizontal, bestPredictions)
  names(currentParametersArray) = c("Best_Score", "Best_Params", "Vertical", "Horizontal", "Predictions")
  
  for (i in 1:100) {
    # Try the first classifier
    firstClassifierPredictions = classifyHorizontal(data, X_sorted[i])
    print(length(firstClassifierPredictions))
    # Calculate the current number of missclassification on the model
    currScore = scoreClassifier(labels, firstClassifierPredictions)
    if (currScore < bestScore) {
      bestPredictions = firstClassifierPredictions
      bestScore = currScore
      # Parameters for the rule
      bestParam = X_sorted[i]
      vertical = TRUE
      horizontal = FALSE
    }
    
    # Try the second classifier
    secondClassifierPredictions = classifyVertical(data, Z_sorted[i])
    # Calculate the current number of missclassification on the model
    currScore = scoreClassifier(labels, secondClassifierPredictions)
    if (currScore < bestScore) {
      bestPredictions = secondClassifierPredictions
      bestScore = currScore
      # Parameters for the rule
      bestParam  = Z_sorted[i]
      vertical   = FALSE
      horizontal = TRUE
    }
  }
  
  currentParametersArray[1] = bestScore
  currentParametersArray[2] = bestParam
  currentParametersArray[3] = vertical
  currentParametersArray[4] = horizontal
  currentParametersArray[5] = bestPredictions
  
  return(currentParametersArray)
} 
  
# Boosting loop
modelsArray = numeric()
for (j in 1:10) {
  currFittedModel = fitModel(data)
  # Compute the error rate of the bet fitted model
  
}
