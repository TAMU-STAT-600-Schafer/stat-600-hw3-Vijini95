# This is a script to save your own tests for the function
source("FunctionsLR.R")

#Simple example
# Sample data
X <- matrix(c(
  1, 1.0, 2.0,   # Sample 1
  1, 1.5, 1.8,   # Sample 2
  1, 2.0, 1.0,   # Sample 3
  1, 2.2, 1.5,   # Sample 4
  1, 3.0, 0.5,   # Sample 5
  1, 3.2, 1.0    # Sample 6
), nrow = 6, byrow = TRUE)

# Class labels
y <- c(0, 0, 1, 1, 2, 2)
numIter = 15
# Call the function with the synthetic dataset
result <- LRMultiClass(X, y, X, y, numIter = 15, eta = 0.5, lambda = 0)

# Access the outputs
beta_estimated <- result$beta
error_train <- result$error_train
objective_values <- result$objective

#After 15 iterations, objective value reaches to 0.004241083, and error reaches to 0.

# Plot objective values
plot(0:numIter, objective_values, type = 'b', xlab = 'Iteration', ylab = 'Objective Function Value', main = 'Objective Function over Iterations')

# Plot error_train values
plot(0:numIter, error_train, type = 'b', xlab = 'Iteration', ylab = 'Objective Function Value', main = 'Objective Function over Iterations')
