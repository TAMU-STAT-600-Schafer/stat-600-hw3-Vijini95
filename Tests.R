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

######################################################################
#use two normal populations
# Set seed for reproducibility
set.seed(123)
n_samples <- 100 # Number of samples per class
p <- 2 # Number of features (excluding intercept)

# Generate data for class 0
X0 <- matrix(rnorm(n_samples * p, mean = -2, sd = 1), n_samples, p)

# Generate data for class 1
X1 <- matrix(rnorm(n_samples * p, mean = 2, sd = 1), n_samples, p)
X <- rbind(X0, X1) # Combine the data
# Add intercept term
X <- cbind(1, X)  # Now X has p + 1 columns 
y <- c(rep(0, n_samples), rep(1, n_samples)) # Create labels

# Split data into training and testing sets
set.seed(456)  # Different seed for splitting
train_indices <- sample(1:(2 * n_samples), size = n_samples)
test_indices <- setdiff(1:(2 * n_samples), train_indices)

X_train <- X[train_indices, ]
y_train <- y[train_indices]

X_test <- X[test_indices, ]
y_test <- y[test_indices]

result1 <- LRMultiClass(X_train, y_train, X_test, y_test, numIter = 50, eta = 0.1, lambda = 1)
#objective value = 2.275099
plot(result1$objective, type = 'o')

#######################################################################
#increase number of iterations
result2 <- LRMultiClass(X_train, y_train, X_test, y_test, numIter = 100, eta = 0.1, lambda = 1)
#objective value = 2.275072
plot(result2$objective, type = 'o')
#decrease number of iterations
result3 <- LRMultiClass(X_train, y_train, X_test, y_test, numIter = 10, eta = 0.1, lambda = 1)
#objective value = 9.629257
plot(result3$objective, type = 'o')
#if the number of iteration is increasing, objective function value is decreasing.

