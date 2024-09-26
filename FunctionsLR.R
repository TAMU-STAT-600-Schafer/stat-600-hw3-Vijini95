compute_probabilities <- function(X, beta) {
  #Compute linear scores for each class
  linear_scores <- X %*% beta
  # Compute the exponentials
  exp_scores <- exp(linear_scores)
  # Compute the sum over classes for each sample
  sum_exp_scores <- rowSums(exp_scores)
  # Compute probabilities by dividing exponentials by the sum
  probabilities <- exp_scores / sum_exp_scores
  # Now, probabilities[i, k] = p_k(x_i; beta)
  return(probabilities)
}
#Objective value
compute_objective <- function(X, y_adj, beta, lambda) {
  probabilities <- compute_probabilities(X, beta)
  nll <- -sum(log(probabilities[cbind(1:nrow(X), y_adj)]))
  reg <- (lambda / 2) * sum(beta ^ 2)
  return(nll + reg)
}
#Prediction
predict_classes <- function(X, beta) {
  probabilities <- compute_probabilities(X, beta)
  predicted_classes <- max.col(probabilities)
  return(predicted_classes)
}

# Function that implements multi-class logistic regression.
#############################################################
# Description of supplied parameters:
# X - n x p training data, 1st column should be 1s to account for intercept
# y - a vector of size n of class labels, from 0 to K-1
# Xt - ntest x p testing data, 1st column should be 1s to account for intercept
# yt - a vector of size ntest of test class labels, from 0 to K-1
# numIter - number of FIXED iterations of the algorithm, default value is 50
# eta - learning rate, default value is 0.1
# lambda - ridge parameter, default value is 1
# beta_init - (optional) initial starting values of beta for the algorithm, should be p x K matrix

## Return output
##########################################################################
# beta - p x K matrix of estimated beta values after numIter iterations
# error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
# error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
# objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
LRMultiClass <- function(X, y, Xt, yt, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL){
  
  ## Check the supplied parameters as described. You can assume that X, Xt are matrices; y, yt are vectors; and numIter, eta, lambda are scalars. You can assume that beta_init is either NULL (default) or a matrix.
  ###################################
  # Check that the first column of X and Xt are 1s, if not - display appropriate message and stop execution.
  if (!all(X[, 1] == 1)) {
    stop("The first column of X must be all one")
  }
  if (!all(Xt[, 1] == 1)) {
    stop("The first column of Xt must be all one")
  }
  # Check for compatibility of dimensions between X and Y
  if (nrow(X) != length(y)) {
    stop("Number of rows in X must be same with length of y")
  }
  # Check for compatibility of dimensions between Xt and Yt
  if (nrow(Xt) != length(yt)) {
    stop("Number of rows in Xt must be same with length of yt")
  }
  # Check for compatibility of dimensions between X and Xt
  if (ncol(X) != ncol(Xt)) {
    stop("Number of columns in X and Xt must be the same")
  }
  # Check eta is positive
  if (eta <= 0) {
    stop("eta must be positive.")
  }
  # Check lambda is non-negative
  if (lambda < 0) {
    stop("lambda must be non-negative.")
  }
  # Check whether beta_init is NULL. If NULL, initialize beta with p x K matrix of zeroes. If not NULL, check for compatibility of dimensions with what has been already supplied.
  K <- length(unique(y)) #K represents number of unique classes
  if (is.null(beta_init)) {
    beta <- matrix(0, nrow = ncol(X), ncol = K)
  } else {
    if (!all(dim(beta_init) == c(ncol(X), K))) {
      stop("Dimension of beta_init must be p x K.")
    }
    beta <- beta_init
  }
  ## Calculate corresponding pk, objective value f(beta_init), training error and testing error given the starting point beta_init
  ##########################################################################
  y_adj <- y + 1
  yt_adj <- yt + 1
  
  #Calculate corresponding pk
  probabilities <- compute_probabilities(X, beta)
  
  #Calculate objective value
  objective <- numeric(numIter + 1)
  objective[1] <- compute_objective(X, y_adj, beta, lambda)
  
  #Compute Training Error:
  predicted_train <- predict_classes(X, beta)
  error_train <- numeric(numIter + 1)
  error_train[1] <- mean(predicted_train != y_adj) * 100
  
  #Compute Testing Error:
  predicted_test <- predict_classes(Xt, beta)
  error_test <- numeric(numIter + 1)
  error_test[1] <- mean(predicted_test != yt_adj) * 100
  ## Newton's method cycle - implement the update EXACTLY numIter iterations
  ##########################################################################
  
  # Within one iteration: perform the update, calculate updated objective function and training/testing errors in %
  for (iter in 1:numIter) {
    # Compute probabilities for current beta
    probabilities <- compute_probabilities(X, beta)
    
    # Initialize gradient and Hessian matrices
    gradients <- matrix(0, nrow = ncol(X), ncol = K)
    hessians <- list()
    
    # Loop over each class
    for (k in 1:K) {
      # Compute gradient for class k
      indicator <- as.numeric(y_adj == k)
      p_k <- probabilities[, k]
      grad_k <- t(X) %*% (p_k - indicator) + lambda * beta[, k]
      
      # Compute Hessian for class k without storing Wk
      w_k <- p_k * (1 - p_k)  # n x 1 vector
      X_weighted <- X * w_k  # Element-wise multiplication
      H_k <- t(X) %*% X_weighted + lambda * diag(ncol(X))
      
      # Solve for delta_beta_k
      delta_beta_k <- solve(H_k, grad_k)
      
      # Update beta_k
      beta[, k] <- beta[, k] - eta * delta_beta_k
    }
    
    # Compute objective value at new beta
    objective[iter + 1] <- compute_objective(X, y_adj, beta, lambda)
    
    # Compute training error
    predicted_train <- predict_classes(X, beta)
    error_train[iter + 1] <- mean(predicted_train != y_adj) * 100
    
    # Compute testing error
    predicted_test <- predict_classes(Xt, beta)
    error_test[iter + 1] <- mean(predicted_test != yt_adj) * 100
  }
  
  ## Return output
  ##########################################################################
  # beta - p x K matrix of estimated beta values after numIter iterations
  # error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
  # error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
  # objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
  return(list(beta = beta, error_train = error_train, error_test = error_test, objective =  objective))
}
