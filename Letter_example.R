# Application of multi-class logistic to letters data
library(microbenchmark)
# Load the letter data
#########################
# Training data
letter_train <- read.table("letter-train.txt", header = F, colClasses = "numeric")
Y <- letter_train[, 1]
X <- as.matrix(letter_train[, -1])

# Testing data
letter_test <- read.table("letter-test.txt", header = F, colClasses = "numeric")
Yt <- letter_test[, 1]
Xt <- as.matrix(letter_test[, -1])

# [ToDo] Make sure to add column for an intercept to X and Xt
add_train <- array(1, dim = dim(X)[1])
add_test <- array(1, dim = dim(Xt)[1])
X <- cbind(add_train, X)
Xt <- cbind(add_test, Xt)
# Source the LR function
source("FunctionsLR.R")

# [ToDo] Try the algorithm LRMultiClass with lambda = 1 and 50 iterations. Call the resulting object out, i.e. out <- LRMultiClass(...)
out <- LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL)

# The code below will draw pictures of objective function, as well as train/test error over the iterations
plot(out$objective, type = 'o')
plot(out$error_train, type = 'o')
plot(out$error_test, type = 'o')

# Feel free to modify the code above for different lambda/eta/numIter values to see how it affects the convergence as well as train/test errors

#change numIter
out11 <- LRMultiClass(X, Y, Xt, Yt, numIter = 100, eta = 0.1, lambda = 1, beta_init = NULL)

#error_train reaches to 20.90
#error_test reaches to 25.27778
#objective value reaches to 1686

out12 <- LRMultiClass(X, Y, Xt, Yt, numIter = 1000, eta = 0.1, lambda = 1, beta_init = NULL)

#error_train reaches to 19.80
#error_test reaches to 24.81667
#objective value reaches to 1658.283
#If increase the number of iterations then errors, objective value converges to a minimum value

#change eta
out21 <- LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.5, lambda = 1, beta_init = NULL)

#error_train reaches to 19.80
#error_test reaches to 24.83889
#objective value reaches to 1660.530

out22 <- LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.8, lambda = 1, beta_init = NULL)

#error_train reaches to 19.75
#error_test reaches to 24.80556
#objective value reaches to 1658.104
#If increase the value of eta then errors, objective value converges to a minimum value, but if it increase more it doesn't give the values

#change lambda
out31 <- LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.1, lambda = 10, beta_init = NULL)

#error_train reaches to 22.90
#error_test reaches to 26.51111
#objective value reaches to 2249.191

out32 <- LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.1, lambda = 0.5, beta_init = NULL)

#error_train reaches to 21.60
#error_test reaches to 26.16111
#objective value reaches to 1731.578
#If decrease the value of lambda then errors, objective value converges to a minimum value

# [ToDo] Use microbenchmark to time your code with lambda=1 and 50 iterations. To save time, only apply microbenchmark 5 times.

# [ToDo] Report the median time of your code from microbenchmark above in the comments below

