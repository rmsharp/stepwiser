#' Forms dataset used for simulation
#' 
#' @param n number of observations
#' @param betas coefficients used to form actual predictive variables
#' @param corr correlations used in making predictive variables
#' @param p number of predictive variables
#' @export
make_dataset <- function(n, betas, corr, p) {
  y <- rnorm(n, 0, 100)
  error_fun <- function(y) {
    rnorm(n, sample(1:10, 1), sample(50:200, 1))
  }
  X <- get_predictors(y, betas, corr, p, error_fun)
  data <- as.data.frame(X, drop = FALSE)
  names(data) <- paste0("X_", 1:ncol(data))
  data <- data.frame(y = y, data)
  train_test <- sample(seq(n), size = n * 0.5, replace = FALSE)
  train <- data[train_test, ]
  test <- data[-train_test, ]
  train_y <- as.matrix(train["y"])
  test_y <- as.matrix(test["y"])
  train_X <- as.matrix(train[setdiff(colnames(data), "y")])
  test_X <- as.matrix(test[setdiff(colnames(data), "y")])
  list(all = data,
       train = train,
       test = test,
       train_y = train_y,
       test_y = test_y,
       train_X = train_X,
       test_X = test_X)
}
