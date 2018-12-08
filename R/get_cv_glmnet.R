#' Wrapper around cv.glmnet
#' 
#' @param x matrix of predictor varibles
#' @param y vector of target variables
#' @param cv_alpha The elasticnet mixing parameter
#' @import glmnet
#' @import foreach
#' @importFrom stats predict
#' @importFrom utils globalVariables
#' @export
get_cv_glmnet <- function(x, y, cv_alpha) {
  globalVariables("i")
  if (length(cv_alpha) == 1) {
    if (cv_alpha == 0 | cv_alpha == 1) {
      cv.glmnet(x, y, family = "gaussian", nfolds = 10, type.measure = "deviance", 
                parallel = TRUE, alpha = cv_alpha)
    } 
  } else {
    search <- foreach(i = cv_alpha,
                      .combine = rbind) %dopar% {
                        cv <- cv.glmnet(x, y, family = "gaussian", nfolds = 10, 
                                        type.measure = "deviance", 
                                        parallel = TRUE, alpha = i)
                        data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], lambda.1se = cv$lambda.1se, alpha = i)
                      }
    search[search$cvm == min(search$cvm), ]
    
  }
}