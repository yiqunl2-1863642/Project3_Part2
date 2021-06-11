#' Random Forest Cross-validation function
#'
#' This function performs a random forest cross validation on the penguin data.
#'
#' @param k Numeric input used as the number of folds.
#' @keywords inference
#'
#' @return Numeric indicating the average mean square error across \code{k}
#'   folds
#'
#' @examples
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k) {
  data("my_penguins")
  penguins <- my_penguins %>% tidyr::drop_na()
  # create a vector that randomly assign observations to fold 1,...,k
  fold <- sample(rep(1:k, length = nrow(penguins)))
  MSE <- rep(0, k)
  for (i in 1:k) {
    # assign the observations with indexes of fold not equal to i to be training
    # data
    TRAINING_DATA <- penguins[fold != i,]
    # assign the observations with indexes of fold equal to i to be test
    # data
    TEST_DATA <- penguins[fold == i,]
    # assign body masses with indexes of fold equal to i to be the true value
    # of body mass
    TRUE_DATA <- penguins$body_mass_g[fold==i]
    # create a model using random forest algorithm
    MODEL <- randomForest::randomForest(body_mass_g ~ bill_length_mm +
                            bill_depth_mm + flipper_length_mm,
                          data = TRAINING_DATA, ntree = 100)
    # predict body mass based on the random model
    PREDICTIONS <- predict(MODEL, TEST_DATA[, 3:5])
    # record the MSE of this fold
    MSE[i] <- mean((PREDICTIONS - TRUE_DATA)^2)
  }
  # return the average of MSE in each fold
  return(mean(MSE))
}

utils::globalVariables(c("bill_length_mm", "bill_depth_mm", "body_mass_g", "flipper_length_mm"))
