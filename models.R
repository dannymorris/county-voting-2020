

library(h2o)
library(dplyr)

run_gbm <- function(train_data, target, ntrees = 150, learn_rate = 0.1, 
                    nfolds = 5, ...) {
  gbm <- h2o.gbm(
    x = colnames(train_data)[colnames(train_data) != target],
    y = target,
    training_frame = as.h2o(train_data),
    nfolds = nfolds,
    learn_rate = learn_rate,
    ntrees = ntrees,
    ...
  )
  return(gbm)
}


run_linear_regression <- function(train_data, target, family = "gaussian",
                                  alpha = 0, lambda = NULL, lambda_search = T) {
  lin_reg <- h2o.glm(
    x = colnames(train_data)[colnames(train_data) != target],
    y = target,
    training_frame = as.h2o(train_data),
    family = family,
    alpha = alpha,
    lambda = lambda,
    lambda_search = lambda_search
  )
  return(lin_reg)
}

get_training_metrics <- function(model) {
  out <- model@model$training_metrics
  return(out)
}

get_cv_metrics <- function(model) {
  out <- model@model$cross_validation_metrics_summary %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    as_tibble() %>%
    select(metric = rowname, mean, sd) %>%
    head(20)
  return(out)
}

get_var_importance <- function(model) {
  out <- h2o.varimp(model) %>% as_tibble()
  return(out)
}

get_coefficients <- function(model) {
  out <- model@model$standardized_coefficient_magnitudes %>% as_tibble()
  return(out)
}

get_predictions_vec <- function(model, data) {
  out <- h2o.predict(model, newdata = as.h2o(data)) %>% as.vector()
  return(out)
  
}
