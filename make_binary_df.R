
library(dplyr)
library(recipes)

convert_numeric_to_ordinal_int <- function(x, n = 5) {
  out <- ntile(x = x, n = n) %>% as.factor()
  return(out)
}

convert_to_dummies <- function(train_data) {
  rec <- recipe(~ ., data = train_data)
  out <- rec %>%
    step_dummy(all_predictors(), one_hot = T) %>%
    prep() %>%
    bake(new_data = train_data)
  return(out)
}
