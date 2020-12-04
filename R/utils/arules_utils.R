
library(dplyr)
library(stringr)
library(arules)
library(recipes)
library(DT)

bucket_percentage <- function(x) {
  out <- vector(mode = "character", length = length(x))
  out[x < 0.25] <- "1"
  out[x >= 0.25 & x < 0.5] <- "2"
  out[x >= 0.5 & x < 0.75] <- "3"
  out[x >= 0.75] <- "4"
  return(out)
}

convert_numeric_to_discrete <- function(x, n = 10) {
  out <- ntile(x = x, n = n) %>% as.character()
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

convert_df_to_transactions <- function(df) {
  out <- df %>%
    as("matrix") %>%
    as("transactions")
  return(out)
}

run_apriori <- function(trans, min_supp = 0.001, min_conf = 0.001, 
                        minlen = 2, maxlen = 2, default = "lhs", rhs) {
  out <- apriori(
    data = trans,
    parameter = list(supp = min_supp, conf = min_conf, minlen = minlen, maxlen = maxlen),
    appearance = list(default = default, rhs = rhs)
  )
  return(out)
}

convert_rules_to_df <- function(rules) {
  out <- tibble(
    lhs = labels(lhs(rules)),
    rhs = labels(rhs(rules)), 
    rules@quality
  )  
  return(out)
}

cleanse_var_names <- function(x) {
  out <- x %>%
    str_remove_all("\\{|\\}|") %>%
    str_replace_all("_", " ") %>%
    str_squish()
  return(out)
}

make_rules_DT <- function(rules_df) {
  out <- rules_df %>%
    mutate(lhs = cleanse_var_names(lhs)) %>%
    arrange(desc(lift)) %>%
    datatable() %>%
    formatRound(columns = c("support", "confidence", "coverage", "lift"), digits = 3)
  return(out)
}
