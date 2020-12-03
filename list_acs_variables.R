
library(tidycensus)
library(readr)

get_acs_variables <- function(year = 2018, dataset = "acs5") {
  census_api_key(Sys.getenv("CENSUS_API_KEY"))
  out <- load_variables(year = year, dataset = dataset, cache = TRUE)
  return(out)
}

load_acs_vars <- function() {
  read_csv("acs_variables.csv")
}