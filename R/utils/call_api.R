
library(tidycensus)
library(dplyr)

call_api <- function(vars) {
  
  census_api_key(Sys.getenv("CENSUS_API_KEY"))
  
  out <- get_acs(
    geography = "county",
    variables = vars,
    year = 2018,
    survey = "acs5",
    geometry = F
  ) %>%
    select(GEOID, variable, estimate)
  
  return(out)
}
