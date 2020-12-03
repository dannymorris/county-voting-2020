library(tidycensus)
library(dplyr)
library(tidyr)
library(stringr)

source("call_api.R")
source("tag_colnames.R")
source("list_acs_variables.R")

get_insurance <- function(tag) {
  
  variables <- load_acs_vars() %>%
    filter(str_detect(concept, "HEALTH INSURANCE COVERAGE STATUS BY SEX BY AGE")) %>%
    filter(str_detect(label, "No health insurance coverage")) %>%
    filter(!str_detect(label, "Under 6|6 to 18")) %>%
    filter(!label %in% c("Estimate!!Total", "Estimate!!Total!!Male", "Estimate!!Total!!Female")) %>%
    select(name, label)
  
  out <- call_api(vars = variables$name) %>%
    inner_join(variables, by = c("variable" = "name")) %>%
    mutate(label = str_remove_all(label, "Estimate!!|!!Female|!!Male")) %>%
    group_by(GEOID) %>%
    summarise(No_health_insurance_19_Plus = sum(estimate)) %>%
    ungroup() %>%
    rename_at(vars(-GEOID), list(~tag_colnames(., tag)))
  
  return(out)
}
