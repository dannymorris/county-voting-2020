library(tidycensus)
library(dplyr)
library(tidyr)

source("call_api.R")
source("tag_colnames.R")
source("list_acs_variables.R")

get_industry <- function(tag) {
  
  ind_vars <- load_acs_vars() %>%
    filter(concept == "SEX BY INDUSTRY FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER") %>%
    filter(str_detect(label, "Agriculture|Construction|Manufacturing|Retail|Information|scientific|Educational|Arts|Public")) %>%
    filter(!label %in% c("Estimate!!Total!!Male!!Agriculture, forestry, fishing and hunting, and mining",
                         "Estimate!!Total!!Male!!Professional, scientific, and management, and administrative, and waste management services",
                         "Estimate!!Total!!Male!!Educational services, and health care and social assistance",
                         "Estimate!!Total!!Male!!Arts, entertainment, and recreation, and accommodation and food services",
                         "Estimate!!Total!!Female!!Agriculture, forestry, fishing and hunting, and mining",
                         "Estimate!!Total!!Female!!Professional, scientific, and management, and administrative, and waste management services",
                         "Estimate!!Total!!Female!!Educational services, and health care and social assistance",
                         "Estimate!!Total!!Female!!Arts, entertainment, and recreation, and accommodation and food services"))
  
  out <- call_api(vars = ind_vars$name) %>%
    inner_join(ind_vars, by = c("variable" = "name")) %>%
    mutate(label = str_remove_all(label, "Estimate!!|!!Female|!!Male")) %>%
    group_by(GEOID, label) %>%
    summarise(estimate = sum(estimate)) %>%
    ungroup() %>%
    spread(label, estimate) %>%
    rename_at(vars(-GEOID), list(~tag_colnames(., tag)))
  
  return(out)
}
