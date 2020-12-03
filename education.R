library(tidycensus)
library(dplyr)
library(tidyr)

source("call_api.R")
source("tag_colnames.R")
source("list_acs_variables.R")

get_education <- function(tag) {
  
  edu_vars <- load_acs_vars() %>%
    filter(str_detect(concept, "SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER")) %>%
    filter(!str_detect(concept, "MEDIAN EARNINGS")) %>%
    filter(str_detect(concept, "WHITE ALONE|AFRICAN AMERICAN ALONE|ASIAN ALONE|HISPANIC OR LATINO")) %>%
    filter(!str_detect(concept, "NOT HISPANIC OR LATINO")) %>%
    filter(str_detect(label, "school|college|degree")) %>%
    select(name, label, concept) %>%
    distinct()
  
  edu <- call_api(vars = edu_vars$name)
  
  out <- edu %>%
    inner_join(edu_vars, by = c("variable" = "name")) %>%
    mutate(label = str_remove_all(label, "Estimate!!|!!Female|!!Male")) %>%
    group_by(GEOID, label) %>%
    summarise(estimate = sum(estimate)) %>%
    ungroup() %>%
    spread(label, estimate) %>%
    rename_at(vars(-GEOID), list(~tag_colnames(., tag)))
  
  return(out)
}
