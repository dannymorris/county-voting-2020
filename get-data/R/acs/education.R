
library(dplyr)
library(tidyr)
library(stringr)

source("R/utils/call_api.R")
source("R/utils/tag_colnames.R")
source("R/utils/get_acs_variables.R")

get_education <- function(tag) {
  
  variables <- load_acs_vars() %>%
    filter(str_detect(concept, "SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER")) %>%
    filter(!str_detect(concept, "MEDIAN EARNINGS")) %>%
    filter(str_detect(concept, "WHITE ALONE|AFRICAN AMERICAN ALONE|ASIAN ALONE|HISPANIC OR LATINO")) %>%
    filter(!str_detect(concept, "NOT HISPANIC OR LATINO")) %>%
    filter(str_detect(label, "school|college|degree")) %>%
    select(name, label, concept) %>%
    distinct()
  
  out <- call_api(vars = variables$name) %>%
    inner_join(variables, by = c("variable" = "name")) %>%
    mutate(label = str_remove_all(label, "Estimate!!|!!Female|!!Male")) %>%
    group_by(GEOID, label) %>%
    summarise(estimate = sum(estimate)) %>%
    ungroup() %>%
    spread(label, estimate) %>%
    rename_at(vars(-GEOID), list(~tag_colnames(., tag)))
  
  return(out)
}
