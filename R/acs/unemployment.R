library(tidycensus)
library(dplyr)
library(tidyr)
library(stringr)

source("R/utils/call_api.R")
source("R/utils/tag_colnames.R")
source("R/utils/get_acs_variables.R")

get_unemployment <- function(tag) {
  
  emp_vars <- load_acs_vars() %>%
    filter(str_detect(label, "Unemploy")) %>%
    filter(str_detect(label, "16 to 64|65 years and over")) %>%
    filter(str_detect(concept, "SEX BY AGE BY EMPLOYMENT STATUS FOR THE POPULATION 16 YEARS AND OVER")) %>%
    filter(str_detect(concept, "WHITE ALONE|BLACK OR AFRICAN AMERICAN ALONE|ASIAN ALONE|HISPANIC OR LATINO")) %>%
    filter(!str_detect(concept, "WHITE ALONE, NOT HISPANIC OR LATINO")) %>%
    select(name, concept, label) %>%
    distinct()
  
  out <- call_api(vars = emp_vars$name) %>%
    inner_join(emp_vars, by = c("variable" = "name")) %>%
    mutate(concept = str_remove_all(concept, "SEX BY AGE BY EMPLOYMENT STATUS FOR THE POPULATION")) %>%
    group_by(GEOID, concept) %>%
    summarise(estimate = sum(estimate)) %>%
    ungroup() %>%
    spread(concept, estimate) %>%
    mutate(Total_16_YEARS_AND_OVER = apply(.[,-1], 1, sum)) %>%
    rename_at(vars(-GEOID), list(~tag_colnames(., tag)))
  
  return(out)
}
