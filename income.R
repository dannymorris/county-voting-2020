library(tidycensus)
library(dplyr)
library(tidyr)
library(stringr)

source("call_api.R")
source("tag_colnames.R")

get_income <- function(tag) {

  variables <- load_acs_vars() %>%
    filter(str_detect(concept, "PER CAPITA INCOME")) %>%
    filter(!str_detect(concept, "INDIAN|HAWAIIAN|SOME|TWO|NOT|ASIAN")) %>%
    select(name, concept) %>%
    distinct()
  
  out <- call_api(vars = variables$name) %>%
    inner_join(variables, by = c("variable" = "name")) %>%
    select(-variable) %>%
    spread(concept, estimate) %>%
    rename_at(vars(-GEOID), list(~tag_colnames(., tag)))
  
  return(out)
}
