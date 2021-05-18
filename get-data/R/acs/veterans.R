
library(dplyr)
library(tidyr)
library(stringr)

source("R/utils/call_api.R")
source("R/utils/tag_colnames.R")
source("R/utils/get_acs_variables.R")

get_veterans <- function(tag) {
  
  variables <- load_acs_vars() %>%
    filter(str_detect(concept, "SEX BY AGE BY VETERAN STATUS FOR THE CIVILIAN POPULATION 18 YEARS AND OVER")) %>%
    filter(label == "Estimate!!Total!!Veteran") %>%
    select(name, label)
  
  out <- call_api(vars = variables$name) %>%
    inner_join(variables, by = c("variable" = "name")) %>%
    select(-variable) %>%
    spread(label, estimate) %>%
    rename_at(vars(-GEOID), list(~tag_colnames(., tag)))
  
  return(out)
}
