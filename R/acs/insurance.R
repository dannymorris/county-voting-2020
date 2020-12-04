
library(dplyr)
library(tidyr)
library(stringr)

source("R/utils/call_api.R")
source("R/utils/tag_colnames.R")
source("R/utils/get_acs_variables.R")

get_insurance <- function(tag) {
  
  variables <- load_acs_vars() %>%
    filter(concept == "TYPES OF HEALTH INSURANCE COVERAGE BY AGE") %>%
    filter(!str_detect(label, "Under 19 years")) %>%
    filter(str_detect(label, "With one type|No health insurance")) %>%
    filter(str_detect(label, "No health insurance|employer-based|direct-purchase|Medicare|Medicaid|TRICARE|VA")) %>%
    select(name, label)
  
  out <- call_api(vars = variables$name) %>%
    inner_join(variables, by = c("variable" = "name")) %>%
    mutate(label = str_remove_all(label, "!!|Estimate|Total|19 to 34 years|35 to 64 years|65 years and over")) %>%
    group_by(GEOID, label) %>%
    summarise(estimate = sum(estimate)) %>%
    ungroup() %>%
    spread(label, estimate) %>%
    rename_at(vars(-GEOID), list(~tag_colnames(., tag)))
  
  return(out)
}
