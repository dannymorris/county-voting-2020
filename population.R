
library(tidycensus)
library(dplyr)

source("call_api.R")
source("tag_colnames.R")
source("list_acs_variables.R")

get_population <- function(tag) {
  
  pop_vars <- load_acs_vars() %>%
    filter(name == "B01001_001") %>%
    select(name, label) %>%
    distinct()
  
  out <- call_api(vars = unique(pop_vars$name)) %>%
    inner_join(pop_vars, by = c("variable" = "name")) %>%
    mutate(label = str_remove_all(label, "Estimate!!|!!Female|!!Male")) %>%
    select(-variable) %>%
    spread(label, estimate) %>%
    rename_at(vars(-GEOID), list(~tag_colnames(., tag)))
  
  return(out)
}
