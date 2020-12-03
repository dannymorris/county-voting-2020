library(tidycensus)
library(dplyr)
library(tidyr)

source("call_api.R")
source("tag_colnames.R")
source("list_acs_variables.R")

get_gini <- function(tag) {

  gini_vars <- load_acs_vars() %>%
    filter(name == "B19083_001") %>%
    select(name, label) %>%
    distinct()
  
  out <- call_api(vars = "B19083_001") %>%
    inner_join(gini_vars, by = c("variable" = "name")) %>%
    mutate(label = str_remove_all(label, "Estimate!!|!!Female|!!Male")) %>%
    select(-variable) %>%
    spread(label, estimate) %>%
    rename_at(vars(-GEOID), list(~tag_colnames(., tag)))
  
  return(out)
}
