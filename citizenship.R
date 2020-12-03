library(tidycensus)
library(dplyr)
library(tidyr)

source("call_api.R")
source("tag_colnames.R")
source("list_acs_variables.R")

get_citizenship <- function(tag) {
  
  variables <- load_acs_vars() %>%
    filter(name %in% c("B05001_006", "B05001_005", "B05001_002")) %>%
    select(name, label)
  
  out <- call_api(vars = variables$name) %>%
    inner_join(variables, by = c("variable" = "name")) %>%
    select(-variable) %>%
    spread(label, estimate) %>%
    rename_at(vars(-GEOID), list(~tag_colnames(., tag)))
  
  return(out)
}
