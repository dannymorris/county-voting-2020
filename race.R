library(tidycensus)
library(dplyr)
library(tidyr)

source("call_api.R")
source("tag_colnames.R")
source("list_acs_variables.R")

get_race <- function(tag) {
  
  race_vars <- c("B02001_002", # white
                 "B02001_003", # african american
                 "B02001_005", # asian
                 "B03001_003") # hispanic/latino
  
  race_vars <- load_acs_vars() %>%
    filter(name %in% race_vars) %>%
    select(name, label) %>%
    distinct()
  
  out <- call_api(vars = unique(race_vars$name)) %>%
    inner_join(race_vars, by = c("variable" = "name")) %>%
    mutate(label = str_remove_all(label, "Estimate!!|!!Female|!!Male")) %>%
    select(-variable) %>%
    spread(label, estimate) %>%
    rename_at(vars(-GEOID), list(~tag_colnames(., tag)))

  return(out)
}
