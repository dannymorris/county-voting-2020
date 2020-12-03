library(tidycensus)
library(dplyr)
library(tidyr)

source("call_api.R")
source("tag_colnames.R")

bucket_age <- function(x) {
  out <- vector("character", length = length(x))
  out[x == "Total"] <- "Total"
  out[str_detect(x, "18|20|21|22 to 24|25 to 29")] <- "18_29"
  out[str_detect(x, "30 to 34|35 to 39|40 to 44")] <- "30_44"
  out[str_detect(x, "45 to 49|50 to 54|55 to 59")] <- "45_59"
  out[str_detect(x, "60 and 61|62 to 64|65 and 66|67 to 69|70 to 74|75 to 79|80 to 84|85 years and over")] <- "60_Plus"
  return(out)  
}

get_age <- function(tag) {
  
  age_vars <- load_acs_vars() %>%
    filter(concept == "SEX BY AGE") %>%
    filter(!str_detect(label, 'Under 5|5 to 9|10 to 14|15 to 17')) %>%
    filter(!label %in% c("Estimate!!Total", "Estimate!!Total!!Male", "Estimate!!Total!!Female")) %>%
    select(name, label) %>%
    distinct()
  
  out <- call_api(vars = age_vars$name) %>%
    inner_join(age_vars, by = c("variable" = "name")) %>%
    mutate(label = str_remove_all(label, "Estimate!!|!!Female|!!Male")) %>%
    group_by(GEOID, label) %>%
    summarise(estimate = sum(estimate)) %>%
    ungroup() %>%
    mutate(Age_Band = bucket_age(label)) %>%
    group_by(GEOID, Age_Band) %>%
    summarise(estimate = sum(estimate)) %>%
    ungroup() %>%
    spread(Age_Band, estimate) %>%
    mutate(`18_Plus` = apply(.[,-1], 1, sum)) %>%
    rename_at(vars(-GEOID), list(~tag_colnames(., tag)))
  
  return(out)
}
