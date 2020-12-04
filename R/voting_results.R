
library(dplyr)
library(readr)

get_voting_results <- function() {
  out <- read_csv("data/presidential.csv") %>%
    select(state, fips, name, votes,
           trump = results_trumpd,
           biden = results_bidenj,
           party_winner = leader_party_id) %>%
    mutate(trump_pct = trump/votes) %>%
    mutate(biden_pct = biden/votes) %>%
    mutate(margin = abs(trump_pct - biden_pct)) 
  return(out)
}
