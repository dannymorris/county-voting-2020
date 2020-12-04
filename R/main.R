library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(ggplot2)
library(tidycensus)
library(d3heatmap)
library(arules)
library(DT)

census_api_key(Sys.getenv("CENSUS_API_KEY"))

# functions for working with association rules
source("R/utils/arules_utils.R")

# functions for collecting data 
source("R/voting_results.R")
source("R/acs/population.R")
source("R/acs/age.R")
source("R/acs/race.R")
source("R/acs/gini.R")
source("R/acs/income.R")
source("R/acs/unemployment.R")
source("R/acs/education.R")
source("R/acs/industry.R")
source("R/acs/citizenship.R")
source("R/acs/insurance.R")
source("R/acs/veterans.R")

#################
## Source data ##
#################

acs_vars <- load_acs_vars()
acs_concepts <- get_acs_concepts()

votes <- get_voting_results()
population <- get_population(tag = "POPULATION")
age <- get_age(tag = "AGE")
race <- get_race(tag = "RACE")
gini <- get_gini(tag = "GINI")
income <- get_income(tag = "INCOME")
unemployment <- get_unemployment(tag = "UNEMPLOY")
education <- get_education(tag = "EDU_ATTAIN")
industry <- get_industry(tag = "INDUSTRY")
citizenship <- get_citizenship(tag = "CITIZEN")
insurance <- get_insurance(tag = "HEALTH_INSURANCE")
veterans <- get_veterans(tag = "VETERANS")

###########################
## Prepare training data ##
###########################

data_joins <- votes %>%
  select(fips, state, name, party_winner, trump_pct, margin) %>%
  inner_join(population, by = c("fips" = "GEOID")) %>%
  inner_join(age, by = c("fips" = "GEOID")) %>%
  inner_join(race, by = c("fips" = "GEOID")) %>%
  inner_join(gini, by = c("fips" = "GEOID")) %>%
  inner_join(income, by = c("fips" = "GEOID")) %>%
  inner_join(unemployment, by = c("fips" = "GEOID")) %>%
  inner_join(education, by = c("fips" = "GEOID")) %>%
  inner_join(industry, by = c("fips" = "GEOID")) %>%
  inner_join(citizenship, by = c("fips" = "GEOID")) %>%
  inner_join(insurance, by = c("fips" = "GEOID")) %>%
  inner_join(veterans , by = c("fips" = "GEOID")) %>%
  drop_na() 

population_adjusted <- data_joins %>%
  mutate_at(vars(AGE_18_29,
                 AGE_30_44,
                 AGE_45_59,
                 AGE_60_Plus,
                 contains("UNEMPLOY"),
                 contains("HEALTH_INSURANCE"),
                 contains("VETERANS"),
                 contains("EDU_ATTAIN"),
                 contains("INDUSTRY")), 
            list(~./AGE_18_Plus)) %>%
  mutate_at(vars(contains("RACE"),
                 contains("CITIZEN")),
            list(~./POPULATION_Total))

ggplot(population_adjusted,
       aes(x = trump_pct,
           y = RACE_Total__Asian_alone)) +
  geom_point()

#######################
## Association Rules ##
#######################

binary_data <- population_adjusted %>%
  select(-fips, -state, -name, -party_winner, -margin, 
         -AGE_18_Plus, -POPULATION_Total) %>%
  mutate(trump_pct = bucket_percentage(trump_pct)) %>%
  mutate_at(vars(-trump_pct), convert_numeric_to_discrete) %>%
  convert_to_dummies() 

transactions <- convert_df_to_transactions(df = binary_data)

rules <- run_apriori(
  trans = transactions,
  min_supp = 0.001,
  min_conf = 0.001,
  minlen = 2,
  maxlen = 2,
  default = "lhs",
  rhs = "trump_pct_X4"
)

rules_df <- convert_rules_to_df(rules = rules)

make_rules_DT(rules_df = rules_df)

