library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(ggplot2)
library(tidycensus)
library(d3heatmap)
library(h2o)
library(arules)

census_api_key(Sys.getenv("CENSUS_API_KEY"))

source("utils.R")
source("get_voting_data.R")
source("population.R")
source("age.R")
source("race.R")
source("gini.R")
source("income.R")
source("unemployment.R")
source("education.R")
source("industry.R")
source("citizenship.R")
source("insurance.R")
source("veterans.R")

source("make_binary_df.R")
source("models.R")

votes <- get_voting_data()
population <- get_population(tag = "POPULATION")
age <- get_age(tag = "AGE")
race <- get_race(tag = "RACE")
gini <- get_gini(tag = "GINI")
income <- get_income(tag = "INCOME")
unemployment <- get_unemployment(tag = "UNEMP")
education <- get_education(tag = "EDU")
industry <- get_industry(tag = "INDUSTRY")
citizenship <- get_citizenship(tag = "CITIZEN")
insurance <- get_insurance(tag = "HEALTH_INSURANCE")
veterans <- get_veterans(tag = "VETERANS")

train_data <- votes %>%
  select(fips, party_winner, trump_pct) %>%
  inner_join(population, by = c("fips" = "GEOID")) %>%
  inner_join(age, by = c("fips" = "GEOID")) %>%
  inner_join(race, by = c("fips" = "GEOID")) %>%
  inner_join(gini, by = c("fips" = "GEOID")) %>%
  #inner_join(income, by = c("fips" = "GEOID")) %>%
  inner_join(unemployment, by = c("fips" = "GEOID")) %>%
  inner_join(education, by = c("fips" = "GEOID")) %>%
  inner_join(industry, by = c("fips" = "GEOID")) %>%
  inner_join(citizenship, by = c("fips" = "GEOID")) %>%
  inner_join(insurance, by = c("fips" = "GEOID")) %>%
  inner_join(veterans , by = c("fips" = "GEOID")) %>%
  mutate_at(vars(AGE_18_29:AGE_60_Plus,
                 contains("UNEMP"),
                 contains("HEALTH_INSURANCE"),
                 contains("VETERANS")), 
            list(~./AGE_18_Plus)) %>%
  mutate_at(vars(contains("RACE"),
                 contains("EDU"),
                 contains("INDUSTRY"),
                 contains("CITIZEN")),
            list(~./POPULATION_Total)) %>%
  mutate_at(vars(party_winner), as.factor) %>%
  mutate_at(vars(-fips, -party_winner, -trump_pct), list(z_scale)) %>%
  drop_na() %>%
  select(-fips, -POPULATION_Total) 

#######################
## Exploratory plots ##
#######################

train_data %>%
  select(x = trump_pct,
         y = HEALTH_INSURANCE_No_health_insurance_19_Plus) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point()

train_data %>% 
  select(-party_winner) %>%
  cor() %>%
  broom::tidy() %>%
  select(-.rownames) %>%
  d3heatmap(labRow = NULL, 
            labCol = NULL,
            Rowv = NULL,
            Colv = NULL)

##########################
## Principal Components ##
##########################

pca <- train_data %>%
  select(-party_winner, -trump_pct) %>%
  prcomp()

pca$rotation %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  as_tibble() %>%
  View

library(psych)
library(GPArotation)
fa(r=cor(m1), nfactors=8, rotate="varimax", SMC=FALSE, fm="minres")

fa <- train_data %>%
  select(-trump_pct, -party_winner) %>%
  cor() %>%
  fa(r=., nfactors=8, rotate="varimax", SMC=FALSE, fm="minres")

summary(fa)
fa$loadings -> ll


##################################
## Explanatory regression model ##
##################################

h2o.init()

lin_reg <- run_linear_regression(
  train_data = train_data %>% select(-party_winner),
  target = "trump_pct",
  family = "gaussian",
  alpha = 0,
  lambda = NULL,
  lambda_search = T
)

get_training_metrics(lin_reg)
get_coefficients(lin_reg)

lin_reg_pred <- get_predictions_vec(lin_reg, train_data)

train_data %>%
  mutate(pred = lin_reg_pred) %>%
  ggplot(aes(x = trump_pct, y = pred)) +
  geom_point()

#################################
## Predictive regression model ##
#################################

h2o.init()

gbm_reg <- run_gbm(
  train_data = train_data %>% select(-party_winner),
  target = "trump_pct",
  ntrees = 150,
  learn_rate = 0.1,
  nfolds = 5
)

get_cv_metrics(gbm_reg)
get_var_importance(gbm_reg)

#######################
## Association Rules ##
#######################

binary_data <- train_data %>%
  drop_na() %>%
  mutate_if(is.numeric, convert_numeric_to_ordinal_int) %>%
  select(-fips, -party_winner, -AGE_18_Plus, -POPULATION_Total) %>%
  convert_to_dummies()

transactions <- binary_data %>%
  as("matrix") %>%
  as("transactions")

rules <- apriori(
  data = transactions,
  parameter = list(supp = 0.01, conf = 0.1, minlen = 2),
  appearance = list(default="rhs", lhs="trump_pct_X5")
)

sort(rules,  by="confidence", decreasing=TRUE) %>%
  head(20) %>%
  inspect()

rules_df <- tibble(
  lhs = labels(lhs(rules)),
  rhs = labels(rhs(rules)), 
  rules@quality
)

library(DT)

rules_df %>%
  mutate(rhs = str_remove_all(rhs, "\\{|\\}|")) %>%
  mutate(rhs = str_replace_all(rhs, "_", " ")) %>%
  mutate(rhs = str_squish(rhs)) %>%
  arrange(desc(lift)) %>%
  datatable() %>%
  formatRound(columns = c("support", "confidence", "coverage", "lift"), digits = 3)
f
