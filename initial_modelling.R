## MODELLING SCRIPT FOR INITIAL PREDICTION OF POPULARITY

## ----------------------------------------------------------

## Set up

## packages to use

library(tidyverse) ## data wrangling
library(data.table) ## data wrangling (faster)
#install.packages("tidymodels")
library(tidymodels) ## machine learning suite

## data

data <- read_csv("data/modelling_data.csv")

## remove genre if we're keeping subgenre

data <- data[,-c(1,3:4)]

## -----------------------------------------------------------

## Pre Processing

## Set a seed for replicability, and split the data

set.seed(109)
split <- initial_split(data, prop = 0.7, strata = track_popularity)

## Training data

train <- split %>% training()

## testing data

test <- split %>% testing()

## cross validation set for tuning

## set seed for replicability

set.seed(324)
cv <- vfold_cv(train, v = 5)

## pre-processing recipe

recipe <- recipe(track_popularity ~ ., data = train) %>%
  step_impute_mean(track_age) %>% ## handle missing values
  step_normalize(all_numeric_predictors()) %>% ## normalize data
  step_dummy(all_nominal_predictors()) ## handle categorical vars


## juice into a clean dataset
juice <- recipe %>% prep() %>% juice()

## ----------------------------------------------------------

## begin modelling with a penalized regression

glm <- linear_reg(mode = "regression",
                  penalty = tune(),
                  mixture = tune()) %>%
  set_engine("glmnet")

## set a workflow

glm_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(glm)

## set environment for tuning the model

## control

gc <- control_grid(verbose = FALSE,
                   allow_par = TRUE,
                   save_pred = TRUE)

## metrics

ms <- metric_set(rmse,rsq)

## allow for parallel computing

#install.packages("doParallel")
library(doParallel)

## use 5 cores for the socket to match 5 cv folds

registerDoParallel(cores = 5)

#install.packages("glmnet")

## set seed and tune

set.seed(421)

grid <- tune_grid(
  glm_workflow,
  cv,
  grid = 30,
  control = gc,
  metrics = ms
)

stopImplicitCluster() ## stop the parallel run

autoplot(grid)

## not great metrics in terms of r-square




## random forest

#install.packages("ranger")

rf <- rand_forest(mode = "regression",
                  min_n = tune(),
                  mtry = tune(),
                  trees = 500) %>%
  set_engine("ranger")

## workflow
rf_workflow <- workflow() %>%
  add_model(rf) %>%
  add_recipe(recipe)


## go parallel and tune

registerDoParallel(cores = 5)

set.seed(92830)

rfgrid <- tune_grid(
  rf_workflow,
  cv,
  grid = 10,
  metrics = ms,
  control = gc
)

stopImplicitCluster()

autoplot(rfgrid)

## better metrics... R2 ~= .325


