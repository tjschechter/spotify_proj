## Modelling with a little more expertise...

## Packages that will be used in this script:

library(tidyverse) ## data wrangling
library(data.table) ## data wrangling (faster)
library(tidymodels) ## machine learning suite
library(bestNormalize) ## for symmetric normalization

## data

data <- read_csv("data/modelling_data.csv")

## remove genre if we're keeping subgenre

data <- data[,-c(1,3:4)]

## create factors of key and mode

data$key <- factor(data$key)
data$mode <- factor(data$mode)

## set seed and split the data

set.seed(894)

split <- initial_validation_split(data, strata = track_popularity, prop = c(0.7, 0.15))

## assign training data

train <- split %>% training()

## assign testing

test <- split %>% testing()

## validation set

val <- split %>% validation()

## set seed and create validation sets for tuning

set.seed(992)

val_set <- validation_set(split)

## Preprocessing ------------------------------------------------

## create pre-processing recipe

r <- recipe(track_popularity ~ ., data = train) %>% ## set formula
  step_impute_mean(all_numeric_predictors()) %>% ## handle missing values
  step_nzv(all_numeric_predictors()) %>% ## near zero variance filter
  step_corr(all_numeric_predictors()) %>% ## multicollinearity filter
  step_orderNorm(all_numeric_predictors()) %>% ## normalize data
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) ## handle factors as dummy vars

## prep the recipe

p <- r %>% prep(retain = FALSE)

## process the validation data

val_p <- bake(p, val)
