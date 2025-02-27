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


