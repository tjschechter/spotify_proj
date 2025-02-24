## script for exploratory data analysis of spotify data

## -------------------------------------------------------------

# SET UP

## packages to use
#install.packages(c("tidyverse", "tidymodels",
 #                  "data.table","janitor"))

## with packages installed, load them in as needed

library(tidyverse) ## data wrangling suite
library(data.table) ## data wrangling faster
library(magrittr) ## for piping


## update packages globally

#update.packages(ask = FALSE, checkBuilt = TRUE)

## read in data

data <- fread("data/spotify_songs.csv")

## -------------------------------------------------------------

# INITIAL ANALYSIS

# FACTORS

## start with a general structure of the data using summary()

data_sum <- summary(data) %>% as.data.table()

## from domain knowledge of music, factor mode and key (not continuous)

## mutate key to factor

data <- data %>% mutate(key = factor(key))

## mutate mode to factor

data <- data %>% mutate(mode = factor(mode))

# DATE OF RELEASE

## load in lubridate library

library(lubridate) ## date wrangling

## what was the last song released? When?

## convert release date to a date column

data <- data %>% 
  mutate(track_album_release_date = as.Date(track_album_release_date,
                                            format = c("%Y-%m-%d")))

## find the most recent release

max(data$track_album_release_date)

## 2020-01-29

## Create a column for age of song

## Ensure it is in a quantified manner, days in this case

data <- data %>% 
  mutate(track_age = as.numeric(as.Date("2020-01-29") - 
           as.Date(track_album_release_date)))


## what distribution of age of track?

hist(data$track_age)

## mostly younger songs... any correlation with popularity?

## take the min max scaling of the data to normalize it

## caret install

install.packages("caret")

library(caret)

## select a data frame of numeric data

numeric_data <- data %>% select_if(is.numeric)

## process the data s.t. it is all normalized

norm_data <- preProcess(numeric_data, method = c("range"))

norm_dataset <- predict(norm_data, numeric_data)

## check the distributions now

hist(norm_dataset$track_popularity)
hist(norm_dataset$track_age)

## create a linear model 

age_popularity <- lm(track_popularity ~ track_age, norm_dataset)

summary(age_popularity)

## ok... popularity - need to see distribution

## create a plot to explore relationship bw age, popularity

data %>% ggplot(aes(x = track_age, y = track_popularity)) +
  geom_point(alpha = 0.5)

norm_dataset %>% ggplot(aes(x = track_age, y = track_popularity)) +
  geom_point()


## ------------------------------------------------------------

# DISTRIBUTIONS

## Check the distributions of the normalized, scaled data

hist(norm_dataset$danceability) ## kurtosis to left, skew right

hist(norm_dataset$energy) ## kurtosis to left, skew right

hist(norm_dataset$loudness) ## approx normal dist

hist(norm_dataset$speechiness) ## skew left, kurtosis right

hist(norm_dataset$acousticness) ## skew left, kurtosis right

hist(norm_dataset$instrumentalness) ## skew left, kurtosis right

hist(norm_dataset$liveness) ## skew left, kurtosis right

hist(norm_dataset$valence) ## approx norm dist

hist(norm_dataset$tempo) ## approx norm 

hist(norm_dataset$duration_ms) ## approx norm

## --------------------------------------------------------------

# RELATIONSHIPS

## run a correlation matrix across the normalized data

cor_mat <- cor(norm_dataset, method = c("pearson"))

## install and unpack the necessary package

#install.packages("ggcorrplot")

library(ggcorrplot)

## visualize the correlation matrix

ggcorrplot(cor_mat)

## ---------------------------------------------------------------

## Subset a data set to export and use for predictions

sub_data <- data[,c(4,7,10:24)]

#write.csv(sub_data,"modelling_data.csv")
