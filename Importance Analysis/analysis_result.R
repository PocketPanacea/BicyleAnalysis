# This R script creates a sample submission via Random Forests
# and plots the freature importance from the trained model

library(ggplot2)
library(lubridate)
library(plyr)
library(randomForest)
library(scales)

set.seed(1)

setwd("E:/R Workstation/Random Forest Model/")

train <- read.csv("input/train.csv")
test <- read.csv("input/test.csv")

# Write to the log
cat(sprintf("Training set has %d rows and %d columns\n", nrow(train), ncol(train)))
cat(sprintf("Test set has %d rows and %d columns\n", nrow(test), ncol(test)))

library(randomForest)

extractFeatures <- function(data) {
  
  features <- c("season",
                "holiday",
                "workingday",
                "weather",
                "temp",
                "atemp",
                "humidity",
                "windspeed",
                "hour")
  
  data$hour <- hour(ymd_hms(data$datetime))
  data$month <- month(ymd_hms(data$datetime))
  
  return(data[,features])
}

# Train a model across all the training data and plot the variable importance

sample_locs <- sample.int(nrow(train))
cutoff <- as.integer(nrow(train)*0.7)
internal_train <- train[sample_locs[1:cutoff],]
internal_valid <- train[sample_locs[(cutoff+1):nrow(train)],]

features <- extractFeatures(internal_train)
rf <- randomForest(features, internal_train$count, ntree=100, importance=TRUE)

valid_features <- extractFeatures(internal_valid)
valid_features$Predictions <- predict(rf, extractFeatures(internal_valid))
valid_features$Actuals     <- internal_valid$count

ggplot(valid_features, aes(x=Actuals, y=Predictions)) +
  geom_point() + 
  theme_light(base_size=16) +
  xlab("Actual Hourly Bike Rentals") +
  ylab("Predicted Hourly Bike Rentals") +
  ggtitle(paste0("Correlation: ", round(cor(valid_features$Actuals, valid_features$Predictions), 3)))

