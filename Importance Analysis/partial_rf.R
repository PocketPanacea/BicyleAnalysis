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

imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

partials <- data.frame()

for (i in seq_along(names(features))) {
  partial <- partialPlot(rf, features, names(features)[i], plot=FALSE)
  xt <- rescale(partial$x)
  partials <- rbind(partials, data.frame(x=partial$x, xt=xt, y=partial$y, feature=names(features)[i]))
}

ranges <- ddply(partials, "feature", function(d) {
  r <- range(d$y)
  data.frame(feature=d$feature[1], range=r[2]-r[1])
})

features_to_plot <- ranges[ranges$range>0.05*max(ranges$range),"feature"]

ggplot(partials[partials$feature %in% features_to_plot,], aes(x=xt, y=y, color=feature)) +
  geom_line(size=2) +
  theme_light(base_size=16) +
  xlab("Feature Range (Min to Max)") +
  ylab("Hourly Bike Rentals") 

