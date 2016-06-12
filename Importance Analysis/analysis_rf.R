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


# Random Forest Importance
rf <- randomForest(extractFeatures(train), train$count, ntree=100, importance=TRUE)
imp <- importance(rf, type = 1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

#ggplot
ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=16) +
  xlab("") + 
  ylab("Relative Importance") +
  theme(plot.title   = element_text(size=18),
        strip.text.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

