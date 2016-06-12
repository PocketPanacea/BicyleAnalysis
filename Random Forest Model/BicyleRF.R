# This R script creates a sample submission via Random Forests
# and plots the freature importance from the trained model

library(ggplot2)
library(lubridate)
library(randomForest)

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
  
  return(data[,features])
}

# input and output normalized
trainFeature <- extractFeatures(train)

testFeature <- extractFeatures(test)

submission <- data.frame(datatime = test$datetime,
                         season = test$season,
                         holiday = test$holiday,
                         workingday = test$workingday,
                         weather = test$weather,
                         temp = test$temp,
                         atemp = test$atemp,
                         humidity = test$humidity,
                         windspeed = test$windspeed,
                         count = NA
)

# We only use past data to make predictions on the test set 
# so we train a new model for each test set cutoff point

# extract basic number (y/m/d h) via function with ymd_hms
for (i_year in unique(year(ymd_hms(test$datetime)))) {
  
  for (i_month in unique(month(ymd_hms(test$datetime)))) {
    
    # Write process to the log with Random Forest
    cat("Year: ", i_year, "\tMonth: ", i_month, "\n")
    
    testLocs   <- year(ymd_hms(test$datetime)) == i_year & month(ymd_hms(test$datetime)) == i_month
    testSubset <- test[testLocs,]
    trainLocs  <- ymd_hms(train$datetime) <= min(ymd_hms(testSubset$datetime))
    
    rf <- randomForest(extractFeatures(train[trainLocs,]), train[trainLocs,"count"], ntree = 100)
    
    submission[testLocs, "count"] <- predict(rf, extractFeatures(testSubset))
    
  }
  
}

write.csv(submission, file = "output/submission_rf_output.csv", row.names = FALSE)

# Random Forest Importance
rf <- randomForest(extractFeatures(train), train$count, ntree=100, importance=TRUE)
imp <- importance(rf, type = 1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

# plot the features with library(ggplot)
ggplot(featureImportance, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "#0099FF") +
  coord_flip() + 
  theme_light(base_size = 20) +
  xlab("Importance") +
  ylab("") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title = element_text(size = 18))

