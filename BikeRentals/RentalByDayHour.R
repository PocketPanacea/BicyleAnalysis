library(ggplot2)
library(lubridate)
library(readr)
library(scales)

# because of ggplot version, import library(splines) to replace parameter "se"
#library(splines)

# Tweak these to show something else on the axes
x_axis <- "times"
y_axis <- "count"
color  <- "day"

# The competition data is stored in the /input directory
setwd("E:/R Workstation/BikeRentals/")
train <- read_csv("input/train.csv")

# Write some basic stats to the log
cat("Number of training rows ", nrow(train), "\n")
cat("Number of test rows ", nrow(test), "\n")
head(train)

# calculate train$data
train$hour  <- hour(ymd_hms(train$datetime))
train$times <- as.POSIXct(strftime(ymd_hms(train$datetime), format="%H:%M:%S"), format="%H:%M:%S")
train$day   <- wday(ymd_hms(train$datetime), label=TRUE)

# ggplot
ggplot(train, aes_string(x=x_axis, y=y_axis, color=color)) +
  geom_smooth(se=FALSE, fill=NA, size=2) +
  theme_light(base_size=20) +
  xlab("Hour of the Day") +
  scale_x_datetime(date_minor_breaks="1 hours", date_breaks="4 hours", labels=date_format("%I:%M %p")) + 
  ylab("Number of Bike Rentals") +
  scale_color_discrete("") +
  ggtitle("\n") +
  theme(plot.title=element_text(size=18))

