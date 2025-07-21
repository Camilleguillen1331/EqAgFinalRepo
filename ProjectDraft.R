#import packages

library(arrow)
library(ggplot2)
library(dplyr)

install.packages("dplyr")
install.packages("ggplot2")
install.packages("arrow")
#import data

CAES_data <- read.csv('C:/Users/eburn/Desktop/EquityAgProjects/EqAgFinalRepo/data/CAES.csv' , stringsAsFactors = TRUE )

#subset data to include only the variables we need
CAES_data <- CAES[c(1, 3, 5, 25, 26, 33, 34)]

#creating a plot to view our data 
ggplot(CAES_data, aes(x = Traffic )) +
  geom_histogram(binwidth = 1000, fill = "steelblue", color = "white") +
  labs(title = "Histogram of Traffic Density", x = "Values", y = "Frequency") +
  theme_minimal()