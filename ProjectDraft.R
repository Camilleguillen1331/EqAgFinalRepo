#import packages

library(arrow)
library(ggplot2)
library(dplyr)

install.packages("dplyr")
install.packages("ggplot2")
install.packages("arrow")
#import data

# # eden path
# CAES_data <- read.csv('C:/Users/eburn/Desktop/EquityAgProjects/EqAgFinalRepo/data/CAES.csv' , stringsAsFactors = TRUE )

# Cam path
CAES_data <- read.csv('~/Desktop/EquityAgSB/EqAgFinalRepo/data/CAES.csv' , stringsAsFactors = TRUE )


#subset data to include only the variables we need
CAES_data <- CAES[c(1, 3, 5, 25, 26, 33, 34)]

#creating a plot to view our data 
ggplot(CAES_data, aes(x = Traffic )) +
  geom_histogram(binwidth = 1000, fill = "steelblue", color = "white") +
  labs(title = "Histogram of Traffic Density", x = "Values", y = "Frequency") +
  theme_minimal()

ggplot(CAES_data, aes(x = Traffic, y = Imp..Water.Bodies)) +
  geom_point(color = "steelblue", alpha = 0.6, size = 2) +
  labs(title = "Scatter Plot of Traffic vs. Impared Water Bodies",
       x = "Traffic",
       y = "Impared Water Bodies") +
  theme_minimal()

# Filter data for a specific county
CAES_data_filter <- CAES_data %>%
  filter(California.County %in% c("Imperial", "Los Angeles", "Kern", "Orange", "Riverside",
                       "San Bernardino", "San Diego", "San Luis Obispo", 
                       "Santa Barbara", "Ventura"))
# re run the plots 
ggplot(CAES_data_filter, aes(x = Traffic )) +
  geom_histogram(binwidth = 1000, fill = "steelblue", color = "white") +
  labs(title = "Histogram of Traffic Density", x = "Values", y = "Frequency") +
  theme_minimal()
# scatter plots 
ggplot(CAES_data_filter, aes(x = Traffic, y = Imp..Water.Bodies)) +
  geom_point(color = "steelblue", alpha = 0.6, size = 2) +
  labs(title = "Scatter Plot of Traffic vs. Impared Water Bodies",
       x = "Traffic",
       y = "Impared Water Bodies") +
  theme_minimal()



