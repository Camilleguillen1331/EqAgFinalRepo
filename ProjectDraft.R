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


CAES_data_filter <- CAES_data_filter %>%
  filter(Traffic < 8000)



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

# taking out zeros 
CAES_data_filter_zeros <- CAES_data_filter %>%
  filter(Imp..Water.Bodies !=0)


# scatter plot 
ggplot(CAES_data_filter_zeros, aes(x = Traffic, y = Imp..Water.Bodies)) +
  geom_point(color = "steelblue", alpha = 0.6, size = 2) +
  labs(title = "Scatter Plot of Traffic vs. Impared Water Bodies",
       x = "Traffic",
       y = "Impared Water Bodies") +
  theme_minimal()

ggplot(CAES_data_filter_zeros, aes(x = Imp..Water.Bodies, y = Traffic)) +
  geom_point(color = "steelblue", alpha = 0.6, size = 2) +
  labs(title = "Scatter Plot of Traffic vs. Impared Water Bodies",
       x = "Impared Water Bodiesc",
       y = "Traffic") +
  theme_minimal()

# regression one

Regression_one <- aov(formula = Traffic ~ Imp..Water.Bodies, data = CAES_data_filter)

# summary

summary(object= Regression_one)

# save output 

sink(file = "traffic_water_anova.txt")
summary(object = Regression_one)
sink()

# linear regression 

plot(x =CAES_data_filter$Traffic, y=CAES_data_filter$Imp..Water.Bodies)

# log transformation

CAES_data_filter$logdata <- log10(CAES_data_filter$Imp..Water.Bodies)

CAES_data_filter$logdata <- asinh(CAES_data_filter$Imp..Water.Bodies)


sink(file = "logdata.txt")
summary(object = Regression_one)
sink()

# plot 
plot(x = CAES_data_filter$Traffic,
     y = CAES_data_filter$logdata,
     xlab = "Traffic",
     ylab = "Impared Water Badies")


summary(Regression_one)

# run linear model 

ols <- lm(logdata ~ Traffic, data = CAES_data_filter)
summary(ols)

plot(x = CAES_data_filter$Traffic,
     y = CAES_data_filter$logdata,
     xlab = "Traffic",
     ylab = "Impared Water Badies")
abline(CAES_data_filter$Traffic,CAES_data_filter$logdata)

ggplot(CAES_data_filter, aes(x =CAES_data_filter$Traffic , y =CAES_data_filter$logdata )) +
  geom_point(color = "purple", alpha = 0.6, size = 2) +
  labs(title = "Scatter Plot of Traffic vs. Impared Water Bodies",
       x = "Impared Water Bodiesc",
       y = "Traffic") +
  theme_bw()



