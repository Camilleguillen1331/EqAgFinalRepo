#import packages

library(arrow)
library(ggplot2)
library(dplyr)


install.packages("dplyr")
install.packages("ggplot2")
install.packages("arrow")

#import data

#Annabel Path 

Merged_CAES <- read.csv('C:/Users/annab/Desktop/EquityAg/EqAgFinalRepo/data/Merged_Census_Enviro_Data_ExportTable1.csv')

#subset data to include only the variables we need
Merged_CAES <- Merged_CAES[c(1,3,17,15,37,45,46)]

# Filter data for a specific county
CAES_socal <- Merged_CAES %>%
  filter(California_County %in% c("Imperial", "Los Angeles", "Kern", "Orange", "Riverside",
                                  "San Bernardino", "San Diego", "San Luis Obispo",
                                  "Santa Barbara", "Ventura")) 

#running regression 
Regression_one <- aov(formula = Traffic ~ Imp__Water_Bodies, data = Merged_CAES)

#summary 
summary(object= Regression_one)

# save output 

sink(file = "traffic_water_anova.txt")
summary(object = Regression_one)
sink()

# linear regression 

plot(x =Merged_CAES$Traffic, y=Merged_CAES$Imp__Water_Bodies)

plot(x =Merged_CAES_zeros$Traffic, y=Merged_CAES_zeros$Imp__Water_Bodies)


#removing outliers 

Merged_CAES <- Merged_CAES %>%
  filter(Traffic < 8000)

#removing zeroes
# taking out zeros 
Merged_CAES_zeros <- Merged_CAES %>%
  filter(Imp__Water_Bodies !=0)

#log transformation 

Merged_CAES$logdata <- log10(Merged_CAES$Imp__Water_Bodies)


Merged_CAES$logdata <- asinh(Merged_CAES$Imp__Water_Bodies)

sink(file = "logdata.txt")
summary(object = Regression_one)
sink()

# plot 
plot(x = Merged_CAES$Traffic,
     y = Merged_CAES$logdata,
     xlab = "Traffic",
     ylab = "Impaired Water Bodies")

summary(Regression_one)
 

#ols
ols <- lm(logdata ~ Traffic, data = Merged_CAES)
summary(ols)

plot(x = Merged_CAES$Traffic,
     y = Merged_CAES$logdata,
     xlab = "Traffic",
     ylab = "Impared Water Badies")
abline(Merged_CAES$Traffic,Merged_CAES$logdata)

ggplot(Merged_CAES, aes(x = Traffic, y = Imp__Water_Bodies)) +
  geom_point(color = "steelblue", alpha = 0.4, size = 2) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linetype = "dashed") +
  labs(
    title = "Scatter Plot of Traffic vs. Impaired Water Bodies",
    x = "Traffic",
    y = "Impaired Water Bodies"
  ) +
  theme_bw()