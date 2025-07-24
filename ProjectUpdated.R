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

# cam path 

Merged_CAES <- read.csv('~/Desktop/EquityAgSB/EqAgFinalRepo/data/Merged_Census_Enviro_Data_ExportTable1.csv' , stringsAsFactors = TRUE )

#subset data to include only the variables we need
Merged_CAES <- Merged_CAES[c(1,3,17,15,37,45,46)]

# Filter data for a specific county
CAES_socal <- Merged_CAES %>%
  filter(California_County %in% c("Imperial", "Los Angeles", "Kern", "Orange", "Riverside",
                                  "San Bernardino", "San Diego", "San Luis Obispo",
                                  "Santa Barbara", "Ventura")) 

#running regression 
Regression_one <- aov(formula = Traffic ~ Imp__Water_Bodies + California_County, data = CAES_socal)

#summary 
summary(object= Regression_one)

# save output 

sink(file = "traffic_water_anova.txt")
summary(object = Regression_one)
sink()

# linear regression 

plot(x =CAES_socal$Traffic, y=CAES_socal$Imp__Water_Bodies)

# plot(x =Merged_CAES_zeros$Traffic, y=Merged_CAES_zeros$Imp__Water_Bodies)


#removing outliers 

CAES_socal <- CAES_socal %>%
  filter(Traffic < 8000)

#removing zeroes
# taking out zeros 
 # Merged_CAES_zeros <- Merged_CAES %>%
  # filter(Imp__Water_Bodies !=0)

#log transformation 

CAES_socal$logdata <- log10(CAES_socal$Imp__Water_Bodies)


CAES_socal$logdata <- asinh(CAES_socal$Imp__Water_Bodies)

sink(file = "logdata.txt")
summary(object = Regression_one)
sink()

# plot 
plot(x = CAES_socal$Traffic,
     y = CAES_socal$logdata,
     xlab = "Traffic",
     ylab = "Impaired Water Bodies")

summary(Regression_one)
 

#ols
# ols <- lm(logdata ~ Traffic + California_County , data = CAES_socal)
# summary(ols)

# san deigo 
# Make sure California_County is a factor
CAES_socal$California_County <- factor(CAES_socal$California_County)

# Set San Diego as the reference level
CAES_socal$California_County <- relevel(CAES_socal$California_County, ref = "Los Angeles")

# Now run the model
ols <- lm(logdata ~ Traffic + California_County, data = CAES_socal)

summary(ols)



# plot(x = CAES_socal$Traffic,
#      y = CAES_socal$logdata,
#      xlab = "Traffic",
#      ylab = "Impared Water Badies")
# abline(CAES_socal$Traffic,CAES_socal$logdata)
# 
# ggplot(CAES_socal, aes(x = Traffic, y = Imp__Water_Bodies)) +
#   geom_point(color = "steelblue", alpha = 0.4, size = 2) +
#   geom_smooth(method = "lm", color = "black", se = FALSE, linetype = "dashed") +
#   labs(
#     title = "Scatter Plot of Traffic vs. Impaired Water Bodies",
#     x = "Traffic",
#     y = "Impaired Water Bodies"
#   ) +
#   theme_bw()

# ## plots
# ggplot(Merged_CAES, aes(x = Traffic , y = Imp__Water_Bodies)) +
#   geom_point(aes(color = GEO_ID), alpha = 0.5) +
#   geom_smooth(method = "lm", color = "black", se = FALSE) +
#   labs(title = "Traffic vs. Impaired Water Bodies",
#        x = "Impaired Water Bodies",
#        y = "Traffic") +
#   theme_bw()
# 
# ggplot(Merged_CAES, aes(x = Traffic , y = Imp__Water_Bodies)) +
#   geom_point(aes(color = GEO_ID), alpha = 0.5) +
#   geom_smooth(method = "lm", color = "black", se = FALSE) +
#   labs(title = "Traffic vs. Impaired Water Bodies",
#        x = "Impaired Water Bodies",
#        y = "Traffic") +
#   theme_bw() +
#   theme(legend.position = "none")
# 


CAES_socal$California_County <- factor(CAES_socal$California_County)
CAES_socal$California_County <- relevel(CAES_socal$California_County, ref = "Los Angeles")


ggplot(CAES_socal, aes(x = Traffic, y = Imp__Water_Bodies)) +
  geom_point(aes(color = California_County), alpha = 0.5) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  scale_color_manual(values = c(
    "San Diego" = "#d95f02",          # Reference county color
    "Los Angeles" = "#1b9e77",
    "Orange" = "#7570b3",
    "Riverside" = "#e7298a",
    "San Bernardino" = "#66a61e",
    "Ventura" = "#e6ab02",
    "Santa Barbara" = "#a6761d",
    "San Luis Obispo" = "#666666",
    "Imperial" = "#1f78b4",
    "Kern" = "#b2df8a"
  )) +
  labs(title = "Traffic vs. Impaired Water Bodies",
       x = "Traffic",
       y = "Impaired Water Bodies",
       color = "County") +
  theme_bw()
