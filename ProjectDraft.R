#import packages

library(arrow)
library(ggplot2)
library(dplyr)

install.packages("dplyr")
install.packages("ggplot2")

#import data

CAES_data <- read.csv('C:/Users/eburn/Desktop/EquityAgProjects/EqAgFinalRepo/data/CAES.csv' , stringsAsFactors = TRUE )

#subset data to include only the variables we need
CAES_subset <- CAES_data %>% select(Census Tract, County, Approximate Location, Traffic, Traffic Pctl, Imp. Water Bodies, Imp. Water Bodies Pctl) 

