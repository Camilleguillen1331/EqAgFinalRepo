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

ggplot(CAES_data_filter_zeros, aes(x = Traffic, y = Imp..Water.Bodies)) +
  geom_point(color = "steelblue", alpha = 0.4, size = 2) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linetype = "dashed") +
  labs(
    title = "Scatter Plot of Traffic vs. Impaired Water Bodies",
    x = "Traffic",
    y = "Impaired Water Bodies"
  ) +
  theme_bw()

# making a table 

# Install and load broom if you haven't already
install.packages("broom")  # Only if not installed
library(broom)

# regression table
regression_table <- tidy(ols)

print(regression_table)


regression_table <- tidy(ols, conf.int = TRUE)

library(gt)
regression_table %>%
  gt()

write.csv(regression_table, "regression_table.csv", row.names = FALSE)




# Step 1: Create a binary variable: 1 if impaired water bodies > 0, else 0
CAES_data_binary <- CAES_data_filter %>%
  mutate(any_impairment = ifelse(Imp..Water.Bodies > 0, 1, 0))

# Step 2: Run logistic regression (binary outcome)
regression_binary <- glm(any_impairment ~ Traffic, data = CAES_data_binary, family = binomial)

# Step 3: View summary of the model
summary(regression_binary)

# Step 4: Get tidy regression table with odds ratios and confidence intervals
regression_table_binary <- tidy(regression_binary, conf.int = TRUE, exponentiate = TRUE)

# Step 5: Print the tidy table
print(regression_table_binary)

ggplot(CAES_data_binary, aes(x = Traffic, y = any_impairment)) +
  geom_jitter(height = 0.1, color = "blue", alpha = 0.6, size = 2) +
  labs(
    title = "Scatter Plot of Traffic vs. Impairment Presence",
    x = "Traffic Volume",
    y = "Impairment Presence (0 = No, 1 = Yes)"
  ) +
  theme_minimal()



# trying to add line of best fit 
ggplot(CAES_data_binary, aes(x = Traffic, y = any_impairment)) +
  geom_jitter(height = 0.1, color = "blue", alpha = 0.6, size = 2) +
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    se = TRUE,
    color = "red"
  ) +
  labs(
    title = "Scatter Plot of Traffic vs. Impairment Presence",
    x = "Traffic Volume",
    y = "Impairment Presence (0 = No, 1 = Yes)"
  ) +
  theme_minimal()

# without line of best fit
ggplot(CAES_data_binary, aes(x = Traffic, y = any_impairment)) +
  geom_jitter(height = 0.1, color = "blue", alpha = 0.6, size = 2) +
  geom_smooth(
  ) +
  coord_cartesian(xlim = c(min(CAES_data_binary$Traffic), max(CAES_data_binary$Traffic)),
                  ylim = c(0.95, 1.05)) +  # zoom in to only show y ≈ 1
  labs(
    title = "Zoomed In: High Impairment Cases (Impaired > 1)",
    x = "Traffic Volume",
    y = "Impairment Presence (0 = No, 1 = Yes)"
  ) +
  theme_minimal()

# trying to make box plot 
ggplot(CAES_data_binary, aes(x = factor(any_impairment), y = Traffic, fill = factor(any_impairment))) +
  geom_boxplot() +
  scale_fill_manual(
    values = c("0" = "lightgray", "1" = "skyblue"),
    labels = c("No Impairment", "Yes Impairment")
  ) +
  labs(
    title = "Boxplot of Traffic by Impairment Status",
    x = "Impairment Presence",
    y = "Traffic Volume",
    fill = "Impairment"
  ) +
  scale_x_discrete(labels = c("0" = "No Impairment", "1" = "Yes Impairment")) +
  theme_minimal()

table(CAES_data_binary$any_impairment)

# filter the data 
CAES_data_binary <- CAES_data_binary %>%
  filter(Traffic < 25000)

# violin plot
ggplot(CAES_data_binary, aes(x = factor(any_impairment), y = Traffic, fill = factor(any_impairment))) +
  geom_violin(trim = FALSE) +
  scale_fill_manual(
    values = c("0" = "lightgray", "1" = "skyblue"),
    labels = c("No Impairment", "Yes Impairment")
  ) +
  labs(
    title = "Violin Plot of Traffic by Impairment Status",
    x = "Impairment Presence",
    y = "Traffic Volume",
    fill = "Impairment"
  ) +
  scale_x_discrete(labels = c("0" = "No Impairment", "1" = "Yes Impairment")) +
  theme_minimal()

# add blox plot inside
ggplot(CAES_data_binary, aes(x = factor(any_impairment), y = Traffic, fill = factor(any_impairment))) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  scale_fill_manual(
    values = c("0" = "lightgray", "1" = "skyblue"),
    labels = c("No Impairment", "Yes Impairment")
  ) +
  labs(
    title = "Violin Plot with Boxplot Inside: Traffic by Impairment Status",
    x = "Impairment Presence",
    y = "Traffic Volume",
    fill = "Impairment"
  ) +
  scale_x_discrete(labels = c("0" = "No Impairment", "1" = "Yes Impairment")) +
  theme_minimal()

# add mean points 
ggplot(CAES_data_binary, aes(x = factor(any_impairment), y = Traffic, fill = factor(any_impairment))) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "black", outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "green") +
  scale_fill_manual(
    values = c("0" = "lightgray", "1" = "purple"),
    labels = c("No Impairment", "Yes Impairment")
  ) +
  labs(
    title = "Violin Plot of Traffic by Impairment Status with Mean Points",
    x = "Impairment Presence",
    y = "Traffic Volume",
    fill = "Impairment"
  ) +
  scale_x_discrete(labels = c("0" = "No Impairment", "1" = "Yes Impairment")) +
  theme_minimal()




