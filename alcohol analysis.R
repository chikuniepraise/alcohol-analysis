# Importing libraries
library(readr)
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)

# importing the datasets
consumerpref <- read_excel("Consumer Preferences and Purchase Patterns.xlsx")
alcoholPerCap <- read.csv("Alcohol consumption per capital.csv")

str(consumerpref)
str(alcoholPerCap$)

consumerpref <- data.frame(consumerpref)
consumerpref <- consumerpref %>%
  drop_na()

#View(alcoholPerCap)
#View(consumerpref)


# data cleaning
# removing empty columns in consumerpref

#empty columns in the consumerpref dataset includes:
# Name, and Last.modified.time, 
consumerpref <- consumerpref %>% select(-Name, -Last.modified.time)
# Removal of columns that are not necessary for the analysis
# such as completion time and start time of participants, and email
consumerpref <- consumerpref %>% select(-Name, -Last.modified.time)
consumerpref <- consumerpref %>% select(-Start.time, -Completion.time, -Email)



# cleaning of the alcoholPerCap dataset
# removing empty columns
alcoholPerCap <- alcoholPerCap %>% select(-c(9, 10, 11, 12, 17, 18, 19))
# removing columns that are not need for analysis
alcoholPerCap <- alcoholPerCap %>% select(-c(1,2,3,5,7,9:10, 12:16))

#renaming the columns for better understanding.

colnames(alcoholPerCap)

colnames(alcoholPerCap)[1] = "Country_name"
colnames(alcoholPerCap)[2] = "Year"
colnames(alcoholPerCap)[3] = "Alcohol_Types"
colnames(alcoholPerCap)[4] = "Consumption_per_Capital"

colnames(alcoholPerCap)

# renaming the consumerpref columns
colnames(consumerpref)

colnames(consumerpref)[6] = "UK residents' annual income"
colnames(consumerpref)[7] = "Nigerian residents' Annual Income"
colnames(consumerpref)[8] = "Frequently consume alcohol"
colnames(consumerpref)[9] = "Preferred type of alcohol"
colnames(consumerpref)[10] = "Favorite drinking spot"
colnames(consumerpref)[11] = "Average alcohol consumed per session"
colnames(consumerpref)[12] = "Favorite alcohol brand"
colnames(consumerpref)[13] = "Brand's influence"
colnames(consumerpref)[14] = "Alcohol purchasing channel"
colnames(consumerpref)[15] = "How often do you buy alcohol?"
colnames(consumerpref)[16] = "Monthly Spendings on Alcohol"
colnames(consumerpref)[17] = "Drink alone"
colnames(consumerpref)[18] = "Mix drinks with"
colnames(consumerpref)[19] = "Duration of drinking"
colnames(consumerpref)[20] = "Age started drinking"
colnames(consumerpref)[21] = "Mood's influence"
colnames(consumerpref)[22] = "Identify fakes"
colnames(consumerpref)[23] = "gifted alcohol?"
colnames(consumerpref)[24] = "Shots before getting high"
colnames(consumerpref)[25] = "Purchase decisions influenced by marketing"
colnames(consumerpref)[26] = "Health effects of alcohol consumption"
colnames(consumerpref)[27] = "Willingness to try new alcohol products or flavors"

str(consumerpref)
colnames(consumerpref)

# extracting the rows on consumption per capital for Nigeria and United Kingdom
NGAperCap <- subset(alcoholPerCap, Country_name == "NGA" & Alcohol_Types == "SA_TOTAL")
str(NGAperCap)
#View(NGAperCap)

UKperCap <- subset(alcoholPerCap, Country_name == "GBR" & Alcohol_Types == "SA_TOTAL")
View(UKperCap)
# analysis using the objectives

mergedpercap=rbind(NGAperCap,UKperCap)
# View(mergedpercap)
# 1. What are the specific patterns of alcohol consumption in Nigeria and the UK in terms of frequency, quantity, and timing?
# using the variables

# Trend Plot
ggplot(data = mergedpercap, aes(x = Year, y = Consumption_per_Capital, color = Country_name)) +
  geom_line() +
  geom_point() +
  labs(x = "Country", y = "Consumption per Capital", title = " Trends for Nigeria and the United Kingdom") +
  theme_minimal()

# GDP Trend Plot
ggplot(data = gathered_data, aes(x = country, y = Value, color = Variable, group = Variable)) +
  geom_line() +
  geom_point() +
  labs(x = "Country", y = "Value", title = "GDP Trends for Nigeria (NGA)") +
  theme_minimal()




consumerpref <- na.omit(consumerpref)
# Summary statistics of numerical variables
summary(consumerpref[c("ID", "Monthly Spendings on Alcohol")])

# Summary of categorical variables
table(consumerpref$Gender)
table(consumerpref$Occupation)
table(consumerpref$Location)
# and so on for other categorical variables...


# Create a bar plot of age distribution
ggplot(consumerpref, aes(x = factor(Age))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Count")

# Create a bar plot of preferred type of alcohol
ggplot(consumerpref, aes(x = `Preferred type of alcohol`)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Preferred Type of Alcohol", x = "Type of Alcohol", y = "Count")

# Create a boxplot of monthly spending on alcohol
ggplot(consumerpref, aes(y = `Monthly Spendings on Alcohol`)) +
  geom_boxplot(fill = "pink", color = "darkred") +
  labs(title = "Monthly Spendings on Alcohol", y = "Amount", x = "UK/Nigerian Resident")

# Create a bar plot of the frequency of alcohol consumption
ggplot(consumerpref, aes(x = `Frequently consume alcohol`)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Frequency of Alcohol Consumption", x = "Frequency", y = "Count")





# Frequency of alcohol consumption
ggplot(consumerpref, aes(x = `Frequently consume alcohol`, fill = Location)) +
  geom_bar(position = "dodge") +
  labs(title = "Frequency of Alcohol Consumption", x = "Frequency", y = "Count")

# Quantity of alcohol consumed per session
ggplot(consumerpref, aes(x = `Average alcohol consumed per session`, fill = Location)) +
  geom_bar(position = "dodge") +
  labs(title = "Average Alcohol Consumed per Session", x = "Average Drinks", y = "Count")

# Timing of alcohol consumption
ggplot(consumerpref, aes(x = `Duration of drinking`, fill = Location)) +
  geom_bar(position = "dodge") +
  labs(title = "Duration of Drinking", x = "Duration", y = "Count")

# Preferred type of alcohol
ggplot(consumerpref, aes(x = `Preferred type of alcohol`, fill = Location)) +
  geom_bar(position = "dodge") +
  labs(title = "Preferred Type of Alcohol", x = "Type of Alcohol", y = "Count")

# Influence of mood on alcohol choice
ggplot(consumerpref, aes(x = `Mood's influence`, fill = Location)) +
  geom_bar(position = "dodge") +
  labs(title = "Influence of Mood on Alcohol Choice", x = "Influence", y = "Count")

# Influence of being gifted alcohol
ggplot(consumerpref, aes(x = `gifted alcohol?`, fill = Location)) +
  geom_bar(position = "dodge") +
  labs(title = "Influence of Being Gifted Alcohol", x = "Influence", y = "Count")


# Preferred type of alcohol by income level
ggplot(consumerpref, aes(x = `Preferred type of alcohol`, fill = Location)) +
  geom_bar(position = "dodge") +
  labs(title = "Preferred Type of Alcohol by Income Level", x = "Type of Alcohol", y = "Count")

# Monthly spending on alcohol by income level
ggplot(consumerpref, aes(x = factor(`Monthly Spendings on Alcohol`), fill = Location)) +
  geom_bar(position = "dodge") +
  labs(title = "Monthly Spending on Alcohol by Income Level", x = "Monthly Spendings", y = "Count")

# Preferred type of alcohol by age group
ggplot(consumerpref, aes(x = `Preferred type of alcohol`, fill = factor(Age))) +
  geom_bar(position = "dodge") +
  labs(title = "Preferred Type of Alcohol by Age Group", x = "Type of Alcohol", y = "Count")

# Preferred type of alcohol by gender
ggplot(consumerpref, aes(x = `Preferred type of alcohol`, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Preferred Type of Alcohol by Gender", x = "Type of Alcohol", y = "Count")





