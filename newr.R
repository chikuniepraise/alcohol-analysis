# Importing libraries
library(readr)
library(tidyr)
library(readxl)
library(dplyr)
library(ggplot2)

# importing the datasets
consumerpref <- read_excel("Analyzing Consumer Preferences and Purchase Patterns in the Alcohol Industry(1-215).xlsx")
alcoholPerCap <- read.csv("Alcohol consumption per capital.csv")

consumerpref <- data.frame(consumerpref)


# data cleaning
# removing unnecessary columns in consumerpref
consumerpref <- consumerpref %>%
  select(-c(Name, Last.modified.time, Start.time, Completion.time, Email))

# removing unnecessary columns in alcoholPerCap
alcoholPerCap <- alcoholPerCap %>% select(-c(9, 10, 11, 12, 17, 18, 19))
alcoholPerCap <- alcoholPerCap %>% select(-c(1,2,3,5,7,9:10, 12:16))

colnames(alcoholPerCap)
colnames(alcoholPerCap)[1] = "Country_name"
colnames(alcoholPerCap)[2] = "Year"
colnames(alcoholPerCap)[3] = "Alcohol_Types"
colnames(alcoholPerCap)[4] = "Consumption_per_Capital"
# View(alcoholPerCap)



#data preprocessing

# Rename columns in consumerpref
colnames(consumerpref) <- c("ID","Age", "Gender",  "Occupation", "Location",
                            "UK residents' annual income", "Nigerian residents' Annual Income",
                            "Frequently consume alcohol", "Preferred type of alcohol",
                            "Favorite drinking spot", "Average alcohol consumed per session",
                            "Favorite alcohol brand", "Brand's influence",
                            "Alcohol purchasing channel", "How often do you buy alcohol?",
                            "Monthly Spendings on Alcohol", "Drink alone", "Mix drinks with",
                            "Duration of drinking", "Age started drinking", "Mood's influence",
                            "Identify fakes", "gifted alcohol?", "Shots before getting high",
                            "Purchase decisions influenced by marketing",
                            "Health effects of alcohol consumption",
                            "Willingness to try new alcohol products or flavors")


# cleaning of the alcoholPerCap dataset
# removing empty columns
consumerpref <- consumerpref %>%
  drop_na()


View(alcoholPerCap)
# Convert necessary columns to appropriate data types and renaming
consumerpref <- consumerpref %>%
  mutate_at(vars(`Frequently consume alcohol`,`Average alcohol consumed per session`, `Preferred type of alcohol`, 
                 `Favorite drinking spot`,
                 `Favorite alcohol brand`, `Brand's influence`, `Alcohol purchasing channel`,
                 `How often do you buy alcohol?`, `Drink alone`, `Mix drinks with`, `Duration of drinking`,
                 `Mood's influence`, `Identify fakes`, `gifted alcohol?`, `Shots before getting high`,
                 `Purchase decisions influenced by marketing`, `Health effects of alcohol consumption`,
                 `Willingness to try new alcohol products or flavors`), as.factor)

str(consumerpref)
# Data preprocessing for Average alcohol consumed per session
# Convert the categorical values to numeric representations
# Define a function to map the categories to numerical values
map_to_numeric <- function(x) {
  if (x == "None") return(0)
  if (grepl("1-2", x)) return(1.5)
  if (grepl("3-4", x)) return(3.5)
  if (grepl("5 or more", x)) return(5)
  return(NA) # For any other values not matching the patterns
}



# Apply the function to the column
consumerpref$Average_alcohol_numeric <- sapply(consumerpref$`Average alcohol consumed per session`, map_to_numeric)
View(consumerpref)
# Summary statistics of numerical variables
summary(consumerpref[c("ID", "Monthly Spendings on Alcohol")])

# Summary of categorical variables
table(consumerpref$Gender)
table(consumerpref$Occupation)
table(consumerpref$Location)
# and so on for other categorical variables...

# Data visualization

# 1.	What are the specific patterns of alcohol consumption in 
#     Nigeria and the UK in terms of frequency, quantity, and timing?

# Frequency of alcohol consumption
frequency_table <- table(consumerpref$`Frequently consume alcohol`, consumerpref$Location)
print(frequency_table)

# Average alcohol consumed per session
avg_alcohol_consumed <- aggregate(consumerpref$`Average_alcohol_numeric`, 
                                  by = list(consumerpref$Location),
                                  FUN = mean)
print(avg_alcohol_consumed)

# Timing of drinking
ggplot(consumerpref, aes(x = `Duration of drinking`, fill = Location)) +
  geom_bar(position = "dodge") +
  labs(title = "Timing of Drinking", x = "Duration", y = "Count")

ggplot(consumerpref, aes(x = `Frequently consume alcohol`, fill = Location)) +
  geom_bar(position = "dodge") +
  labs(title = "Frequency of Alcohol Consumption", x = "Frequency", y = "Count")


# Bar plot of age distribution
ggplot(consumerpref, aes(x = factor(Age), fill = Location)) +
  geom_bar(position = "dodge") +
  labs(title = "Age Distribution", x = "Age", y = "Count")


# 2.	What types of alcoholic beverages are most preferred in Nigeria and the UK, and why?

# Preferred type of alcohol
preferred_type_table <- table(consumerpref$`Preferred type of alcohol`, consumerpref$Location)
print(preferred_type_table)

# Bar plot of preferred type of alcohol
ggplot(consumerpref, aes(x = `Preferred type of alcohol`)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Preferred Type of Alcohol", x = "Type of Alcohol", y = "Count")


# Bar plot of preferred type of alcohol in nigeria and UK
ggplot(consumerpref, aes(x = `Preferred type of alcohol`,fill = `Location`)) +
  geom_bar(position = "dodge") +
  labs(title = "Preferred Type of Alcohol", x = "Type of Alcohol", y = "Count")

#3.  3.	How do socio-cultural factors, such as traditions, customs, and societal norms,
# influence the choice of alcoholic beverages and consumption patterns in Nigeria and the UK?

# Cross-tabulation between Preferred type of alcohol and Location
cross_tab <- table(consumerpref$`Preferred type of alcohol`, consumerpref$Location)
print(cross_tab)

# Chi-square test for association
chi_sq_test <- chisq.test(cross_tab)
print(chi_sq_test)



# 4.	How do economic factors, such as income levels, pricing, and availability,
#     shape the choice of alcoholic beverages and consumption patterns in both countries?
  
# Data preprocessing for Monthly Spendings on Alcohol
# Convert the categorical values to numeric representations
# Define a function to map the categories to numerical values
map_to_numeric_spending <- function(x) {
  if (grepl("Less than", x)) return(50)
  if (grepl("(N\\d+,\\d+ - N\\d+,\\d+)|(£\\d+ - £\\d+)", x)) return(75)
  if (grepl("Above", x)) return(200)
  if (grepl("Not much", x)) return(25)
  return(NA) # For any other values not matching the patterns
}
# Apply the function to the column
consumerpref$Monthly_Spending_numeric <- sapply(consumerpref$`Monthly Spendings on Alcohol`, map_to_numeric_spending)
View(consumerpref)
# Now you can use the `Monthly_Spending_numeric` column for numerical analysis

# Create the side-by-side bar plot for Monthly Spendings on Alcohol by Location
ggplot(consumerpref, aes(x = `Monthly_Spending_numeric`, fill = Location)) +
  geom_bar(position = "dodge") +
  labs(title = "Monthly Spendings on Alcohol by Location", x = "Monthly Spendings", y = "Count") +
  scale_x_continuous(breaks = c(25, 50, 75, 200), labels = c("Not much", "Less than N50,000 or £50", "(N50,000 - N99,000) or (£50 - £99)", "Above N200,000 or £200"))



# extracting the rows on consumption per capital for Nigeria and United Kingdom
NGAperCap <- subset(alcoholPerCap, Country_name == "NGA" & Alcohol_Types == "SA_TOTAL")
str(NGAperCap)
#View(NGAperCap)
UKperCap <- subset(alcoholPerCap, Country_name == "GBR" & Alcohol_Types == "SA_TOTAL")
View(UKperCap)
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
  


#5.	What is the impact of demographic factors, such as age, gender,
#   and education, on alcohol preference and consumption patterns 
#   in Nigeria and the UK?

# Bar plot of preferred type of alcohol by Gender
ggplot(consumerpref, aes(x = `Preferred type of alcohol`, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Preferred Type of Alcohol by Gender", x = "Type of Alcohol", y = "Count")


# Bar plot of preferred type of alcohol by Gender
ggplot(consumerpref, aes(x = `Preferred type of alcohol`, fill = Age)) +
  geom_bar(position = "dodge") +
  labs(title = "Preferred Type of Alcohol by Gender", x = "Type of Alcohol", y = "Count")
