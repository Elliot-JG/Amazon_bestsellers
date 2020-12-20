# Script to explore Amazon book bestsellers from 2001 to 2020 

# Load in libraries 
library(readr)
library(tidyverse)
library(dplyr)

# Load in the data 
data <- readr:: read_csv("bestsellers.csv")

# Lets just have a mess around 
# Average rating for non fiction and fiction, which one comes out on top 

# Fix column names (User rating has a space)
names(data)<-str_replace_all(names(data), c(" " = "."))

# Use base R to get rid of duplicated rows
 # This affects the averages and for now we just want to work with unique values
data <- data[!duplicated(data$Name), ]

# Data frame of average ratings based on genre
rating <- data %>%
  group_by(Genre) %>%
  summarise(avg_rate = mean(User.Rating))%>%
  mutate(avg_rate = round(avg_rate, 2))

# Data frame of average price based on genre
price <- data %>%
  group_by(Genre) %>%
  summarise(avg_price = mean(Price))%>%
  mutate(avg_price = round(avg_price, 2)) 

# PLOT
  # Average price
ggplot(price, 
       aes(x=Genre, 
           y=avg_price)) +
  geom_bar(aes(
    # Set the avg_rice as a categorical value, or a 'factor'
    fill = as.factor(avg_price)), 
    stat='identity')+
  xlab("Genre")+
  ylab("Average price ($)")+
  # Limit y axes
  coord_cartesian(ylim = c(5, 15)) +
  theme_grey(base_size = 25) +
  labs(fill = "Average price ($)")


  # Average rating
ggplot(rating, 
       aes(x=Genre, 
           y=avg_rate)) +
  geom_bar(aes(
    # Set the avg_rate as a categorical value, or a 'factor'
    fill = as.factor(avg_rate)), 
    stat='identity')+
  xlab("Genre")+
  ylab("Average rating (out of 5)")+
  # Limit y axes
  coord_cartesian(ylim = c(4, 5)) +
  theme_grey(base_size = 25) +
  labs(fill = "Average rating")
  