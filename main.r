# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)

# Load the dataset
data <- read_csv("stack_overflow_data.csv")

# Arrange data by year in descending order
# This step isn't strictly necessary, so it can be omitted if not used later.
data <- data %>% arrange(desc(year))

# Part 1: Analyze the popularity of R over time
# Filter data for the tag 'R', calculate the percentage of R questions per year
r_over_time <- data %>% 				
  filter(tag == "r") %>%
  mutate(percentage = 100 * num_questions / year_total) # Calculate yearly percentage of R questions
head(r_over_time)

# Part 2: Calculate the percentage of R-tagged questions in 2020
# Filter for the year 2020 and retrieve the percentage as a numeric variable
r_2020 <- r_over_time %>%
  filter(year == 2020)
r_percentage <- as.numeric(r_2020$percentage)
r_percentage

# Part 3: Identify top 5 programming languages by total number of questions (2016-2020)
highest_tag_name <- data %>% 
  filter(year >= 2016 & year <= 2020) %>%   # Focus on years 2016-2020
  group_by(tag) %>%
  summarise(total_questions = sum(num_questions)) %>%  # Sum questions per tag
  arrange(desc(total_questions)) %>%
  slice_head(n = 5) # Get top 5 tags
highest_tags <- unique(highest_tag_name$tag)
highest_tags

# Filter original data for these top 5 tags to use in visualization
top_tags_data <- data %>% filter(tag %in% highest_tags)

# Part 4: Find the tag with the largest year-over-year increase in its question percentage
b <- data %>% 
  group_by(year) %>% 
  mutate(total_each_year = sum(num_questions)) %>%  # Calculate total questions each year
  ungroup() %>%
  mutate(percentage = num_questions * 100 / total_each_year) %>%  # Calculate percentage per tag
  arrange(tag, year) %>%
  group_by(tag) %>%
  mutate(ratio = percentage / lag(percentage)) %>%  # Calculate year-over-year ratio
  ungroup() %>%
  slice_max(ratio, n = 1) # Select tag with the highest ratio
highest_ratio_tag <- b$tag
highest_ratio_tag

# Filter for SwiftUI tag to explore its trend over time
swiftui_data <- data %>% 
  group_by(year) %>% 
  mutate(total_each_year = sum(num_questions)) %>%
  ungroup() %>%
  mutate(percentage = num_questions * 100 / total_each_year) %>%
  filter(tag == "swiftui")

# Plot 1: Popularity of R Over Time
ggplot(r_over_time, aes(x = year, y = percentage)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Popularity of R Over Time", x = "Year", y = "Percentage of Questions") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(r_over_time$year), max(r_over_time$year), by = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Rotate year labels vertically

# Plot 2: Top 5 Programming Languages (2016-2020)
ggplot(top_tags_data, aes(x = year, y = num_questions, color = tag)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Top 5 Programming Languages (2016-2020)", x = "Year", y = "Number of Questions") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(top_tags_data$year), max(top_tags_data$year), by = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "right", legend.title = element_text(size = 10),  # Adjust legend title size
        legend.text = element_text(size = 9),  # Adjust legend label size
        legend.key.size = unit(0.5, "cm"))  # Legend background and border)  # Rotate year labels vertically

# Plot 3: Number of Questions for SwiftUI Over Time
ggplot(swiftui_data, aes(x = year, y = num_questions)) +
  geom_col(fill = "lightblue") +
  labs(title = "Number of Questions for SwiftUI Over Time", x = "Year", y = "Number of Questions") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(swiftui_data$year), max(swiftui_data$year), by = 1))
