library(tidyverse)

data_athletes = read_csv(file="./clean_athletes_data.csv")
data_hosts = read_csv(file="./2025_Problem_C_Data/summerOly_hosts.csv")
data_medal_counts = read_csv(file="./medal_counts.csv")
data_programs = read_csv(file="./clean_program_data.csv")

top_10_countries <- data_medal_counts %>%
  filter(Rank <= 10) %>%
  filter(Year>1950)|>
  group_by(NOC) %>%  # Group by country code (NOC)
  summarise(top_10_count = n_distinct(Year)) %>%  # Count how many distinct years each country ranked in top 10
  filter(top_10_count >= 5)  # Keep countries that have ranked in the top 10 in at least 5 Olympics

# Get the list of countries that ranked in top 10 in at least 5 Olympics
countries_in_top_10_5_or_more <- top_10_countries$NOC
