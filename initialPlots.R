library(tidyverse)
library(ggplot2)

data_athletes = read_csv(file="./clean_athletes_data.csv")
data_hosts = read_csv(file="./2025_Problem_C_Data/summerOly_hosts.csv")
data_medal_counts = read_csv(file="./medal_counts.csv")
data_programs = read_csv(file="./clean_program_data.csv")

# Plot number of competing athletes and countries over year

competing_countries = data_athletes |>
  group_by(Year) |>
  summarise(numCountries = n_distinct(NOC), .groups = "drop")

competing_athletes = data_athletes |>
  select(NOC,Year) |>
  group_by(Year) |>
  summarize(numAthletes=n())

ggplot(competing_countries, aes(x = Year, y = numCountries)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Number of Competing Countries Over Time",
       x = "Year",
       y = "Number of Countries") +
  scale_x_continuous(breaks = seq(1920, 2024, by = 8), limits = c(1920, 2024)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  geom_hline(yintercept = 0)

ggplot(competing_athletes, aes(x = Year, y = numAthletes)) +
  geom_col(fill = "steelblue", width = 0.7) +
  labs(title = "Number of Competing Athletes Over Time",
       x = "Year",
       y = "Number of Athletes") +
  scale_x_continuous(breaks = seq(1920, 2024, by = 8), limits = c(1919, 2025)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  geom_hline(yintercept = 0)

