library(tidyverse)

data_athletes = read_csv(file="./clean_athletes_data.csv")
data_hosts = read_csv(file="./2025_Problem_C_Data/summerOly_hosts.csv")
data_medal_counts = read_csv(file="./medal_counts.csv")
data_programs = read_csv(file="./clean_program_data.csv")

haveWonMedals = unique(data_medal_counts$NOC)
allCountries= unique(data_athletes$NOC)
haveNotWonMedals = setdiff(allCountries,haveWonMedals)

# Track the year of the first medal of every team that has won a medal

first_competition = data_athletes |>
  select(c(NOC,Year)) |>
  group_by(NOC) |>
  summarize(First_Olympics=min(Year))

# next want to find the years of the first medals for countries BUT not if it is also the first competition of the country

first_medal <- data_medal_counts %>%
  filter(Total > 0) %>%  # Filter only countries that have won at least one medal
  group_by(NOC) %>%
  summarize(First_Medal_Year = min(Year))  # Get the first year a medal was won

first_medal$NumAthletes = NA

for (i in 1:nrow(first_medal)) {
  first_year <- first_medal$First_Medal_Year[i]
  curr_NOC = first_medal$NOC[i]
  numAthletes = length(unique(data_athletes$Name[which(data_athletes$Year==first_year & data_athletes$NOC==curr_NOC)]))
  first_medal$NumAthletes[i] = numAthletes
}

