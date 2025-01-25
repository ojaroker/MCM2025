library(tidyverse)

data_athletes = read_csv(file="./clean_athletes_data.csv")
data_hosts = read_csv(file="./2025_Problem_C_Data/summerOly_hosts.csv")
data_medal_counts = read_csv(file="./medal_counts.csv")
data_programs = read_csv(file="./clean_program_data.csv")

validCountries = data_medal_counts |>
  filter(Year==2024)|>
  select(NOC)

top_10_countries = data_medal_counts |>
  filter(Rank <= 10) |>
  filter(Year>1950)|>
  filter(NOC %in% validCountries$NOC)|>
  group_by(NOC) |>  
  summarise(top_10_count = n_distinct(Year)) |>  
  filter(top_10_count >= 5) 

# Get the list of countries that ranked in top 10 in at least 5 Olympics
countries_in_top_10_5_or_more = top_10_countries$NOC

# Now want to consider teams that have not won a medal in any single sport in the past 7 Olympics

# Define the last 7 Olympic years
recent_olympics <- c(2024, 2020, 2016, 2012, 2008, 2004, 2000)

# Filter the athlete data for relevant countries and recent Olympics
recent_medals <- data_athletes %>%
  filter(NOC %in% countries_in_top_10_5_or_more & Year %in% recent_olympics)

# Identify countries and sports where medals were won
medal_wins <- recent_medals %>%
  filter(Medal > 0) %>%  # Consider only cases where medals were won
  group_by(NOC, Sport) %>%
  summarise(HasMedal = n(), .groups = "drop")

# Find countries and sports with no medals in the past 7 Olympics
countries_sports_no_medals <- recent_medals %>%
  anti_join(medal_wins, by = c("NOC", "Sport")) %>%  # Exclude medal-winning records
  distinct(NOC, Sport)

teamSports <- c("Basketball", "Water Polo", "Synchronized Swimming", "Softball", "Hockey", 
                "Football", "Volleyball", "Beach Volleyball", "Handball", "Rowing", "Rugby Sevens",
                "Artistic Swimming", "Baseball", "Baseball/Softball", "3x3 Basketball", "3x3 Basketball, Basketball")

countries_with_no_team_sport_medals <- countries_sports_no_medals %>%
  filter(Sport %in% teamSports) %>%  # Keep only team sports
  group_by(NOC) %>%
  summarise(team_sports_without_medals = n_distinct(Sport), .groups = "drop") %>%
  filter(team_sports_without_medals > 0) %>%  # Ensure they have missed medals in at least one team sport
  pull(NOC)

team_sports_no_medals_by_country <- countries_sports_no_medals %>%
  filter(NOC %in% c("USA","JPN","GBR"), Sport %in% teamSports) %>%
  group_by(NOC) %>%
  summarise(team_sports_without_medals = list(unique(Sport)), .groups = "drop")
# Get the list of team sports where these countries have not won medals
team_sports_no_medals_list <- countries_sports_no_medals %>%
  filter(NOC %in% countries_with_no_team_sport_medals, Sport %in% teamSports) %>%
  distinct(Sport) %>%
  pull(Sport)

usa_no_medals <- team_sports_no_medals_by_country %>%
  filter(NOC == "USA") %>%
  pull(team_sports_without_medals)

jpn_no_medals <- team_sports_no_medals_by_country %>%
  filter(NOC == "JPN") %>%
  pull(team_sports_without_medals)

gbr_no_medals <- team_sports_no_medals_by_country %>%
  filter(NOC == "GBR") %>%
  pull(team_sports_without_medals)





