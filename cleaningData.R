library(tidyverse)
library(ggplot2)

data_athletes_raw = read_csv(file="./2025_Problem_C_Data/summerOly_athletes.csv")
data_hosts_raw = read_csv(file="./2025_Problem_C_Data/summerOly_hosts.csv")
data_medal_counts_raw = read_csv(file="./2025_Problem_C_Data/summerOly_medal_counts.csv")
data_programs_raw = read_csv(file="./2025_Problem_C_Data/summerOly_programs.csv")


## Cleaning data_programs

# Replacing NAs and non numericals with 0

#data_programs = data_programs_raw

data_programs$Sport[c(9,10)]="Baseball and Softball"
data_programs$Discipline[45]="Modern Pentathlon"
data_programs$Discipline[66]="Water Motorsports"
data_programs$Code[48]="ROQUE"
data_programs$Code[40]="JUEDEPAUME"
data_programs$'1896'[c(50,53)]=0
for (colname in colnames(data_programs)) {
  # Replace weird characters with 0
  data_programs[[colname]][data_programs[[colname]] == "\x95"] <- 0
  
  # Remove NAs if needed (e.g., replace with 0 or other strategy)
  data_programs[[colname]][is.na(data_programs[[colname]])] <- 0
}

# Remove skating and ice hockey

data_programs = data_programs |>
  filter(!row_number() %in% c(70,71,72,73,74)) |>
  select(-`Sports Governing Body`)

# Remove data from before 1914 (1912 Olympics and earlier)

data_programs = data_programs |>
  select(-c('1896','1900','1904','1906*','1908','1912'))

# write.csv(data_programs,"clean_program_data.csv")
  
############################################################

## Cleaning athlete data

# data_athletes=data_athletes_raw

data_athletes = data_athletes |>
  select(-Team)

data_athletes = data_athletes |>
  mutate(Medal = recode(Medal, 
                        "No medal" = 0, 
                        "Bronze" = 1, 
                        "Silver" = 2, 
                        "Gold" = 3))

data_athletes = data_athletes |>
  filter(!row_number() %in% which(Year<1914))

# write.csv(data_athletes,"clean_athletes_data.csv")

############################################################

## Cleaning medal count data

data_medal_counts = data_medal_counts |>
  mutate(NOC = str_trim(NOC))

data_medal_counts = data_medal_counts |>
  filter(!row_number() %in% which(Year<1914))

#write.csv(data_medal_counts,"medal_counts.csv")


############################################################

## Get a list of all countries that participated in 2024

countries2024 = unique(data_athletes$NOC[which(data_athletes$Year==2024)])

#write.csv(countries2024,"countries2024.csv")

