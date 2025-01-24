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
  
##############################

