library(tidyverse)

data_athletes = read_csv(file="./clean_athletes_data.csv")
data_hosts = read_csv(file="./2025_Problem_C_Data/summerOly_hosts.csv")
data_medal_counts = read_csv(file="./medal_counts.csv")
data_programs = read_csv(file="./clean_program_data.csv")

haveWonMedals = unique(data_medal_counts$NOC)
allCountries= unique(data_athletes$NOC)
haveNotWonMedals = setdiff(allCountries,haveWonMedals)

# Track the year of the first competition for every team

first_competition = data_athletes |>
  select(c(NOC,Year)) |>
  group_by(NOC) |>
  summarize(First_Olympics=min(Year))

# next want to find the years of the first medals for countries BUT not if it is also the first competition of the country

first_medal = data_medal_counts |>
  group_by(NOC) |>
  summarize(First_Medal_Year = min(Year))  # Get the first year a medal was won

first_medal$NumAthletes = NA

for (i in 1:nrow(first_medal)) {
  first_year = first_medal$First_Medal_Year[i]
  curr_NOC = first_medal$NOC[i]
  numAthletes = length(unique(data_athletes$Name[which(data_athletes$Year==first_year & data_athletes$NOC==curr_NOC)]))
  first_medal$NumAthletes[i] = numAthletes
}

first_medal = first_medal |>
  filter(row_number() %in% which(NumAthletes>0))

medAthletesFirstMedal = round(median(first_medal$NumAthletes)*.5)

##########################################################################

# Random forest to predict athlete counts

library(randomForest)

data_non_medalist = data_athletes |>
  filter(NOC %in% haveNotWonMedals)

# remove a few teams that won medals but changed their NOC
data_non_medalist = data_non_medalist |>
  filter(!NOC %in% c("AIN","EOR","FIJ","PAR","SLO"))

# Step 2: Aggregate athlete counts per country per year
country_athlete_counts = data_non_medalist |>
  group_by(NOC, Year) |>
  summarise(AthleteCount = n(), .groups = "drop")

# Step 3: Create growth trend features for prediction
country_athlete_counts = country_athlete_counts |>
  arrange(NOC, Year) |>
  group_by(NOC) |>
  mutate(AthleteGrowth = AthleteCount - lag(AthleteCount, default = 0),
         AthleteRate = AthleteCount / lag(AthleteCount, default = 1))

# Step 4: Create target variable for past data (athlete count exceeding median threshold)
country_athlete_counts = country_athlete_counts |>
  mutate(Target = ifelse(AthleteCount > medAthletesFirstMedal, 1, 0))

train_data = country_athlete_counts 

rf_model = randomForest(as.factor(Target) ~ Year + AthleteCount + AthleteGrowth + AthleteRate, 
                         data = train_data, 
                         ntree = 500, 
                         mtry = 2, 
                         importance = TRUE)


test_data = country_athlete_counts |>
  filter(Year == 2024 & NOC %in% haveNotWonMedals) |>
  mutate(Year = 2028)

test_data$Predicted = predict(rf_model, newdata = test_data, type = "response")

test_data = test_data |>
  mutate(PredictedWin = ifelse(Predicted == 1, "Likely to win first medal", "Unlikely to win first medal"))


medalTable = test_data |>
  filter(Predicted==1)|>
  select(NOC,AthleteCount,AthleteGrowth,AthleteRate,PredictedWin)

xtable(medalTable)
  
## Now change the constant that is scaling down the threshold and test

