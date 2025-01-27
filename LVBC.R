library(tidyverse)

data_athletes = read_csv(file="./clean_athletes_data.csv")
data_scores = read_csv(file="./2024_actual.csv")
data_pred = read_csv(file="./2028_predictions.csv")
data_pred$NOC[which(data_pred$NOC=="AIN")]="RUS"

std_scores = rep(NA,length(data_scores$NOC))

for(i in 1:length(data_scores$NOC)){
  curr_NOC = data_scores$NOC[i]
  curr_Year = data_scores$Year[i]
  numAthletes = length(unique(data_athletes$Name[which(data_athletes$Year==curr_Year & data_athletes$NOC==curr_NOC)]))
  std_scores[i] = data_scores$Total_Final_Score[i] / numAthletes
}
# russia 2020
std_scores[2414] = data_scores$Total_Final_Score[2414] / 341

data_scores = data_scores |>
  mutate(standardized = std_scores)

innovationP = tibble("NOC"=unique(data_pred$NOC),
                     p=rep(NA,length(unique(data_pred$NOC))))

for(i in 1:length(innovationP$NOC)){
  curr_NOC = innovationP$NOC[i]
  maxS = max(data_scores$standardized[which(data_scores$NOC==curr_NOC)])
  diff = data_scores$standardized[which(data_scores$NOC==curr_NOC & data_scores$Year==2024)] -
         data_scores$standardized[which(data_scores$NOC==curr_NOC & data_scores$Year==2020)]
  if(length(diff)==0){
    diff = 0.1*maxS
  }
  innovationP$p[i] = diff/maxS
}

std28 = rep(NA,length(data_pred$NOC))
data_pred$Predicted_Score[which(data_pred$Predicted_Score<0)]=0
# use 2024 athlete counts to standardize
for(i in 1:length(std28)){
  curr_NOC=data_pred$NOC[i]
  numAthletes = length(unique(data_athletes$Name[which(data_athletes$NOC==curr_NOC & data_athletes$Year==2024)]))
  std28[i]=data_pred$Predicted_Score[i]/numAthletes
}
data_pred = data_pred |>
  mutate(standardized = std28)

competitionMatrix = matrix(nrow=length(data_pred$standardized),ncol=length(data_pred$standardized))
colnames(competitionMatrix) = data_pred$NOC
rownames(competitionMatrix) = data_pred$NOC
for(row in data_pred$NOC){
  for(col in data_pred$NOC){
    if(row==col){
      competitionMatrix[row,col]=0
    } else{
    
      rowS = data_pred$standardized[which(data_pred$NOC==row)]
      colS = data_pred$standardized[which(data_pred$NOC==col)]
      
      competitionMatrix[row,col]=1/(abs(rowS-colS)+1)
    }
  }
}

carrying_capacity = tibble("NOC"=data_pred$NOC,
                           "K"=rep(NA,length(data_pred$NOC)))


carrying_capacity <- data_athletes |>
  filter(Year == 2024) |>                  # Filter for the year 2024
  filter(Medal > 0) |>                      # Exclude "No medal" entries
  select(NOC, Event, Medal) |>               # Select relevant columns
  distinct(NOC, Event, .keep_all = TRUE) |>   # Remove duplicate NOC-Event pairs
  group_by(NOC) |>                            # Group by NOC
  summarize(K = n())     

carrying_capacity$K=round(1.4*(carrying_capacity$K+1))
for(i in 1:length(carrying_capacity$K)){
  carrying_capacity$K[i]=max(10,carrying_capacity$K[i])
}

# not have any p=0
innovationP$p[which(innovationP$p==0)]=0.01


#######################################
# library(deSolve)
# library(dplyr)
# 
# # Given data
# total_gold <- 339
# total_silver <- 338
# total_bronze <- 395
# 
# # Set imitation coefficient q (smaller to avoid stagnation)
# q_value <- 200
# 
# # Prepare initial conditions
# NOCs <- innovationP$NOC
# p_values <- setNames(innovationP$p, innovationP$NOC)
# K_values <- setNames(carrying_capacity$K, carrying_capacity$NOC)
# 
# # Ensure no zero or negative K values
# if(any(K_values <= 0)) stop("Error: Carrying capacities must be greater than zero.")
# 
# competition_matrix <- as.matrix(competitionMatrix[NOCs, NOCs])
# 
# # Initial values: Start with a small number instead of zero
# initial_R <- setNames(rep(1, length(NOCs)), NOCs)  
# 
# # Define the Adapted Lotka-Volterra-Bass Competition model
# alvbc_model <- function(t, R, params) {
#   p <- params$p
#   K <- params$K
#   alpha <- params$alpha
#   q <- params$q
#   
#   dRdt <- numeric(length(R))
#   
#   for (i in seq_along(R)) {
#     innovation <- p[i] * (K[i] - R[i])
#     imitation <- q * (R[i] / K[i]) * (K[i] - R[i])
#     competition <- sum(alpha[i, -i] * R[i] * R[-i])
#     
#     dRdt[i] <- innovation + imitation - competition
#   }
#   return(list(dRdt))
# }
# 
# # Parameters for the ODE model
# params <- list(p = p_values, K = K_values, alpha = competition_matrix, q = q_value)
# 
# # Time steps for simulation
# n_ticks <- 100
# time <- seq(0, n_ticks, by = 1)
# 
# # Run the ODE solver
# ode_solution <- ode(y = initial_R, times = time, func = alvbc_model, parms = params,method="lsoda")
# 
# # Print solution to debug
# #print(head(ode_solution))
# 
# # Extract final medal count predictions
# predicted_medals <- as.data.frame(ode_solution[n_ticks + 1, -1])
# colnames(predicted_medals) <- NOCs
# predicted_medals <- t(predicted_medals)  
# predicted_medals <- data.frame(NOC = rownames(predicted_medals), Medal_Count = as.numeric(predicted_medals))
# 
# # Allocate medals by ranking
# predicted_medals <- predicted_medals |> arrange(desc(Medal_Count))



##############################################################3




library(deSolve)
library(dplyr)

# Given total medal counts
total_gold <- 339
total_silver <- 338
total_bronze <- 395

# Set imitation coefficient q (adjust for test)
q_value <- 10000

# Select a subset of countries for testing
selected_NOCs <- c("USA", "CHN", "GBR", "GER", "FRA", "JPN", "AUS", 
                   "NED", "KOR", "ITA", "NZL", "CAN", "BRA", "HUN", "ESP")

# Filter data for selected countries
innovationP_subset <- innovationP %>% filter(NOC %in% selected_NOCs)
competitionMatrix_subset <- competitionMatrix[selected_NOCs, selected_NOCs]
carrying_capacity_subset <- carrying_capacity %>% filter(NOC %in% selected_NOCs)

# Prepare parameters for the subset
NOCs <- innovationP_subset$NOC
p_values <- setNames(innovationP_subset$p, innovationP_subset$NOC)
K_values <- setNames(carrying_capacity_subset$K, carrying_capacity_subset$NOC)

# Ensure no zero or negative K values
if (any(K_values <= 0)) stop("Error: Carrying capacities must be greater than zero.")

# Initial values: Start with a small number instead of zero
initial_R <- setNames(rep(1, length(NOCs)), NOCs)

# Define the Adapted Lotka-Volterra-Bass Competition model
alvbc_model <- function(t, R, params) {
  p <- params$p
  K <- params$K
  alpha <- params$alpha
  q <- params$q
  
  dRdt <- numeric(length(R))
  
  for (i in seq_along(R)) {
    innovation <- p[i] * (K[i] - R[i])
    imitation <- q * (R[i] / K[i]) * (K[i] - R[i])
    competition <- sum(alpha[i, -i] * R[i] * R[-i])
    
    dRdt[i] <- innovation + imitation - competition
  }
  return(list(dRdt))
}

# Parameters for the ODE model
params <- list(p = p_values, K = K_values, alpha = competitionMatrix_subset, q = q_value)

# Time steps for simulation
n_ticks <- 100
time <- seq(0, n_ticks, by = 1)

# Run the ODE solver
ode_solution <- ode(y = initial_R, times = time, func = alvbc_model, parms = params, method = "lsoda")

# Extract the final row of results
final_row <- ode_solution[nrow(ode_solution), -1]

# Ensure the number of columns matches the number of countries (NOCs)
if (length(final_row) != length(NOCs)) {
  stop("Error: Mismatch between predicted results and number of countries.")
}

# Convert to data frame with proper column names
predicted_medals <- data.frame(NOC = NOCs, Medal_Count = as.numeric(final_row))

# Sort the results in descending order of predicted medals
predicted_medals <- predicted_medals |> arrange(desc(Medal_Count))

# Incorporate historical proportions to allocate medals
recent_medals <- medal_counts %>% filter(Year %in% c(2020, 2024))

medal_proportions <- recent_medals %>%
  group_by(NOC) %>%
  summarise(
    Total = sum(Gold + Silver + Bronze),
    Gold_Prop = sum(Gold) / Total,
    Silver_Prop = sum(Silver) / Total,
    Bronze_Prop = sum(Bronze) / Total
  ) %>%
  replace_na(list(Gold_Prop = 0, Silver_Prop = 0, Bronze_Prop = 0))

# Merge with predicted medal counts
predicted_medals <- predicted_medals %>%
  left_join(medal_proportions, by = "NOC") %>%
  mutate(
    Gold = round(Medal_Count * Gold_Prop),
    Silver = round(Medal_Count * Silver_Prop),
    Bronze = round(Medal_Count * Bronze_Prop)
  )

# Adjust to match total medals
predicted_medals$Gold <- round(predicted_medals$Gold * total_gold / sum(predicted_medals$Gold))
predicted_medals$Silver <- round(predicted_medals$Silver * total_silver / sum(predicted_medals$Silver))
predicted_medals$Bronze <- round(predicted_medals$Bronze * total_bronze / sum(predicted_medals$Bronze))

# Initialize matrices to store simulation results (columns = simulations, rows = countries)
gold_results_matrix <- matrix(NA, nrow = length(NOCs), ncol = num_simulations)
silver_results_matrix <- matrix(NA, nrow = length(NOCs), ncol = num_simulations)
bronze_results_matrix <- matrix(NA, nrow = length(NOCs), ncol = num_simulations)

# Monte Carlo simulations for uncertainty
num_simulations <- 1000

# Initialize matrices to store simulation results (columns = simulations, rows = countries)
# Sorting and ensuring consistency
predicted_medals <- predicted_medals %>% arrange(NOC)
medal_proportions <- medal_proportions %>% arrange(NOC)

# Initialize matrices to store simulation results (columns = simulations, rows = countries)
gold_results_matrix <- matrix(NA, nrow = length(NOCs), ncol = num_simulations)
silver_results_matrix <- matrix(NA, nrow = length(NOCs), ncol = num_simulations)
bronze_results_matrix <- matrix(NA, nrow = length(NOCs), ncol = num_simulations)

# Monte Carlo simulations for uncertainty
for (i in 1:num_simulations) {
  # Add noise to q and p values
  noisy_q <- q_value + rnorm(1, mean = 0, sd = 1000)  # Noise for q
  noisy_p_values <- p_values + rnorm(length(p_values), mean = 0, sd = 0.2)  # Noise for p values
  noisy_K_values = round(K_values*(rnorm(length(K_values), mean=1.03, sd=0.1)))
  
  # Re-run the model with noisy parameters
  params_noisy <- list(p = noisy_p_values, K = noisy_K_values, alpha = competitionMatrix_subset, q = noisy_q)
  
  # Run the ODE solver with noisy parameters
  ode_solution_noisy <- ode(y = initial_R, times = time, func = alvbc_model, parms = params_noisy, method = "lsoda")
  
  # Extract the final row of results from the noisy simulation
  final_row_noisy <- ode_solution_noisy[nrow(ode_solution_noisy), -1]
  
  # Create a data frame with the results, sorted by NOC
  predicted_medals_noisy <- data.frame(NOC = NOCs, Medal_Count = as.numeric(final_row_noisy))
  predicted_medals_noisy <- predicted_medals_noisy %>% arrange(NOC)  # Ensure order matches NOCs
  
  # Allocate medals based on the proportions (Gold_Prop, Silver_Prop, Bronze_Prop)
  predicted_medals_noisy <- predicted_medals_noisy %>%
    left_join(medal_proportions, by = "NOC") %>%
    mutate(
      Gold_Allocated = round(Medal_Count * Gold_Prop),
      Silver_Allocated = round(Medal_Count * Silver_Prop),
      Bronze_Allocated = round(Medal_Count * Bronze_Prop)
    )
  
  # Store the results in the respective matrices (Gold, Silver, Bronze)
  gold_results_matrix[, i] <- predicted_medals_noisy$Gold_Allocated
  silver_results_matrix[, i] <- predicted_medals_noisy$Silver_Allocated
  bronze_results_matrix[, i] <- predicted_medals_noisy$Bronze_Allocated
}

# Calculate the standard deviation of the simulated results across all simulations
gold_sd <- apply(gold_results_matrix, 1, sd, na.rm = TRUE)
silver_sd <- apply(silver_results_matrix, 1, sd, na.rm = TRUE)
bronze_sd <- apply(bronze_results_matrix, 1, sd, na.rm = TRUE)

# Combine the results into one final data frame
final_results <- data.frame(
  NOC = NOCs,
  Gold = rowMeans(gold_results_matrix, na.rm = TRUE),
  Gold_SD = gold_sd,
  Silver = rowMeans(silver_results_matrix, na.rm = TRUE),
  Silver_SD = silver_sd,
  Bronze = rowMeans(bronze_results_matrix, na.rm = TRUE),
  Bronze_SD = bronze_sd
)

# Ensure that the final results are sorted by NOC
final_results <- final_results %>% arrange(NOC)

# Print final results with uncertainty
print(final_results)

##################################################


# Round the results to the nearest integer
final_results_rounded <- final_results %>%
  mutate(
    Gold = round(Gold),
    Gold_SD = round(Gold_SD),
    Silver = round(Silver),
    Silver_SD = round(Silver_SD),
    Bronze = round(Bronze),
    Bronze_SD = round(Bronze_SD),
    Total_Medals = round(Gold + Silver + Bronze),
    Total_SD = round(Gold_SD + Silver_SD + Bronze_SD)
  ) %>%
  # Order by Total_Medals in descending order
  arrange(desc(Total_Medals))

# Convert to LaTeX table format without .00 decimals
latex_table <- xtable(final_results_rounded, 
                      caption = "Medal Allocation with Uncertainty", 
                      label = "tab:medals")

# Print the LaTeX table (this will display it in LaTeX code)
print(latex_table, type = "latex", include.rownames = FALSE, 
      booktabs = TRUE, 
      sanitize.colnames.function = function(x) {x},
      format.args = list(big.mark = ""))  # Ensure numbers are whole
