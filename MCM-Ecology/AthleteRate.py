# Reloading necessary files after reset
import pandas as pd

# File paths
athletes_data_path = 'clean_athletes_data.csv'
program_data_path = 'clean_program_data.csv'
medal_counts_path = 'medal_counts.csv'

# Reading the datasets
athletes_data = pd.read_csv(athletes_data_path)
program_data = pd.read_csv(program_data_path)
medal_counts = pd.read_csv(medal_counts_path)

# Reapplying the logic for adjusted scores considering experience and gender

# Step 1: Add participation count (experience) for each athlete
# Group by athlete name and count the number of unique years they participated in
athletes_data["Participation_Count"] = athletes_data.groupby("Name")["Year"].transform("nunique")

# Step 2: Define an experience curve based on a quadratic function
# Assumed coefficients of the experience curve: P(e) = alpha * e - beta * e^2 + gamma
alpha, beta, gamma = 0.5, 0.1, 1.0

# Calculate experience-based score adjustment
athletes_data["Experience_Score"] = (
    alpha * athletes_data["Participation_Count"]
    - beta * athletes_data["Participation_Count"] ** 2
    + gamma
)

# Ensure no negative scores (if the quadratic model generates negative values)
athletes_data["Experience_Score"] = athletes_data["Experience_Score"].clip(lower=0)

# Step 3: Adjust scores based on gender
# Example: Assuming a slight adjustment based on gender
gender_score_map = {"M": 1.05, "F": 1.0}  # Male athletes might get a slight bonus
athletes_data["Gender_Score"] = athletes_data["Sex"].map(gender_score_map)

# Step 4: Recalculate the overall score with updated components
# Incorporate experience and gender into the scoring model
w1, w2, w3, w4, w5 = 1/5, 1/5, 1/5, 1/5, 1/5  # Equal initial weights for all components
athletes_data["Adjusted_Score"] = (
    w1 * (1 / (athletes_data["Medal"].map({"Gold": 3, "Silver": 2, "Bronze": 1, 0: 0}) + 1)) +  # Performance score
    w2 * athletes_data["Medal"].map({"Gold": 3, "Silver": 2, "Bronze": 1, 0: 0}) +              # Medal score
    w3 * 1 +              # Placeholder for event importance
    w4 * athletes_data["Experience_Score"] +         # Experience adjustment
    w5 * athletes_data["Gender_Score"]               # Gender adjustment
)

# Step 5: Aggregate scores at the country and year level
final_adjusted_scores = athletes_data.groupby(["NOC", "Year"])["Adjusted_Score"].sum().reset_index()
final_adjusted_scores = final_adjusted_scores.rename(columns={"Adjusted_Score": "Total_Adjusted_Score"})

# Display the adjusted scores for validation
final_adjusted_scores.head()