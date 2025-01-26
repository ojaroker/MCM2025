import pandas as pd

# Load the uploaded CSV file to inspect its structure
file_path = 'summerOly_medal_counts.csv'
data = pd.read_csv(file_path)

# Display the first few rows of the dataset to understand its structure
data.head()

# Group the data by Year and NOC to compute total medals for each country in each Olympic year
grouped_data = data.groupby(['Year', 'NOC']).agg({
    'Gold': 'sum',
    'Silver': 'sum',
    'Bronze': 'sum',
    'Total': 'sum'
}).reset_index()

# Calculate the total medal pool for each year
total_medal_pool = grouped_data.groupby('Year')['Total'].sum().reset_index()
total_medal_pool.rename(columns={'Total': 'Medal_Pool'}, inplace=True)

# Merge the total medal pool back into the main dataset for normalization or analysis
grouped_data = grouped_data.merge(total_medal_pool, on='Year')

# Display the processed data
grouped_data.head()

import numpy as np
from scipy.integrate import odeint
import matplotlib.pyplot as plt

# Select a specific year for the simulation
year_to_model = 1896
year_data = grouped_data[grouped_data['Year'] == year_to_model]

# Extract initial conditions
x0 = year_data['Medal_Pool'].iloc[0]  # Total medal pool for the year
y0 = year_data['Total'].values        # Medal counts for each country in that year
n_countries = len(y0)                 # Number of countries

# Define parameters (these can be calibrated later)
a = 0.01  # Growth rate of the medal pool
b = np.random.uniform(0.0001, 0.001, size=n_countries)  # Competition coefficients
c = np.random.uniform(0.01, 0.05, size=n_countries)     # Decline rates
d = np.random.uniform(0.0005, 0.002, size=n_countries)  # Success rates

# Define the generalized Lotka-Volterra equations
def generalized_lv(state, t, a, b, c, d):
    x = state[0]  # Medal pool
    y = state[1:]  # Medals won by each country
    n = len(y)
    dxdt = a * x - np.sum([b[i] * x * y[i] for i in range(n)])  # Medal pool dynamics
    dydt = [-c[i] * y[i] + d[i] * x * y[i] for i in range(n)]  # Country medal dynamics
    return [dxdt] + dydt

# Initial state (medal pool and medal counts for countries)
initial_state = [x0] + list(y0)

# Time points (e.g., simulate 10 Olympic cycles)
t = np.linspace(0, 10, 100)

# Solve the differential equations
result = odeint(generalized_lv, initial_state, t, args=(a, b, c, d))

# Extract results
x = result[:, 0]  # Medal pool over time
y = result[:, 1:]  # Medal counts for each country

# Plot results
plt.figure(figsize=(12, 6))
for i, country in enumerate(year_data['NOC'].values):
    plt.plot(t, y[:, i], label=f"{country} Medals")
plt.plot(t, x, label="Total Medal Pool", linestyle="--", color="black")
plt.xlabel("Time (Olympic cycles)")
plt.ylabel("Medals")
plt.title(f"Medal Dynamics for {year_to_model}")
plt.legend()
plt.show()
