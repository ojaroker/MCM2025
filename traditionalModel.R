library(tidyverse)
library(ggplot2)

olympic_teams = c(
  "United States" = "USA", "Sweden" = "SWE", "Finland" = "FIN", "Great Britain" = "GBR",
  "Belgium" = "BEL", "Norway" = "NOR", "Italy" = "ITA", "France" = "FRA",
  "Netherlands" = "NED", "Denmark" = "DEN", "South Africa" = "RSA", "Canada" = "CAN",
  "Switzerland" = "SUI", "Estonia" = "EST", "Brazil" = "BRA", "Australia" = "AUS",
  "Japan" = "JPN", "Spain" = "ESP", "Greece" = "GRE", "Luxembourg" = "LUX",
  "Czechoslovakia" = "TCH", "New Zealand" = "NZL", "Hungary" = "HUN", "Yugoslavia" = "YUG",
  "Argentina" = "ARG", "Uruguay" = "URU", "Austria" = "AUT", "Poland" = "POL",
  "Haiti" = "HAI", "Portugal" = "POR", "Romania" = "ROU", "Germany" = "GER",
  "Egypt" = "EGY", "India" = "IND", "Ireland" = "IRL", "Chile" = "CHL",
  "Philippines" = "PHI", "Mexico" = "MEX", "Latvia" = "LAT", "Turkey" = "TUR",
  "Jamaica" = "JAM", "Peru" = "PER", "Ceylon" = "CEY", "Cuba" = "CUB",
  "Trinidad and Tobago" = "TTO", "Panama" = "PAN", "South Korea" = "KOR", "Iran" = "IRI",
  "Puerto Rico" = "PUR", "Soviet Union" = "URS", "Lebanon" = "LBN", "Bulgaria" = "BUL",
  "Venezuela" = "VEN", "United Team of Germany" = "GDR", "Iceland" = "ISL", "Pakistan" = "PAK",
  "Bahamas" = "BAH", "Ethiopia" = "ETH", "Formosa" = "TPE", "Ghana" = "GHA",
  "Morocco" = "MAR", "Singapore" = "SGP", "British West Indies" = "BWI", "Iraq" = "IRQ",
  "Tunisia" = "TUN", "Kenya" = "KEN", "Nigeria" = "NGR", "East Germany" = "GDR",
  "West Germany" = "FRG", "Mongolia" = "MGL", "Uganda" = "UGA", "Cameroon" = "CMR",
  "Taiwan" = "TPE", "North Korea" = "PRK", "Colombia" = "COL", "Niger" = "NER",
  "Bermuda" = "BER", "Thailand" = "THA", "Zimbabwe" = "ZIM", "Tanzania" = "TAN",
  "Guyana" = "GUY", "China" = "CHN", "Ivory Coast" = "CIV", "Syria" = "SYR",
  "Algeria" = "ALG", "Chinese Taipei" = "TPE", "Dominican Republic" = "DOM", "Zambia" = "ZAM",
  "Suriname" = "SUR", "Costa Rica" = "CRC", "Indonesia" = "INA", "Netherlands Antilles" = "ANT",
  "Senegal" = "SEN", "Virgin Islands" = "ISV", "Djibouti" = "DJI", "Unified Team" = "EUN",
  "Lithuania" = "LTU", "Namibia" = "NAM", "Croatia" = "CRO", "Independent Olympic Participants" = "IOP",
  "Israel" = "ISR", "Slovenia" = "SVN", "Malaysia" = "MAS", "Qatar" = "QAT",
  "Russia" = "RUS", "Ukraine" = "UKR", "Czech Republic" = "CZE", "Kazakhstan" = "KAZ",
  "Belarus" = "BLR", "FR Yugoslavia" = "YUG", "Slovakia" = "SVK", "Armenia" = "ARM",
  "Burundi" = "BDI", "Ecuador" = "ECU", "Hong Kong" = "HKG", "Moldova" = "MDA",
  "Uzbekistan" = "UZB", "Azerbaijan" = "AZE", "Tonga" = "TON", "Georgia" = "GEO",
  "Mozambique" = "MOZ", "Saudi Arabia" = "KSA", "Sri Lanka" = "SRI", "Vietnam" = "VIE",
  "Barbados" = "BRB", "Kuwait" = "KWT", "Kyrgyzstan" = "KGZ", "Macedonia" = "MKD",
  "United Arab Emirates" = "ARE", "Serbia and Montenegro" = "SCG", "Paraguay" = "PRY", "Eritrea" = "ERI",
  "Serbia" = "SRB", "Tajikistan" = "TJK", "Samoa" = "SAM", "Sudan" = "SDN",
  "Afghanistan" = "AFG", "Mauritius" = "MRI", "Togo" = "TGO", "Bahrain" = "BHR",
  "Grenada" = "GRD", "Botswana" = "BWA", "Cyprus" = "CYP", "Gabon" = "GAB",
  "Guatemala" = "GTM", "Montenegro" = "MNE", "Independent Olympic Athletes" = "IOA", "Fiji" = "FJI",
  "Jordan" = "JOR", "Kosovo" = "KOS", "ROC" = "ROC", "San Marino" = "SMR",
  "North Macedonia" = "MKD", "Turkmenistan" = "TKM", "Burkina Faso" = "BFA", "Saint Lucia" = "LCA",
  "Dominica" = "DMA", "Albania" = "ALB", "Cabo Verde" = "CPV", "Refugee Olympic Team" = "ROT"
)

data_medal_counts = data_medal_counts |>
  mutate(NOC = olympic_teams[NOC])


# track the number olympics rather than year
data_medal_counts = data_medal_counts |>
  mutate(Olympics = Year/4-480)

# Linear Regression

countries = data_medal_counts$NOC[which(data_medal_counts$Olympics==26)]

gold_model = lm(Gold ~ Olympics + NOC, data = data_medal_counts)
total_model = lm(Total ~ Olympics + NOC, data = data_medal_counts)

predict_2028 = tibble(NOC = countries, Olympics = 27)

predict_2028$Gold_Predicted = predict(gold_model, newdata = predict_2028)
predict_2028$Total_Predicted = predict(total_model, newdata = predict_2028)



predict_2028 = predict_2028 |>
  mutate(Gold_Predicted = round(Gold_Predicted),
         Total_Predicted = round(Total_Predicted))


gold_predictions <- predict(gold_model, newdata = predict_2028, interval = "prediction", level = 0.9)
total_predictions <- predict(total_model, newdata = predict_2028, interval = "prediction", level = 0.9)


predict_2028 <- predict_2028 |> 
  mutate(Gold_Predicted = round(gold_predictions[, "fit"]),
         Gold_Lower = round(gold_predictions[, "lwr"]),
         Gold_Upper = round(gold_predictions[, "upr"]),
         Total_Predicted = round(total_predictions[, "fit"]),
         Total_Lower = round(total_predictions[, "lwr"]),
         Total_Upper = round(total_predictions[, "upr"]))

predict_2028 <- predict_2028 |> 
  mutate(Gold_Predicted = pmax(Gold_Predicted, 0),
         Gold_Lower = pmax(Gold_Lower, 0),
         Gold_Upper = pmax(Gold_Upper, 0),
         Total_Predicted = pmax(Total_Predicted, 0),
         Total_Lower = pmax(Total_Lower, 0),
         Total_Upper = pmax(Total_Upper, 0))


top_10_countries <- predict_2028 |> 
  arrange(desc(Total_Predicted)) |> 
  slice_head(n = 10)

noc_to_country <- c(
  "USA" = "United States", 
  "CHN" = "China", 
  "GER" = "Germany", 
  "GBR" = "Great Britain", 
  "FRA" = "France", 
  "ITA" = "Italy", 
  "JPN" = "Japan", 
  "AUS" = "Australia", 
  "HUN" = "Hungary", 
  "KOR" = "South Korea"
)


top_10_countries <- top_10_countries |> 
  mutate(Country = noc_to_country[NOC])



# Plot for total medals
ggplot(top_10_countries, aes(x = reorder(Country, Total_Predicted), 
                             y = Total_Predicted)) +
  geom_point(size = 4, color = "steelblue") +  # Dot for Total Medals
  geom_errorbar(aes(ymin = Total_Lower, ymax = Total_Upper), 
                width = 0.4, color = "black", alpha = 0.3) +  # Shaded error bars
  labs(title = "Predicted Total Medals for 2028 Olympics", x = "Country", y = "Total Medals",subtitle = "90 Percent Confidence Intervals Shown") +
  theme_bw() +
  coord_flip()

# Plot for gold medals
ggplot(top_10_countries, aes(x = reorder(Country, Gold_Predicted), 
                             y = Gold_Predicted)) +
  geom_point(size = 4, color = "gold") +  # Dot for Gold Medals
  geom_errorbar(aes(ymin = Gold_Lower, ymax = Gold_Upper), 
                width = 0.4, color = "black", alpha = 0.3) +  # Shaded error bars
  labs(title = "Predicted Gold Medals for 2028 Olympics", x = "Country", y = "Gold Medals",subtitle = "90 Percent Confidence Intervals Shown") +
  theme_bw() +
  coord_flip()

#################################################
#Backtest

countries = data_medal_counts$NOC[which(data_medal_counts$Olympics==25)]

gold_model = lm(Gold ~ Olympics + NOC, data = data_medal_counts)
total_model = lm(Total ~ Olympics + NOC, data = data_medal_counts)

predict_2028 = tibble(NOC = countries, Olympics = 26)

predict_2028$Gold_Predicted = predict(gold_model, newdata = predict_2028)
predict_2028$Total_Predicted = predict(total_model, newdata = predict_2028)



predict_2028 = predict_2028 |>
  mutate(Gold_Predicted = round(Gold_Predicted),
         Total_Predicted = round(Total_Predicted))


gold_predictions <- predict(gold_model, newdata = predict_2028, interval = "prediction", level = 0.9)
total_predictions <- predict(total_model, newdata = predict_2028, interval = "prediction", level = 0.9)


predict_2028 <- predict_2028 |> 
  mutate(Gold_Predicted = round(gold_predictions[, "fit"]),
         Gold_Lower = round(gold_predictions[, "lwr"]),
         Gold_Upper = round(gold_predictions[, "upr"]),
         Total_Predicted = round(total_predictions[, "fit"]),
         Total_Lower = round(total_predictions[, "lwr"]),
         Total_Upper = round(total_predictions[, "upr"]))

predict_2028 <- predict_2028 |> 
  mutate(Gold_Predicted = pmax(Gold_Predicted, 0),
         Gold_Lower = pmax(Gold_Lower, 0),
         Gold_Upper = pmax(Gold_Upper, 0),
         Total_Predicted = pmax(Total_Predicted, 0),
         Total_Lower = pmax(Total_Lower, 0),
         Total_Upper = pmax(Total_Upper, 0))


top_10_countries <- predict_2028 |> 
  arrange(desc(Total_Predicted)) |> 
  slice_head(n = 10)

noc_to_country <- c(
  "USA" = "United States", 
  "CHN" = "China", 
  "GER" = "Germany", 
  "GBR" = "Great Britain", 
  "FRA" = "France", 
  "ITA" = "Italy", 
  "JPN" = "Japan", 
  "AUS" = "Australia", 
  "HUN" = "Hungary", 
  "KOR" = "South Korea"
)


top_10_countries <- top_10_countries |> 
  mutate(Country = noc_to_country[NOC])



# Plot for total medals
ggplot(top_10_countries, aes(x = reorder(Country, Total_Predicted), 
                             y = Total_Predicted)) +
  geom_point(size = 4, color = "steelblue") +  # Dot for Total Medals
  geom_errorbar(aes(ymin = Total_Lower, ymax = Total_Upper), 
                width = 0.4, color = "black", alpha = 0.3) +  # Shaded error bars
  labs(title = "Predicted Total Medals for 2028 Olympics", x = "Country", y = "Total Medals",subtitle = "90 Percent Confidence Intervals Shown") +
  theme_bw() +
  coord_flip()

# Plot for gold medals
ggplot(top_10_countries, aes(x = reorder(Country, Gold_Predicted), 
                             y = Gold_Predicted)) +
  geom_point(size = 4, color = "gold") +  # Dot for Gold Medals
  geom_errorbar(aes(ymin = Gold_Lower, ymax = Gold_Upper), 
                width = 0.4, color = "black", alpha = 0.3) +  # Shaded error bars
  labs(title = "Predicted Gold Medals for 2028 Olympics", x = "Country", y = "Gold Medals",subtitle = "90 Percent Confidence Intervals Shown") +
  theme_bw() +
  coord_flip()
