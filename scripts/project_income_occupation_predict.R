# Script: project_income_occupation_predict.R
# Purpose:Generate income projections (2001-2034) by occupation using linear regression
# Author: Jenny Kong
# Updated: 21 June 2025

# Load required libraries
library(tidyverse)
library(purrr)

# STEP 1: Load cleaned dataset
df_occupation <- read_csv("data/cleaned/income_by_occupation_cleaned.csv")

# Confirm structure
str(df_occupation)

# Select only relevant columns
df_occupation_selected <- df_occupation %>% 
  select(year, occupation, income_incl_cpf)

# Split into historical data (2001-2024)
historical_data <- df_occupation_selected %>% 
  filter(year <= 2024)

# STEP 2: Create future dataframe (2025-2034 x 8 occupations)
future_data <- expand_grid(
  year = 2025:2034,
  occupation = unique(historical_data$occupation)
)

# STEP 3: Predict future income for each occupation
predicted_data <- map_dfr(unique(historical_data$occupation), function(occ) {
  #Subset for one occupation
  model_data <- historical_data %>% filter(occupation == occ)
  
  # Fit linear model
  model <- lm(income_incl_cpf ~ year, data = model_data)
  
  # Prepare future rows for that occupation
  future_subset <- future_data %>% filter(occupation == occ)
  
  # Predict and assign
  future_subset$income_incl_cpf <- predict(model, newdata = future_subset)
  
  return(future_subset)
})

# STEP 4: Label source type and combine data
historical_data <- historical_data %>% mutate(source = "Historical")
predicted_data <- predicted_data %>% mutate(source = "Predicted")

df_combined <- bind_rows(historical_data, predicted_data) %>% 
  mutate(
    source = factor(source),
    occupation = factor(occupation)
  )

# STEP 5: Plot the chart
p <- ggplot(df_combined, aes(x = year, y = income_incl_cpf, color = occupation, linetype = source)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8, alpha = 9) +
  scale_x_continuous(breaks = seq(2001, 2034, 2), expand = expansion(mult = c(0.01, 0.05))) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Projected Median Income by Occupation (2001-2034)", subtitle = "Solid lines: Actual Income (2001-2024) | Dashed lines: Predicted Income (2025-2034)", x = "Year", y = "Median Monthly Income (SGD)", color = "Occupation", linetype = "Data Type") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14), plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 10), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right", plot.margin = margin(10, 30, 10, 10), axis.title = element_text(face = "bold"))

# STEP 6: Export to PDF
ggsave("viz_output/projection_occupation_income_trend.pdf", plot = p, width = 10, height = 6)
