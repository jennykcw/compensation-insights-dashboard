# Script: project_income_industry_predict.R
# Purpose: Generate income projections (2001-2034) by industry using linear regression
# Author: Jenny Kong
# Updated: 21 June 2025

# Load necessary libraries
library(tidyverse)

# Load cleaned dataset
df_industry <- read_csv("data/cleaned/income_by_industry_cleaned.csv")

# Quick structure check
str(df_industry)

# Preview first few rows
head(df_industry)

# Define top 8 industries
top_industries <- c("financial & insurance services", "information & communications", "health & social services", "wholesale & retail trade", "construction", "professional services", "manufacturing", "public administration & education")

# Filter dataset to include only top industries
df_industry_filtered <- df_industry %>% 
  filter(industry %in% top_industries)

# Split historical data (2001-2024) 
historical_data <- df_industry_filtered %>% 
  filter(year <= 2024)

# Create future dataframe (2025-2034 x 8 industries)
future_data <- expand_grid(
  year = 2025:2034,
  industry = unique(historical_data$industry)
)

# Predict future income using lm()
predicted_data <- map_dfr(unique(df_industry_filtered$industry), function(ind) {
  
  # Filter data for current industry
  model_data <- df_industry_filtered %>% filter(industry == ind, year <= 2024)
  
  # Fit model safely
  if (nrow(model_data) >=2) {
    model <- lm(income_incl_cpf ~ year, data = model_data)
  
    # Create future rows for this industry
  future_subset <- tibble(
    year = 2025:2034,
    industry = ind
  )
  
   # Predict income
  future_subset$income_incl_cpf <- predict(model, newdata = future_subset)
  
  return(future_subset)
  } else {
  return(tibble()) # Skip if not enough data
  }
})

# Label source
historical_data <- historical_data %>% mutate(source = "Historical")
predicted_data <- predicted_data %>% mutate(source = "Predicted")
  
# Combine and sort for continuous line drawing
df_combined <- bind_rows(
  historical_data, predicted_data) %>% 
  mutate(
    source = factor(source, levels = c("Historical", "Predicted")),
    industry = factor(industry), 
    linetype = ifelse(year <= 2024, "Historical", "Predicted")
  ) %>% 
  arrange(industry, year)

# Build segment connector from 2024 to 2025 per industry
df_bridge <- historical_data %>% 
  filter(year == 2024) %>% 
  inner_join(predicted_data %>% filter(year == 2025), 
             by = "industry", suffix = c("_2024", "_2025")) %>% 
  transmute(
    industry,
    x = year_2024, 
    y = income_incl_cpf_2024,
    xend = year_2025,
    yend = income_incl_cpf_2025
  )

# Plot chart
ggplot(df_combined, aes(x = year, y = income_incl_cpf, color = industry, group = interaction(industry, source), linetype = source)) +
  geom_line(linewidth = 1) + 
  geom_segment(data = df_bridge, mapping = aes(x = x, y = y, xend = xend, yend = yend, color = industry), linetype = "dashed", linewidth = 1, inherit.aes = FALSE) +
  scale_linetype_manual(values = c("Historical" = "solid", "Predicted" = "dashed")) +
  scale_x_continuous(breaks = seq(2001, 2034, 2)) +
  labs(title = "Projected Median Monthly Income by Industry (2001-2034)", subtitle = "Top 8 industries in Singapore - Linear Model Forecast", x = "Year", y = "Income (SGD)", color = "Industry", linetype = "Source") +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position = "bottom", axis.title = element_text(face = "bold"))

# Export to PDF
ggsave("viz_output/projection_industry_income_trend.pdf", width = 14, height = 7)












