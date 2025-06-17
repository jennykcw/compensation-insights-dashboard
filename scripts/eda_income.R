# Exploratory Data Analysis (EDA)
# Project: Compensation Insights Dashboard
# Author: Jenny Kong
# Date: 14 June 2025

# Load necessary libraries
library(tidyverse)

# Set data path
clean_path <- "data/cleaned/"

# Load cleaned datasets
income_total_cleaned <- read_csv(paste0(clean_path, "income_total_cleaned.csv"))
income_by_industry_cleaned <- read_csv(paste0(clean_path, "income_by_industry_cleaned.csv"))
income_by_occupation_cleaned <- read_csv(paste0(clean_path, "income_by_occupation_cleaned.csv"))

# Confirm loaded
cat("✅ Cleaned datasets loaded successfully.\n")

# Structure check for each dataset
cat("\n🔍 Structure: Total Income\n\n")
str(income_total_cleaned)

cat("\n🔍 Structure: Industry Income\n\n")
str(income_by_industry_cleaned)

cat("\n🔍 Structure: Occupation Income\n\n")
str(income_by_occupation_cleaned)

# Summary statistics for numeric columns
cat("\n📊 Summary: Total Income\n\n")
summary(income_total_cleaned)

cat("\n📊 Summary: Industry Income\n\n")
summary(income_by_industry_cleaned)

cat("\n📊 Summary: Occupation Income\n\n")
summary(income_by_occupation_cleaned)

# Prepare data for viz
# Convert 'percentile' to factor for grouped color chart
income_total_cleaned$percentile <- as.factor(income_total_cleaned$percentile)

# Load ggplot2 for charting
library(ggplot2)

# Line chart: Income trend over time by percentile
ggplot(income_total_cleaned, aes(x = year, y = income_incl_cpf, group = percentile, color = percentile)) +
         geom_line(linewidth = 1) +
         geom_point(size = 2) + # dots help show year-wise trend
         scale_x_continuous(breaks = seq(1996, 2024, 2)) + # Show every 2nd year
         labs(title = "Median Income Trend by Percentile (Including Employer CPF)", x = "Year", y = "Gross Monthly Income (SGD)", color = "Percentile") +
         theme_minimal() +
         theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5), axis.title = element_text(face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1), # rotate for visibility 
         legend.title = element_text(face = "bold"))

# 📁 Save Viz to PDF (for documentation and reuse)
# This section exports finalized chart to the 'viz_output/' folder in PDF format:
# Percentile Income Trend (p20 & p50)
# Percentile Income Trend chart
ggsave("viz_output/percentile_income_trend.pdf", width = 10, height = 6)

# Filter for the latest year (2024) in Industry Income dataset
industry_2024 <- income_by_industry_cleaned %>% 
  filter(year == max(year, na.rm = TRUE))

# Bar chart: Median Income by Industry in 2024
ggplot(industry_2024, aes(x = reorder(industry, income_incl_cpf), y = income_incl_cpf)) +
  geom_col(fill = "#69b3a2") +
  coord_flip() +
  labs(title = "Median Income by Industry (2024)", x = "Industry", y = "Gross Monthly Income (SGD)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5), axis.title = element_text(face = "bold"), axis.text.y = element_text(size = 10))

# 📁 Save Viz to PDF (for documentation and reuse)
# This section exports finalized chart to the 'viz_output/' folder in PDF format:
# Median Income by Industry (2024)
# Industry Income chart
ggsave("viz_output/income_by_industry_2024.pdf", width = 10, height = 6)

# Filter for the latest year (2024) in Occupation Income dataset
occupation_2024 <- income_by_occupation_cleaned %>% 
  filter(year == max(year, na.rm = TRUE))

# Bar chart: Median Income by Occupation in 2024
ggplot(occupation_2024, aes(x= reorder(occupation, income_incl_cpf), y = income_incl_cpf)) + 
  geom_col(fill = "#f9a825") +
  coord_flip() +
  labs(title = "Median Income by Occupation (2024)", x = "Occupation", y = "Gross Monthly Income (SGD)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5), axis.title = element_text(face = "bold"), axis.text.y = element_text(size = 8))

# 📁 Save Viz to PDF (for documentation and reuse)
# This section exports finalized chart to the 'viz_output/' folder in PDF format:
# Median Income by Occupation (2024)
# Occupation Income chart
ggsave("viz_output/income_by_occupation_2024.pdf", width = 10, height = 6)

# Define a vector of selected industries we want to analyze over time
target_industries <- c("financial & insurance services", "information & communications", "public administration & education", "professional services","manufacturing", "wholesale & retail trade", "health & social services", "construction")

# The %in% operator checks whether each industry value exists in the target_industries vector
# Filter the dataset to keep only rows where the industry matches one of the selected industries from the vector
industry_trend <- income_by_industry_cleaned %>% 
  filter(industry %in% target_industries)

# Line chart: Median Income Trend by Industry
ggplot(industry_trend, aes(x = year, y = income_incl_cpf, color = industry, group = industry)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(min(industry_trend$year), max(industry_trend$year), 2)) + 
  labs(
    title = "Median Income Trend by Industry (1996-2024)", x = "Year", y = "Gross Monthly Income (SGD)", color = "Industry"
  ) + 
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5), axis.title = element_text(face = "bold"), legend.title = element_text(face = "bold"))

# 📁 Save Viz to PDF (for documentation and reuse)
# This section exports finalized chart to the 'viz_output/' folder in PDF format:
# Median Income Trend by Industry (2024)
# Industry Income Trend Over Time chart
ggsave("viz_output/industry_income_trend.pdf", width = 10, height = 6)

# Line chart: Median Income Trend by Occupation 
ggplot(income_by_occupation_cleaned, aes(x = year, y = income_incl_cpf, color = occupation, group = occupation)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(min(income_by_occupation_cleaned$year), max(income_by_occupation_cleaned$year), 2)) +
  labs(title = "Median Income Trend by Occupation (1996 - 2024)", x = "Year", y = "Gross Monthly Income (SGD)", color = "Occupation") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5), axis.title = element_text(face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_text(face = "bold"), legend.position = "bottom", legend.direction = "horizontal", legend.box = "vertical", plot.margin = margin(t = 10, r = 15, b = 10, l = 15) # Add spacing around the plot
        )

# 📁 Save Viz to PDF (for documentation and reuse)
# This section exports finalized chart to the 'viz_output/' folder in PDF format:
# Median Income Trend by Occupation (2024)
# Occupation Income Trend Over Time chart
ggsave("viz_output/occupation_income_trend.pdf", width = 14, height = 9)  
                      


  