# Load & Inspect MOM Income Data (2025)
# Project: Compensation Insights Dashboard
# Author: Jenny Kong
# Date: June 2025

# Load required libraries
library(tidyverse)  # Includes readr, dplyr, ggplot2, etc.

# Set data path
data_path <- "data/raw/"

# Load total income data
file_total <- paste0(data_path, "mom_income_total.csv")
if (file.exists(file_total)) {
  income_total <- read_csv(file_total)
  cat("✅Loaded: mom_income_total.csv\n\n")
} else {
  stop("❌ File mom_income_total.csv not found.")
}

# Load industry income data
file_industry <- paste0(data_path, "mom_income_by_industry.csv")
if (file.exists(file_industry)) {
  income_by_industry <- read_csv(file_industry)
  cat("✅ Loaded: mom_income_by_industry.csv\n\n")
} else {
  stop("❌ File mom_income_by_industry.csv not found.")
}

# Load occupation income data
file_occupation <- paste0(data_path, "mom_income_by_occupation.csv")
if (file.exists(file_occupation)) {
  income_by_occupation <- read_csv(file_occupation)
  cat("✅ Loaded: mom_income_by_occupation.csv\n\n")
} else {
  stop("❌ File mom_income_by_occupation.csv not found.")
}

# Preview first few rows of each datasets
cat("Preview: Total Income\n\n")
print(head(income_total))

cat("\nPreview: Industry Income\n\n")
print(head(income_by_industry))

cat("\nPreview: Occupation Income\n\n")
print(head(income_by_occupation))

# Check structure and column names
cat("\nStructure: Total Income\n\n")
str(income_total)

cat("\nStructure: Industry Income\n\n")
str(income_by_industry)

cat("\nStructure: Occupation Income\n\n")
str(income_by_occupation)

# Check for missing values
cat("\nMissing values in Total Income dataset:\n\n")
print(colSums(is.na(income_total)))

cat("\nMissing values in Industry Income dataset:\n\n")
print(colSums(is.na(income_by_industry)))

cat("\nMissing values in Occupation Income dataset:\n\n")
print(colSums(is.na(income_by_occupation)))

# Completion message
cat("\n✅ All datasets loaded and inspected successfully.\n")
