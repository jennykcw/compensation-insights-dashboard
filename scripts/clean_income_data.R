# Load tidyverse
library(tidyverse)

# Set file paths
raw_path <- "data/raw/"
clean_path <- "data/cleaned/"

# Load the Total Income dataset
income_total <- read_csv(paste0(raw_path, "mom_income_total.csv"))

# Check current column names
colnames(income_total)

# Clean, transform, and rename columns for Total Income dataset:
# - Some 'year' values were non-integer (e.g., NA, text like "Total" or "2007a", or empty spaces), causing coercion warnings
# - Some 'percentile' values were non-numeric (e.g., "p20", "p50"), causing NA when coerced
# - Verified using: unique(income_total$year) and unique(income_total$percentile)
# - Converted 'year' to integer and convert 'percentile' to numeric using mutate()
# - Cleaned 'percentile' column by extracting numeric portion from the character labels like "p20", "p50"
# - Renamed income columns for clarity
# - Selected only relevant columns and filtered out rows with invalid values
income_total_cleaned <- income_total %>% 
  mutate(
    year = as.integer(year),
    percentile = as.numeric(gsub("[^0-9]", "", percentile)), # clean 'p20' to 20
    income_incl_cpf = gross_monthly_income_including_employer_cpf,
    income_excl_cpf = gross_monthly_income_excluding_employer_cpf
  ) %>% 
  select(year, percentile, income_incl_cpf, income_excl_cpf) %>% 
  filter(!is.na(year), !is.na(percentile))

# Check for missing values
colSums(is.na(income_total_cleaned))

# Save the cleaned Total Income dataset
write_csv(income_total_cleaned, paste0(clean_path, "income_total_cleaned.csv"))
cat("✅ Saved: income_total_cleaned.csv in data/cleaned/\n")

# Load the Industry Income dataset
income_by_industry <- read_csv(paste0(raw_path, "mom_income_by_industry.csv"))

# Check current column names
colnames(income_by_industry)

# Clean, transform and rename columns for Industry Income dataset:
# - Some 'year' values were non-integer (e.g., NA, text like "Total" or "2007a", or empty spaces), causing coercion warnings
# - Verified using: unique(income_by_industry_cleaned$year)
# - Converted 'year' to integer using filter() and mutate()
# - Renamed income columns for clarity
# - Selected only relevant columns for plotting and analysis
income_by_industry_cleaned <- income_by_industry %>% 
  filter(grepl("^\\d{4}$", year)) %>% 
  mutate(
    year = as.integer(year),
    income_incl_cpf = median_gross_monthly_income_including_employer_cpf,
    income_excl_cpf = median_gross_monthly_income_excluding_employer_cpf
  ) %>% 
  select(year, industry, income_incl_cpf, income_excl_cpf)

# Check for missing values
colSums(is.na(income_by_industry_cleaned))

# Save the cleaned Industry Income dataset
write_csv(income_by_industry_cleaned, paste0(clean_path, "income_by_industry_cleaned.csv"))
cat("✅ Saved: income_by_industry_cleaned.csv in data/cleaned/\n")

# Load the Occupation Income dataset
income_by_occupation <- read_csv(paste0(raw_path, "mom_income_by_occupation.csv"))

# Check current column names
colnames(income_by_occupation)

# Clean, transform and rename columns for Occupation Income dataset:
# - Some 'year' values were non-integer (e.g., NA, text like "Total" or "2007a", or empty spaces), causing coercion warnings
# - Verified using: unique(income_by_occupation_cleaned$year)
# - Converted 'year' to integer using filter() and mutate()
# - Renamed income columns for clarity
# - Selected only relevant columns for plotting and analysis
income_by_occupation_cleaned <- income_by_occupation %>% 
  filter(grepl("^\\d{4}$", year)) %>% 
  mutate(
    year = as.integer(year),
    income_incl_cpf = median_gross_monthly_income_including_employer_cpf,
    income_excl_cpf = median_gross_monthly_income_excluding_employer_cpf
  ) %>% 
  select(year, occupation, income_incl_cpf, income_excl_cpf)

# Check for missing values
colSums(is.na(income_by_occupation_cleaned))

# Save the cleaned Occupation Income dataset
write_csv(income_by_occupation_cleaned, paste0(clean_path, "income_by_occupation_cleaned.csv"))
cat("✅ Saved: income_by_occupation_cleaned.csv in data/cleaned/\n")




