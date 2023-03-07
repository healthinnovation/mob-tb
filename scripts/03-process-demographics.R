library(fs)
library(readr)
library(dplyr)

raw_path <- path("data/raw/")
demographics_filename <- "demographics.csv"
demographics_filepath <- path(raw_path, demographics_filename)
demographics_raw <- read_csv(demographics_filepath, col_types = "cd")

population_filename <- "population.csv"
population_filepath <- path(raw_path, population_filename)
population <- read_csv(population_filepath, col_types = "ccccdd")

demographics <- population %>%
  inner_join(demographics_raw, by = "ubigeo")
  # mutate(
  #   monetary_poverty = monetary_poverty,
  #   complete_secondary_edu = complete_secondary_edu,
  #   life_expectancy = (life_expectancy - 25) / (85 - 25),
  #   years_edu = (years_edu - 1.8) / (16 - 1.8),
  #   family_income = (family_income - 35) / (2500 - 35)
  # ) %>%
  # mutate(edu_achievement = (life_expectancy * years_edu)^0.5)

interim_path <- "data/interim/to-merge/"
output_filename <- "01-demographics.csv"
output_filepath <- path(interim_path, output_filename)
write_csv(demographics, output_filepath, na = "")
