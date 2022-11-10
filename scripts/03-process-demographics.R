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

demographics_full <- population %>%
  inner_join(demographics_raw, by = "ubigeo") %>%
  mutate(
    life_expectancy_norm = (life_expectancy - 25) / (85 - 25),
    years_edu_norm = (years_edu - 1.8) / (16 - 1.8),
    edu_achievement = (life_expectancy_norm * years_edu_norm)^0.5,
    family_income_norm = (family_income - 35) / (2500 - 35),
    hh_1_nbi_or_more_prop = hh_1_nbi_or_more / number_hh,
    hh_inadequate_char_prop = hh_inadequate_char / number_hh,
    overcrowded_hh_prop = overcrowded_hh / number_hh,
    hh_wo_sanitation_prop = hh_wo_sanitation / number_hh,
    hh_school_absence_prop = hh_school_absence / number_hh,
    hh_high_economic_dependence_prop = hh_high_economic_dependence / number_hh
  )

demographics <- demographics_full %>%
  select(
    department:monetary_poverty, life_expectancy = life_expectancy_norm,
    complete_secondary_edu, years_edu = years_edu_norm, edu_achievement,
    family_income = family_income_norm, hh_1_nbi_or_more = hh_1_nbi_or_more_prop,
    hh_inadequate_char = hh_inadequate_char_prop,
    overcrowded_hh = overcrowded_hh_prop, hh_wo_sanitation = hh_wo_sanitation_prop,
    hh_school_absence = hh_school_absence_prop,
    hh_high_economic_dependence = hh_high_economic_dependence_prop
  )

interim_path <- "data/interim/to-merge/"
output_filename <- "01-demographics.csv"
output_filepath <- path(interim_path, output_filename)
write_csv(demographics, output_filepath, na = "")
