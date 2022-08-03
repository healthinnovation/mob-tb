library(fs)
library(readr)
library(dplyr)
library(stringi)

raw_path <- path("analysis", "data", "raw")
infile <- "per-demography-2017.csv"
infile_path <- path(raw_path, infile)
demo_raw <- read_csv(infile_path)

demo <- demo_raw %>%
  mutate(
    across(
      where(is.character), ~ stri_trans_general(toupper(.), id = "Latin-ASCII")
    )
  )

readRDS("analysis/data/raw/indic_dist_censo_2017.rds")
