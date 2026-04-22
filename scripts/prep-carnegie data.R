############ LIBRARIES
library(readr) # read zipped folders with csv
library(dplyr) # data wrangling
library(readxl) # read excel files
library(stringr) # string wrangling
library(fuzzyjoin) # carnegie join pre-2010 for fuzzy matches on names


############ LOAD CARNEGIE DATA AND CLEAN NAMES
# HERD does not have 2025 data so no need for carnegie 2025
# use historical carnegie data to filter to R1s
carnegie_hist <- read_xlsx("data/carnegie-historical.xlsx", sheet = "CCData")
carnegie_r1 <- carnegie_hist |>
  filter((VALUE == 11 & YEAR <= 1994) | VALUE == 15) |>
  mutate(instnm_clean = str_to_lower(str_squish(INSTNM)),
         instnm_clean = str_replace_all(instnm_clean, "-", " "),
         instnm_clean = str_replace_all(instnm_clean, "&", "and"),
         instnm_clean = str_replace_all(instnm_clean, "\\.", ""),
         city_clean = str_to_lower(str_squish(CITY))) |>
  select(UNITID, YEAR, STATE, city_clean, instnm_clean) |>
  rename(unitid = UNITID, stabbr = STATE) |>
  distinct()
table(carnegie_r1$YEAR)




############ RENAME
carnegie_r1 = carnegie_r1 |> rename(carnegie_name_clean = instnm_clean)
carnegie_r1 = carnegie_r1 |> rename(carnegie_city = city_clean)
carnegie_r1 = carnegie_r1 |> rename(year = YEAR)

############ FINAL CHECK
# check distinct names
name_check = distinct(carnegie_r1, carnegie_name_clean)

############ SAVE DATA
write.csv(carnegie_r1, "artifacts/carnegie-r1.csv", row.names = FALSE)



######################## TIDY
rm(list = ls())
gc()
