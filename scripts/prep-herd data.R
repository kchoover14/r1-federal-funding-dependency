############ LIBRARIES
library(readr) # read zipped folders with csv
library(dplyr) # data wrangling
library(readxl) # read excel files


############ DATA SOURCE
# from ncses
# https://ncses.nsf.gov/explore-data/microdata/higher-education-research-development


############ EXTRACT DATA
# create values
years <- 1974:2024
data_path <- "data/"


# read data
herd_list <- lapply(years, function(yr) {
  zip_file <- file.path(data_path, paste0("higher_education_r_and_d_", yr, ".zip"))
  csv_file <- ifelse(yr <= 2009,
                     paste0("herd_", yr, ".csv"),
                     paste0("herd", yr, ".csv"))

  tryCatch({
    df <- read_csv(unz(zip_file, csv_file), show_col_types = FALSE)
    df$year <- yr

    if ("inst_state" %in% names(df)) {
      df$inst_state_code <- df$inst_state
    }

    # pull ipeds where available
    if ("ipeds_unitid" %in% names(df)) {
      df[, c("inst_name_long", "inst_state_code", "ipeds_unitid",
             "questionnaire_no", "question", "row", "column", "data", "year")]
    } else {
      df$ipeds_unitid <- NA
      df[, c("inst_name_long", "inst_state_code", "ipeds_unitid",
             "questionnaire_no", "question", "row", "column", "data", "year")]
    }
  }, error = function(e) {
    message("Failed: ", yr)
    NULL
  })
})

# check porblems
problems(herd_list)

# join if no problems
herd_raw <- do.call(rbind, herd_list)

#save
write.csv(herd_raw, 'artifacts/herd-raw.csv', row.names = FALSE)



############ TRANSFORM DATA
# herd_source = read.csv('artifacts/herd-raw.csv')

# explore data for vars
explore = herd_raw |> distinct(question, row) |> arrange(question, row) |> print(n=350)

# follow up on these fields after basic analysis done
# Capital expenditures by area
# Federal expenditures by field and agency
# Type of R&D conducted


# build data using filter and save
herd_source <- herd_raw |>
  filter(
    (questionnaire_no == "01" & question == "Source" &
       row %in% c("Federal", "Total")) |
      (questionnaire_no %in% c("01.a", "01.g"))
  ) |>
  mutate(
    funding_type = case_when(
      row %in% c("Federal", "Federal government") ~ "federal",
      row == "Total" ~ "total"
    )
  ) |>
  select(inst_name_long, inst_state_code, ipeds_unitid, year, funding_type, data)

# rename
herd_source = herd_source |> rename(herd_name = inst_name_long)
herd_source = herd_source |> rename(herd_state = inst_state_code)
herd_source = herd_source |> rename(ipeds = ipeds_unitid)

# clean names
herd_source <- herd_source |>
  mutate(herd_name_clean = str_to_lower(str_squish(herd_name)),
         herd_name_clean = str_replace_all(herd_name_clean, "\\.", ""),
         herd_name_clean = str_replace_all(herd_name_clean, ",", ""),
         herd_name_clean = str_replace_all(herd_name_clean, "&", "and"),
         herd_name_clean = str_replace_all(herd_name_clean, "-", " "))


# clean names
herd_source <- herd_source |>
  mutate(herd_name_clean = normalize_name(herd_name))

# rename and relocate
herd_source = herd_source |> relocate(herd_name_clean, .after = herd_name)

# search for st as abbr for state or saint
herd_source |>
  filter(str_detect(herd_name_clean, "\\bst\\b")) |>
  select(herd_name_clean) |>
  distinct() |>
  print(n = Inf)

# change st to full word
normalize_st <- function(x) {
  # st = saint
  x <- str_replace_all(x, "\\bst louis\\b", "saint louis")
  x <- str_replace_all(x, "\\bst john\\b", "saint john")
  x <- str_replace_all(x, "\\bst joseph\\b", "saint joseph")
  x <- str_replace_all(x, "\\bst mary\\b", "saint mary")
  x <- str_replace_all(x, "\\bst cloud\\b", "saint cloud")
  x <- str_replace_all(x, "\\bst olaf\\b", "saint olaf")
  x <- str_replace_all(x, "\\bst bonaventure\\b", "saint bonaventure")
  x <- str_replace_all(x, "\\bst lawrence\\b", "saint lawrence")
  x <- str_replace_all(x, "\\bst andrews\\b", "saint andrews")
  x <- str_replace_all(x, "\\bst vincent\\b", "saint vincent")
  x <- str_replace_all(x, "\\bst francis\\b", "saint francis")
  x <- str_replace_all(x, "\\bst thomas\\b", "saint thomas")
  x <- str_replace_all(x, "\\bst michael\\b", "saint michael")
  x <- str_replace_all(x, "\\bst edward\\b", "saint edward")
  x <- str_replace_all(x, "\\bst catherine\\b", "saint catherine")
  x <- str_replace_all(x, "\\bst scholastica\\b", "saint scholastica")
  x
}

# remove st
herd_source <- herd_source |>
  mutate(herd_name_clean = normalize_st(herd_name_clean))

# check results for st as abbr for state or saint
herd_source |>
  filter(str_detect(herd_name_clean, "\\bst\\b")) |>
  select(herd_name_clean) |>
  distinct() |>
  print(n = Inf)

#check for any lingering problems
name_check = distinct(herd_source, herd_name_clean)
write.csv(name_check, 'artifacts/herd-name_check.csv', row.names = FALSE)

#save
write.csv(herd_source, 'artifacts/herd.csv', row.names = FALSE)


######################## TIDY
rm(list = ls())
gc()

