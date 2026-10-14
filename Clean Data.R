## Load packages
library(tidyverse)
library(haven)
library(here)
library(vroom)
`%+%` <- paste0

conflicted::conflict_prefer_all("dplyr", quiet = T)


### Load data
## HMS: Load most recent year
# NOTE: Multiple years are available, but there is no way to deduplicate across years
hms_data <- here("Data", "HMS", "HMS_2025-2026_PUBLIC_instchars.csv") %>%
  vroom(
    col_select = c(
      nrweight,
      starts_with("phq9"),
      age,
      starts_with("gender_"),
      starts_with("race_"),
      psu = schoolnum
    )
  ) %>%
  mutate(
    across(starts_with("phq"), ~ . - 1),
    phq_sum = phq9_1 + phq9_2 + phq9_3 + phq9_4 + phq9_5 + phq9_6 + phq9_7 + phq9_8 + phq9_9,
    sample_char = "HMS",
    n_gender = rowSums(!is.na(pick(starts_with("gender_")))),
    gender = case_when(
      n_gender > 1 ~ "TGD",
      n_gender == 1 & gender_male == 1 ~ "Cisgender Male",
      n_gender == 1 & gender_female == 1 ~ "Cisgender Female",
      (!is.na(gender_trans) | !is.na(gender_nonbin) | !is.na(gender_nonconf) | !is.na(gender_selfID)) ~ "TGD",
      T ~ NA_character_
    ),
    n_race = rowSums(!is.na(pick(starts_with("race_")))),
    race = case_when(
      n_race > 1 ~ "Multiracial/Other",
      n_race == 1 & race_black == 1 ~ "Black",
      n_race == 1 & race_ainaan == 1 ~ "American Indian/Alaska Native",
      n_race == 1 & race_asian == 1 ~ "Asian",
      n_race == 1 & race_his == 1 ~ "Hispanic",
      n_race == 1 & race_pi == 1 ~ "Pacific Islander",
      n_race == 1 & race_mides == 1 ~ "Middle-Eastern",
      n_race == 1 & race_white == 1 ~ "White",
      race_other == 1 ~ "Multiracial/Other",
      T ~ NA_character_
    )
  ) %>%
  select(
    phq_1 = phq9_1,
    phq_2 = phq9_2,
    phq_3 = phq9_3,
    phq_4 = phq9_4,
    phq_5 = phq9_5,
    phq_6 = phq9_6,
    phq_7 = phq9_7,
    phq_8 = phq9_8,
    phq_9 = phq9_9,
    phq_sum,
    sample_char,
    weight = nrweight,
    psu,
    age,
    gender,
    race
  )


## NHANES
# Load and bind demographics files
nhanes_demo_files <- list.files(here("Data", "NHANES", "DEMO"), full.names = T)

nhanes_demo_data <- nhanes_demo_files %>%
  set_names() %>%
  map(
    ~ {.} %>%
      read_xpt()
  ) %>%
  bind_rows(.id = "file") %>%
  select(
    nhanes_id = SEQN,
    nhanes_yr = SDDSRVYR,
    age = RIDAGEYR,
    gender = RIAGENDR,
    race = RIDRETH3,
    psu = SDMVPSU,
    strata = SDMVSTRA,
    WTMEC2YR, WTMECPRP
  ) %>%
  filter(age >= 18) %>%
  mutate(
    nhanes_yr = recode_values(
      nhanes_yr,
      7 ~ "2011-2012",
      8 ~ "2013-2014",
      9 ~ "2015-2016",
      66 ~ "2017-2020",
      12 ~ "2021-2023"
    ),
    gender = recode_values(
      gender,
      1 ~ "Male",
      2 ~ "Female"
    ),
    race = recode_values(
      race,
      c(1, 2) ~ "Hispanic",
      3 ~ "White",
      4 ~ "Black",
      6 ~ "Asian",
      7 ~ "Multiracial/Other"
    ),
    weight = case_when(
      nhanes_yr %in% c("2011-2012", "2013-2014", "2015-2016", "2021-2023") ~ WTMEC2YR,
      nhanes_yr == "2017-2020" ~ WTMECPRP
    )
  )

# Load and bind PHQ-9 files
nhanes_phq_files <- list.files(here("Data", "NHANES", "PHQ"), full.names = T)

nhanes_phq_data <- nhanes_phq_files %>%
  set_names() %>%
  map(
    ~ {.} %>%
      read_xpt()
  ) %>%
  bind_rows(.id = "file") %>%
  dplyr::select(
    phq_1 = DPQ010,
    phq_2 = DPQ020,
    phq_3 = DPQ030,
    phq_4 = DPQ040,
    phq_5 = DPQ050,
    phq_6 = DPQ060,
    phq_7 = DPQ070,
    phq_8 = DPQ080,
    phq_9 = DPQ090,
    nhanes_id = SEQN
  ) %>%
  mutate(
    across(starts_with("phq"), ~ if_else(. %in% 0:3, ., NA_real_)),
    phq_sum = phq_1 + phq_2 + phq_3 + phq_4 + phq_5 + phq_6 + phq_7 + phq_8 + phq_9
  )

# Merge NHANES files
nhanes_data <- left_join(
  nhanes_phq_data,
  nhanes_demo_data,
  by = "nhanes_id"
) %>%
  mutate(
    sample_char = "NHANES"
  ) %>%
  select(
    starts_with("phq"),
    sample_char,
    nhanes_yr,
    nhanes_id,
    psu,
    strata,
    weight,
    age,
    gender,
    race
  )


## Combine HMS, NHANES datasets
combined_data <- bind_rows(
  hms_data,
  nhanes_data
) %>%
  mutate(
    sample_fct = factor(sample_char, levels = c("NHANES", "HMS")),
    most_recent_survey = case_when(
      sample_char == "HMS" ~ T,
      sample_char == "NHANES" & nhanes_yr == "2021-2023" ~ T,
      T ~ F
    )
  ) %>%
  drop_na(phq_1, phq_2, phq_3, phq_4, phq_5, phq_6, phq_7, phq_8, phq_9)


## Save data
saveRDS(combined_data, here("Data", "Clean Data.rds"))
