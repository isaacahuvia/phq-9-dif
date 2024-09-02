## Load packages
library(tidyverse)
library(haven)
library(here)
library(vroom)
`%+%` <- paste0


### Load data
## HMS: Load most recent year
# NOTE: Multiple years are available, but there is no way to deduplicate across years
hms_data <- here("Data", "HMS", "HMS_2022-2023_PUBLIC_instchars.csv") %>%
  vroom(
    col_select = c(
      nrweight,
      starts_with("phq9")
    )
  ) %>%
  mutate(
    across(starts_with("phq"), ~ . - 1),
    phq_sum = phq9_1 + phq9_2 + phq9_3 + phq9_4 + phq9_5 + phq9_6 + phq9_7 + phq9_8 + phq9_9,
    scaled_weight = as.numeric(scale(nrweight)),
    sample_hms = 1,
    sample_char = "HMS"
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
    sample_hms,
    sample_char,
    scaled_weight
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
  dplyr::select(
    nhanes_id = SEQN,
    gender = RIAGENDR,
    race = RIDRETH3,
    age = RIDAGEYR,
    income = INDFMPIR,
    weight = WTINTPRP
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
    scaled_weight = as.numeric(scale(weight)),
    sample_hms = 0,
    sample_char = "NHANES"
  ) %>%
  select(
    starts_with("phq"),
    sample_hms,
    sample_char,
    scaled_weight
  )


## Combine HMS, NHANES datasets
combined_data <- bind_rows(
  hms_data,
  nhanes_data
) %>%
  drop_na(phq_1, phq_2, phq_3, phq_4, phq_5, phq_6, phq_7, phq_8, phq_9)


## Save data
saveRDS(combined_data, here("Data", "Clean Data.rds"))
