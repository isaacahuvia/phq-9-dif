library(tidyverse)

phq_2_options <- expand.grid(
  item_1 = 0:3,
  item_2 = 0:3
)

phq_2_options %>%
  mutate(sum = rowSums(.)) %>%
  count(sum >= 3)

phq_9_options <- expand.grid(
  item_1 = 0:3,
  item_2 = 0:3,
  item_3 = 0:3,
  item_4 = 0:3,
  item_5 = 0:3,
  item_6 = 0:3,
  item_7 = 0:3,
  item_8 = 0:3,
  item_9 = 0:3
)

phq_9_options %>%
  mutate(sum = rowSums(.)) %>%
  count(sum >= 10)
