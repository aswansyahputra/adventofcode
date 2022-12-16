# Load packages and data --------------------------------------------------

library(tidyverse)

calories_raw <- read_lines("2022/data-raw/d01-input.txt")

calories <-
  calories_raw %>%
  enframe(name = NULL, value = "calorie") %>%
  mutate(
    calorie = na_if(calorie, ""),
    calorie = as.integer(calorie),
    elf = if_else(
      is.na(calorie),
      row_number(),
      NA_integer_
    ),
    elf = dplyr::dense_rank(elf)
  ) %>%
  fill(elf, .direction = "up") %>%
  filter(!is.na(calorie))

# Part I ------------------------------------------------------------------

#' Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?

calories %>%
  count(elf, wt = calorie, name = "calories", sort = TRUE)

# Part II -----------------------------------------------------------------

#' Find the top three Elves carrying the most Calories. How many Calories are those Elves carrying in total?

calories %>%
  count(elf, wt = calorie, name = "calories", sort = TRUE) %>%
  slice_max(n = 3, order_by = calories) %>%
  count(wt = calories, name = "calories")
