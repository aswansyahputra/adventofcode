# Load packages and data --------------------------------------------------

library(tidyverse)
library(unglue)

assignments_raw <- read_lines("2022/data-raw/d04-input.txt")

assignment <-
  assignments_raw %>%
  unglue_data(patterns = "{first_elf_start}-{first_elf_end},{second_elf_start}-{second_elf_end}", convert = TRUE) %>%
  as_tibble() %>%
  transmute(
    pair = row_number(),
    first_elf_sections = map2(first_elf_start, first_elf_end, ~ seq.int(.x, .y, by = 1)),
    second_elf_sections = map2(second_elf_start, second_elf_end, ~ seq.int(.x, .y, by = 1))
  )

# Define helpers ----------------------------------------------------------

is_all_covered <- function(x, y) {
  length(intersect(x, y)) == min(c(length(x), length(y)))
}

is_any_overlap <- function(x, y) {
  length(intersect(x, y)) >= 1
}

# Part I ------------------------------------------------------------------

#' In how many assignment pairs does one range fully contain the other?

assignment %>%
  mutate(
    all_covered = map2_lgl(first_elf_sections, second_elf_sections, is_all_covered)
  ) %>%
  count(wt = all_covered)

# Part II -----------------------------------------------------------------

#' In how many assignment pairs do the ranges overlap?

assignment %>%
  mutate(
    any_overlap = map2_lgl(first_elf_sections, second_elf_sections, is_any_overlap)
  ) %>%
  count(wt = any_overlap)

