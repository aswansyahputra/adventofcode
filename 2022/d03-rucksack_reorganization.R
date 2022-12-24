# Load packages and data --------------------------------------------------

library(tidyverse)

rucksacks_raw <- read_lines("2022/data-raw/d03-input.txt")

rucksacks <-
  rucksacks_raw %>%
  enframe(name = NULL, value = "manifest")

# Define helpers ----------------------------------------------------------

priorities_lookup <-
  c(letters, LETTERS) %>%
  set_names(x = seq_along(.), nm = .)

find_common <- function(x, y) {
  uniq_x <- unique(str_split_1(x, pattern = ""))
  uniq_y <- unique(str_split_1(y, pattern = ""))
  res <- intersect(uniq_x, uniq_y)
  return(res)
}
find_common <- Vectorize(find_common)

find_common_among_three <- function(x, y, z) {
  uniq_x <- unique(str_split_1(x, pattern = ""))
  uniq_y <- unique(str_split_1(y, pattern = ""))
  uniq_z <- unique(str_split_1(z, pattern = ""))
  res <- reduce(list(uniq_x, uniq_y, uniq_z), intersect)
  return(res)
}
find_common_among_three <- Vectorize(find_common_among_three)

# Part I ------------------------------------------------------------------

#' Find the item type that appears in both compartments of each rucksack. What is the sum of the priorities of those item types?

rucksacks %>%
  mutate(
    items_count = nchar(manifest),
    first_compartment = str_sub(manifest, start = 1, end = (items_count / 2)),
    second_compartment = str_sub(manifest, start = (items_count / 2) + 1, end = items_count)
  ) %>%
  mutate(
    common_item = find_common(first_compartment, second_compartment),
    priority = recode(common_item, !!!priorities_lookup)
  ) %>%
  count(wt = priority, name = "total_priorities")

# Part II -----------------------------------------------------------------

#' Find the item type that corresponds to the badges of each three-Elf group. What is the sum of the priorities of those item types?

rucksacks %>%
  mutate(
    group = rep(seq.int(from = 1, to = NROW(.) / 3), each = 3),
    rucksack = rep(c("first", "second", "third"), length.out = NROW(.))
  ) %>%
  pivot_wider(
    names_from = rucksack,
    names_glue = "{.name}_rucksack",
    values_from = manifest
  ) %>%
  mutate(
    common_item = find_common_among_three(first_rucksack, second_rucksack, third_rucksack),
    priority = recode(common_item, !!!priorities_lookup)
  ) %>%
  count(wt = priority, name = "total_priorities")

