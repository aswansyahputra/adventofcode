# Load packages and data --------------------------------------------------

library(tidyverse)
library(slider)

datastream_raw <- read_lines("2022/data-raw/d06-input.txt")

datastream <-
  datastream_raw %>%
  str_split_1(pattern = "") %>%
  enframe(name = "seq", value = "signal")

# Part I ------------------------------------------------------------------

#' How many characters need to be processed before the first start-of-packet marker is detected?


datastream %>%
  mutate(
    packet = slide_chr(signal, .f = ~ paste0(.x, collapse = ""), .before = 3),
    n_uniq = slide_int(signal, .f = ~ n_distinct(.x), .before = 3),
    is_marker = (n_uniq == 4)
  ) %>%
  filter(is_marker == TRUE)

# Part II -----------------------------------------------------------------

#' How many characters need to be processed before the first start-of-message marker is detected?

datastream %>%
  mutate(
    packet = slide_chr(signal, .f = ~ paste0(.x, collapse = ""), .before = 13),
    n_uniq = slide_int(signal, .f = ~ n_distinct(.x), .before = 13),
    is_message = (n_uniq == 14)
  ) %>%
  filter(is_message == TRUE)
