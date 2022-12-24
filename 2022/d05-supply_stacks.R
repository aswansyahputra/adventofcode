# Load packages and data --------------------------------------------------

library(tidyverse)
library(unglue)

procedures_raw <- read_lines("2022/data-raw/d05-input.txt", skip = 10)

procedures <-
  procedures_raw %>%
  unglue_data(
    patterns = "move {qty} from {source} to {target}",
    convert = TRUE
  )

stacks_raw <-
  read_fwf("2022/data-raw/d05-input.txt", n_max = 8)

stacks <-
  stacks_raw %>%
  arrange(desc(row_number())) %>%
  as.list() %>%
  map(~.x[!is.na(.x)])

# Define helpers ----------------------------------------------------------

move_crate <- function(stacks, source, target, qty) {
  for (i in seq_len(qty)) {
    vec_source <- stacks[[source]]
    vec_target <- stacks[[target]]
    if (is_empty(vec_source)) stop("Crate stack is already empty", call. = FALSE)
    idx_vec_source_to_remove <- length(vec_source)
    stacks[[source]] <- vec_source[-idx_vec_source_to_remove]
    stacks[[target]] <- append(vec_target, vec_source[idx_vec_source_to_remove])
  }
  return(stacks)
}

move_crate_revised <- function(stacks, source, target, qty) {
  vec_source <- stacks[[source]]
  vec_target <- stacks[[target]]
  if (is_empty(vec_source)) stop("Crate stack is already empty", call. = FALSE)
  idx_vec_source_to_remove <-
    seq.int(
      from = length(vec_source) - qty + 1,
      to = length(vec_source),
      by = 1
    )
  stacks[[source]] <- vec_source[-idx_vec_source_to_remove]
  stacks[[target]] <- append(vec_target, vec_source[idx_vec_source_to_remove])
  return(stacks)
}

# Part I ------------------------------------------------------------------

#' After the rearrangement procedure completes, what crate ends up on top of each stack?

rearranged_stacks <- stacks
for (step in seq_len(NROW(procedures))) {
  qty <- procedures[step, "qty"]
  source <- procedures[step, "source"]
  target <- procedures[step, "target"]
  rearranged_stacks <- move_crate(
    stacks = rearranged_stacks,
    source = source,
    target = target,
    qty = qty
  )
}

rearranged_stacks %>%
  map(last) %>%
  paste(collapse = "") %>%
  str_remove_all(pattern = "\\[|\\]")

# Part II -----------------------------------------------------------------

#' After the rearrangement procedure completes, what crate ends up on top of each stack?

rearranged_stacks_revised <- stacks
for (step in seq_len(NROW(procedures))) {
  qty <- procedures[step, "qty"]
  source <- procedures[step, "source"]
  target <- procedures[step, "target"]
  rearranged_stacks_revised <- move_crate_revised(
    stacks = rearranged_stacks_revised,
    source = source,
    target = target,
    qty = qty
  )
}

rearranged_stacks_revised %>%
  map(last) %>%
  paste(collapse = "") %>%
  str_remove_all(pattern = "\\[|\\]")
