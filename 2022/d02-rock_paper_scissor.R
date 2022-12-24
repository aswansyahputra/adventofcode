# Load packages and data --------------------------------------------------

library(tidyverse)

rps_raw <- read_lines("2022/data-raw/d02-input.txt")

rps <-
  rps_raw %>%
  enframe(name = NULL) %>%
  separate(
    col = value,
    into = c("op", "me")
  )

# Define helpers ----------------------------------------------------------

op_ref <- c("A" = "rock", "B" = "paper", "C" = "scissor")
me_ref <- c("X" = "rock", "Y" = "paper", "Z" = "scissor")
me_ref_revised <- c("X" = "lose", "Y" = "draw", "Z" = "win")
shape_score_ref <- c("rock" = 1, "paper" = 2, "scissor" = 3)
round_score_ref <- c("win" = 6, "draw" = 3, "lose" = 0)

get_outcome <- function(op, me) {
  res <- "win"
  if (op == me) res <- "draw"
  if (op == "rock" && me == "scissor") res <- "lose"
  if (op == "scissor" && me == "paper") res <- "lose"
  if (op == "paper" && me == "rock") res <- "lose"
  return(res)
}
get_outcome <- Vectorize(get_outcome)

get_shape <- function(op, outcome) {
  assigned_values <- c("rock" = 3, "scissor" = 2, "paper" = 1)
  if (outcome == "draw") inc <- 0
  if (outcome == "win") {
    inc <- 1
    if (op == "rock") {
      assigned_values[["paper"]] <- 4
    }
  }
  if (outcome == "lose") {
    inc <- -1
    if (op == "paper") {
      assigned_values[["paper"]] <- 4
    }
  }
  op_value <- recode(op, !!!assigned_values)
  my_value <- op_value + inc
  res <- names(assigned_values[assigned_values == my_value])
  return(res)
}
get_shape <- Vectorize(get_shape)

# Part I ------------------------------------------------------------------

#' What would your total score be if everything goes exactly according to your strategy guide?

rps %>%
  mutate(
    op = recode(op, !!!op_ref),
    me = recode(me, !!!me_ref),
    outcome = get_outcome(op, me)
  ) %>%
  mutate(
    shape_score = recode(me, !!!shape_score_ref),
    round_score = recode(outcome, !!!round_score_ref),
    total_score = shape_score + round_score
  ) %>%
  count(wt = total_score, name = "final_score")

# Part II -----------------------------------------------------------------

#' Following the Elf's instructions for the second column, what would your total score be if everything goes exactly according to your strategy guide?

rps %>%
  mutate(
    op = recode(op, !!!op_ref),
    outcome = recode(me, !!!me_ref_revised),
    me = get_shape(op, outcome)
  ) %>%
  mutate(
    shape_score = recode(me, !!!shape_score_ref),
    round_score = recode(outcome, !!!round_score_ref),
    total_score = shape_score + round_score
  ) %>%
  count(wt = total_score, name = "final_score")
