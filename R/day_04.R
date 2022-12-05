library(tidyr)
library(dplyr)

input <- data.frame(readLines("input/input-day04"))

# part 1
contains <- input |> 
  separate(col = 1, into = c("A", "B", "C", "D")) |>
  mutate(across(everything(), as.numeric)) |> 
  rowwise() |> 
  mutate(n_within = sum(all(seq(A, B) %in% seq(C, D)), all(seq(C, D) %in% seq(A, B)))) |> 
  mutate(contains = if_else(n_within == 0, 0, 1))

sum(contains$contains)

# part 2
intersects <- input |> 
  separate(col = 1, into = c("A", "B", "C", "D")) |>
  mutate(across(everything(), as.numeric)) |> 
  rowwise() |> 
  mutate(n_within = sum(seq(A, B) %in% seq(C, D), seq(C, D) %in% seq(A, B))) |> 
  mutate(intersects = if_else(n_within == 0, 0, 1))
  
sum(intersects$intersects)