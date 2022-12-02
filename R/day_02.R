library(tidyr)
library(dplyr)

input <- as.data.frame(readLines("input/input-day02"))

# part 1
scores_part1 <- input |> 
  separate(col = 1, into = c("elf", "me"), sep = " ") |> 
  mutate(across(everything(), ~ case_when(. %in% c("A", "X") ~ "rock",
                                          . %in% c("B", "Y") ~ "paper",
                                          . %in% c("C", "Z") ~ "scissors"))) |> 
  mutate(type_points = case_when(me == "rock" ~ 1,
                                 me == "paper" ~ 2,
                                 me == "scissors" ~ 3)) |> 
  mutate(outcome = case_when(elf == me ~ "draw",
                             elf == "scissors" & me == "rock" ~ "win",
                             elf == "rock" & me == "paper" ~ "win",
                             elf == "paper" & me =="scissors" ~ "win",
                             TRUE ~ "lose")) |> 
  mutate(outcome_points = case_when(outcome == "win" ~ 6,
                                    outcome == "draw" ~ 3,
                                    outcome == "lose" ~ 0)) |> 
  mutate(total_points = type_points + outcome_points)
  
sum(scores_part1$total_points)

# part 2
scores_part2 <- input |> 
  separate(col = 1, into = c("elf", "outcome"), sep = " ") |> 
  mutate(me = case_when(outcome == "Y" ~ elf,
                        outcome == "X" & elf == "A" ~ "C",
                        outcome == "X" & elf == "B" ~ "A",
                        outcome == "X" & elf == "C" ~ "B",
                        outcome == "Z" & elf == "A" ~ "B",
                        outcome == "Z" & elf == "B" ~ "C",
                        outcome == "Z" & elf == "C" ~ "A")) |> 
  mutate(type_points = case_when(me == "A" ~ 1,
                                 me == "B" ~ 2,
                                 me == "C" ~ 3)) |> 
  mutate(outcome_points = case_when(outcome == "X" ~ 0,
                                    outcome == "Y" ~ 3,
                                    outcome == "Z" ~ 6)) |> 
  mutate(total_points = type_points + outcome_points)


sum(scores_part2$total_points)
