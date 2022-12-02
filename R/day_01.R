library(dplyr)

input <- readLines("input/input-day01")
input_num <- as.numeric(input)

elf_start = 1
elf_no <- vector(mode = "character", length = length(input_num))
for (i in seq_along(input_num)) { 
  if (!is.na(input_num[[i]])) {
    elf_no[[i]] <- paste0("elf_", elf_start)
    elf_start = elf_start
  } else {
    elf_no[[i]] <- paste("not_elf")
    elf_start = elf_start + 1
  }
}

elves_n_calories <- input_num |> 
  bind_cols(elf_no) |> 
  rename(calories = `...1`, elf_id = `...2`) |> 
  group_by(elf_id) |> 
  summarise(total_calories = sum(calories))

max_calories <- slice_max(elves_n_calories, order_by = total_calories, n = 1)
max_calories_3 <- slice_max(elves_n_calories, order_by = total_calories, n = 3)
sum(max_calories_3$total_calories)  
