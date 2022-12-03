library(purrr)
library(dplyr)

input <- readLines("input/input-day03")

# part 1
first <- substring(input, 1, nchar(input)/2)
second <- substring(input, nchar(input)/2 + 1, nchar(input))

get_match <- function(first_half, second_half, ...) {
  pattern <- substring(second_half, 1:nchar(second_half), 1:nchar(second_half))
  pattern |>
    set_names() |>
    map(grep, x = first_half, ignore.case = FALSE, value = TRUE) |>
    unlist() |>
    names() |> 
    unique()
}

matches <- unlist(map2(first, second, get_match))

reference <- data.frame(value = c(letters, LETTERS), priority = 1:52)

priorities <- matches |> 
  data.frame() |> 
  left_join(reference, by = c("matches" = "value"))

sum(priorities$priority)  

# part 2
first <- input[seq(1, length(input), 3)]
second <- input[seq(2, length(input), 3)]
third <- input[seq(3, length(input), 3)]

matches <- first |> 
  map2(second, get_match) |> 
  map2(third, get_match) |> 
  unlist() 
  
priorities <- matches |> 
  data.frame() |> 
  left_join(reference, by = c("matches" = "value"))

sum(priorities$priority)
