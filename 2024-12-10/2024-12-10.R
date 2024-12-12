library(readr)
library(furrr)
library(purrr)
library(dplyr)
library(stringr)
library(tidyr)
future::plan(multicore)

# TODO: refactor lines 10-16  to simplify and remove kludges
landscape <- read_file("2024-12-10/input.txt") |> str_split("\n") |> unlist() |>
    str_split("") |> as_tibble( .name_repair = "universal")

landscape <- landscape |> mutate(y = as.character(row_number())) |>
    pivot_longer(cols = ...1:...57, names_to = "x", values_to = "value")
landscape$x <- landscape$x |> str_replace("\\.\\.\\.","")
landscape <- landscape |> mutate(across(everything(), as.numeric))


nextLevel <- function(trailid, i, j, val){
   search <- c(1,-1, 0, 0)
   search |> map2(rev(search), \(a,b) {
       landscape |> filter(x == i + a & y == j + b & value == val + 1) |>
           mutate(trailid = trailid)
   }) |> bind_rows()
}

iterateLevel <- function(stateTibble) {
   resultState <- stateTibble |> future_pmap(\(trailid, x, y, value) nextLevel(trailid, x, y, value))
   return(resultState |> reduce(bind_rows))
}

trails <- landscape |> filter(value == "0") |> mutate(trailid = row_number())
for (i in c(1:9)){
    trails <- iterateLevel(trails)
}
# part one
nrow(trails |> distinct())

# part two
nrow(trails)