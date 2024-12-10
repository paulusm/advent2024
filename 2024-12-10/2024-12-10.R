library(readr)
library(furrr)
library(purrr)
library(dplyr)
library(stringr)
library(tidyr)
#plan(multiprocess)

landscape <- read_file("2024-12-10/test.txt") |> str_split("\n") |> unlist() |>
    str_split("") |> as_tibble( .name_repair = "unique")

landscape <- landscape |> mutate(y = as.character(row_number())) |>
    pivot_longer(cols = ...1:...8, names_to = "x", values_to = "value")
landscape$x <- landscape$x |> str_replace("\\.\\.\\.","")
landscape <- landscape |> mutate(across(everything(), as.numeric))

heads <- landscape |> filter(value == "0")

nextLevel <- function(i, j, val){
   search <- c(1,-1, 0, 0)
   search |> map2(rev(search), \(a,b) {
       landscape |> filter(x == i + a & y == j + b & value == val + 1)
   }) |> bind_rows()
}

heads |> future_pmap(\(x, y, value) nextLevel(x, y, value))

