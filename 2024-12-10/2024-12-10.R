library(readr)
library(purrr)
library(dplyr)
library(stringr)
library(tidyr)

landscape <- read_file("2024-12-10/test.txt") |> str_split("\r\n") |> unlist() |> str_split("") |> as_tibble( .name_repair = "unique")
# convert to three column tibble of x, y, value
landscape <- landscape |> mutate(y = as.character(row_number())) |>
    pivot_longer(cols = ...1:...8, names_to = "x", values_to = "value")
landscape$x <- landscape$x |> str_replace("\\.\\.\\.","")
