library(readr)
library(stringr)
library(purrr)

input <- read_table("2024-12-04/test.txt", col_names = F) |>
    map(\(x) strsplit(x, split="")) |>
    unlist() |> matrix(nrow = 10, byrow = T)


