library(readr)
library(purrr)
library(dplyr)
library(tidyr)

input <- read_table("2024-12-02/input.txt", col_names = FALSE, show_col_types = FALSE, )

# Part One

# Had to cheat by swapping a row in the input file. If the first row didn't have 8 cols
#it only read in 7

#242

# Part Two