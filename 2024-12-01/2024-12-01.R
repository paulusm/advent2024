library(readr)
library(dplyr)
library(tidyr)

# Part One
# input <- read_table("2024-12-01/test.txt", col_names = FALSE)
input <- read_table("2024-12-01/input.txt", col_names = FALSE, show_col_types = FALSE)
left <- input$X1 |> sort(method = "radix")
right <- input$X2 |> sort(method = "radix")
answer <- sum(abs(left - right))
print(answer)

# Part Two
freqs <- table(input$X2)
input$freq <- freqs[as.character(input$X1)] |> as.double()
answer <- sum(input$X1 * input$freq |> replace_na(0))
print(answer)