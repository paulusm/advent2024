# Part One
# Test
left <- c(3,4,2,1,3,3) |> sort()
right <- c(4,3,5,3,9,3) |> sort()

input <- read_table("2024-12-01/input.txt", col_names = FALSE)
left <- input$X1 |> sort(method = "radix")
right <- input$X2 |> sort(method = "radix")
answer <- sum(abs(left - right))
print(answer)

# Part Two
# Test
library(purrr)
library(dplyr)

testdf <- tibble(
left = c(3,4,2,1,3,3),
right = c(4,3,5,3,9,3)
)
testdf <- testdf |> mutate(count = map(left, \(x) testdf |> filter(right == x) |> nrow()) )
str(testdf)
print(sum(as.numeric(testdf$count) * testdf$left))

input <- input |> mutate(count = map(X1, \(x) input |> filter(X2 == x) |> nrow() ) )
answer2 <- sum(as.numeric(input$count) * input$X1)
print(answer2)
