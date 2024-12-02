library(readr)
library(purrr)

input <- read_table("2024-12-02/test.txt", col_names = FALSE, show_col_types = FALSE)

input |> group_by(row_number()) |>
    group_map(\(x) {
        left <- x$X1 |> sort(method = "radix")
        right <- x$X2 |> sort(method = "radix")
        sum(abs(left - right))
    }) |> print()
