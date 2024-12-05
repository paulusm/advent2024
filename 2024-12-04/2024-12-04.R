library(readr)
library(stringr)
library(purrr)
library(omnibus)

input <- read_table("2024-12-04/test.txt", col_names = F) |>
    map(\(x) strsplit(x, split = "")) |>
    unlist() |> matrix(nrow = 10, byrow = T)

countXmas <- function(mat){
    return(mat |> as_tibble() |>
    unite("line", sep = "", na.rm = T) |> str_match_all("(?=(XMAS|SAMX))") |> pluck(1) |> nrow())
}

resizeRotate45 <- function(mat){
    # For diagonals, we need to resize the matrix first, then transform it
    transformed <- matrix(0, nrow = nrow(mat) * 2 + 1 , ncol = ncol(mat) * 2 + 1)
    position <- nrow(mat) / 2
    transformed[position:(nrow(input) + position - 1), position:(ncol(input) + position - 1)] <- mat
    rotated <- matrix(0, nrow = nrow(transformed) , ncol = ncol(transformed) )
    for (i in 1:(nrow(transformed) - 1))
        for (j in 1:(ncol(transformed) - 1))
            print(c(i + j + 1, -i + j + 10))
            rotated[i + j + 1, -i + j + 10] <- transformed[i, j]

    return(rotated)
}
foo <- input |> resizeRotate45()

horizontal <- input |> countXmas()
vertical <- input |> rotateMatrix(90) |> countXmas()

diagonal1 <- resized |> rotateMatrix(45) |> countXmas()
diagonal2 <- resized |> rotateMatrix(-45) |> countXmas()

c(horizontal, vertical, diagonal1, diagonal2)

