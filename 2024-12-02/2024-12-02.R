library(readr)
library(purrr)
library(dplyr)
library(tidyr)

input <- read_table("2024-12-02/input.txt", col_names = FALSE, show_col_types = FALSE, )

# Part One

# Had to cheat by swapping a row in the input file. If the first row didn't have 8 cols
#it only read in 7

#242

parse <- function(row){
    row <- row[!is.na(row)]
    row <- as.list(row)
    names(row) <- NULL
    unsafe <- c()
    parseResult <- (row |> imap( \(x, idx){
        if(idx == length(row)) return( idx)
        y <-  row[[idx + 1]]
        if(idx > 1) z <-  row[[idx - 1]] else z = NA
        if(abs(x - y) > 3) unsafe <<- c(unsafe,idx + 1)
        if( x == y) unsafe <<- c(unsafe,idx + 1)
        if(idx > 1){
           if((y > x & z > x) | (y < x & z < x)) unsafe <<- c(unsafe,idx)
        }
        return(idx)
    }))
    return(unsafe)
}

#print(parse(input[4,]))

resultList <- input |> group_by(row_number()) |>
    group_map(\(x, i) {
        result <- (parse(x))
        return(length(result) == 0)
    })


input$safe <- resultList
resultList[resultList == TRUE] |> length() |> print()

# Part Two, not working
# 278 is too low, 316 is too high!
input <- read_table("2024-12-02/input.txt", col_names = FALSE, show_col_types = FALSE, )

resultList <- input |> group_by(row_number()) |>
    group_map(\(x, i) {
        result <- (parse(x))
        return(length(result) < 2)
    })


input$safe <- resultList
resultList[resultList == TRUE] |> length() |> print()