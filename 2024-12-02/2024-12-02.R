library(readr)
library(purrr)
library(dplyr)
library(tidyr)

input <- read_table("2024-12-02/input.txt", col_names = FALSE, show_col_types = FALSE, )

# Part One

# Had to cheat by swapping a row in the input file. If the first row didn't have 8 cols
#it only read in 7

resultList <- input |> group_by(row_number()) |>
    group_map(\(x, i) {
        x <- x[!is.na(x)]
        x |> reduce2(c(x[3:length(x)],999), \(x, y, z){
            if(abs(x - y) > 3){return(done(FALSE))}
            if( x == y) {return(done(FALSE))}
            if(z == 999) return(done(TRUE))
            if((y > x & z < y) | (y < x & z > y)) {return(done(FALSE))}
            return (y)
        })
    })

input$safe <- resultList
resultList[resultList == TRUE] |> length() |> print()

# Part Two - Not done

