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

# Part Two, TODO: Refactor!
# 278 is too low
input <- read_table("2024-12-02/input.txt", col_names = FALSE, show_col_types = FALSE, )

resultList <- input |> group_by(row_number()) |>
    group_map(\(x, i) {
        x <- x[!is.na(x)]
        #print(x)
        parse1 <- x |> reduce2(c(x[3:length(x)],999), \(x, y, z, pos = 0){
            pos <- pos + 1
            print(pos)
            if(abs(x - y) > 3){return(done(list(FALSE, pos)) )}
            if( x == y) {return(done(list(FALSE, pos)))}
            if(z == 999) return(done(list(TRUE, pos)))
            if((y > x & z < y) | (y < x & z > y)) {return(done(list(FALSE, pos)))}
            return (y)
        })

        if(parse1[[1]] == FALSE){
            pos <- parse1[[2]]
            print(pos)
            x <- x[-pos]
            print(x)
            parse2 <<- x |> reduce2(c(x[3:length(x)],999), \(x, y, z, pos = 0){
                if(abs(x - y) > 3){return(done(list(FALSE, pos)) )}
                if( x == y) {return(done(list(FALSE, pos)))}
                if(z == 999) return(done(list(TRUE, pos)))
                if((y > x & z < y) | (y < x & z > y)) {return(done(list(FALSE, pos)))}
                return (y)
            })
        }
        return(parse1[[1]] | parse2[[1]])
    })

input$safe <- resultList
resultList[resultList == TRUE] |> length() |> print()