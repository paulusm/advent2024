library(readr)
library(purrr)
library(dplyr)
library(tidyr)

input <- read_table("2024-12-02/input.txt", col_names = FALSE, show_col_types = FALSE, )


resultList <- input |> group_by(row_number()) |>
    group_map(\(x, i) {
        x[!is.na(x)] |> reduce2(c(x[3:length(x)],999), \(x, y, z){

            if(z == 999) return(done(TRUE))
            if(abs(x - y) > 3){return(done(FALSE))}
            if( x == y) {return(done(FALSE))}
            if((y > x & z < y) | (y < x & z > y)) {return(done(FALSE))}
            return (y)
        })
    })

resultList[resultList == TRUE] |> length() |> print()
