library(readr)
library(dplyr)
library(purrr)
library(furrr)
library(stringr)
future::plan(multicore)
#512 72 2024 2 0 2 4 2867 6032
stones <- read_table("2024-12-11/input.txt", col_names = F) |> t()

blink <- function(st){
    st |> future_map( \(x) {
        if(as.double(x) == 0) return("1")
        isEven <- nchar(x) %% 2 == 0
        if(isEven) {
            theLen <- nchar(x)
            # need to remove leading zeros from right half of split
            return(c(substring(x, 1, theLen/2), substring(x, theLen/2+1) |>
                         str_replace("^0+(?=\\d+)","")))
        }
        else return (as.character(as.double(x) * 2024))
    }) |> flatten()
}

for(i in c(1:25)){
    print(i)
    stones <- blink(stones)
}
print(length(stones))