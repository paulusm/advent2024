library(readr)
library(dplyr)
library(purrr)
library(furrr)
library(stringr)
future::plan(multicore)
#512 72 2024 2 0 2 4 2867 6032
stones <- read_table("2024-12-11/input.txt", col_names = F) |> t() |> as.double()
#str_replace("^0+(?=\\d+)","")) |> 
blink <- function(st){
    st |> map( \(x) {
        if(x == 0) return(1)
        isEven <- nchar(x) %% 2 == 0
        if(isEven) {
            z <- as.character(x)
            theLen <- nchar(z)
            # need to remove leading zeros from right half of split
            return(c(substring(z, 1, theLen/2), substring(z, theLen/2+1) |>
                         as.double()))
        }
        else return (as.double(x) * 2024)
    }) |> flatten()
}

for(i in c(1:25)){
    print(i)
    stones <- blink(stones)
}
print(length(stones))