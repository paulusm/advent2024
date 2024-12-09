library(readr)
library(stringr)
library(dplyr)
library(purrr)

input<- read_file("2024-12-09/test.txt") |> str_split("") |> pluck(1)
flatSystem <- input |> imap(\(x, i) {
    position = i - 1
    fileno = floor(position / 2)
    if (i %% 2 == 0) {
        # This is freespace
        return(rep(".", x))
    }
    else {
        return(rep(as.character(fileno), x))
    }
}) |> flatten_chr()

moveOne <- function (diskmap, fromPos, toPos){
    diskmap[toPos] <<- diskmap[fromPos]
    return(diskmap[-length(diskmap)])
}

spaces <- flatSystem |> imap(\(x,i) ifelse(x==".",i,NA)) |> unlist() |> na.omit()

spaces |> map(\(x) flatSystem <<- moveOne(flatSystem, flatSystem[length(flatSystem)], x))