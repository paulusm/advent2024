library(readr)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)


input<- read_file("2024-12-09/input.txt") |> str_split("") |> pluck(1)
fileSystem <- input |> imap(\(x, i) {

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

unused <- fileSystem |> imap(\(x,i) ifelse(x==".",i,NA)) |> unlist() |> na.omit()
valid  <- fileSystem |> imap(\(x,i) ifelse(x!=".",i,NA)) |> unlist() |> na.omit() |> rev()
compacted <- fileSystem
unused |> imap(\(x,i){
    if(which(compacted ==".")[[1]] < length(valid)){
        compacted[x] <<- compacted[valid[i]]
        compacted[valid[i]] <<- "."
    }
})
print(compacted)
compacted[compacted !="."] |> imap(\(x,i) as.numeric(x) * (i-1) )|> reduce(sum) |> format(scientific = F)