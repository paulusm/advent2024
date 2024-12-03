library(readr)
library(stringr)
library(purrr)

program <- read_file("2024-12-03/input.txt")

# Part One
matches <- program |> str_match_all("mul\\((\\d+),(\\d+)\\)")
print(sum(as.numeric(matches[[1]][,2]) * as.numeric(matches[[1]][,3])))

# Part Two, Not working Nasty!
# 1143376 too low
# 56375767 too low
program2 <- read_file("2024-12-03/input.txt")
dontsplit <- str_split(program2, "don't\\(\\)")
goodparts <- dontsplit[[1]][1]
badparts <- dontsplit[[1]][-1]
dosplit <- str_split(badparts, "do\\(\\)")
moregoodparts <- dosplit |> map(\(x) x[-1])
goodparts <- c(goodparts, moregoodparts)
print(goodparts)
goodparts |> map(\(x) {
    if(length(x) == 0) return(0)
    matches <- x |> str_match_all("mul\\((\\d+),(\\d+)\\)")
    return(sum(as.numeric(matches[[1]][,2]) * as.numeric(matches[[1]][,3])))
}) |> reduce(sum) |> print()
