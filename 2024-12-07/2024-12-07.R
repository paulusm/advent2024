library(readr)
library(stringr)
library(purrr)
library(gtools)
library(tibble)

input <- read_lines("2024-12-07/test.txt") |> str_split(" ") |>
    map(\(x) x |> str_replace_all(":",""))

# Part One
input |> map(\(x) {
     target <- as.numeric(x[1])
     args <- length(x) - 2
     ops <- c("+","*")
     options <- permutations(2, args, ops, repeats.allowed = TRUE) |> t() |> as.tibble()

     combi <- options |> map(\(y){
         results <- x[2:length(x)] |> imap(\(z, i){
             paste0(z,")", ifelse(i > args,"",y[i]))
         }) |> str_flatten(collapse = "")
         results <- paste0(strrep("(", args + 1) ,results)
         eval(parse(text = results)) |> as.numeric()
     })
     combi  |>  map(\(x) if (x == target) x else 0) |> reduce(max)
}) |> reduce(sum) |> format(scientific = F)

# Part Two - Not working YET
input[5] |> map(\(x) {
    target <- as.numeric(x[1])
    args <- length(x) - 2
    ops <- c("+","*","|")
    options <- permutations(3, args, ops, repeats.allowed = TRUE) |> t() |> as.tibble()
    combi <- options |> map(\(y){
        results <- x[2:length(x)] |> imap(\(z, i){
            paste0(z, ifelse(i > args,"",y[i]))
        }) |> str_flatten(collapse = "")
    part <- results |> str_extract("\\d[*+|]\\d")
    print(part)
    #while (!is.na(part)) {
    results <- results |> str_replace(part, eval(parse(text = part|> str_replace("\\|",""))) |> as.character())
    print(results)
    part <- results |> str_extract("\\d[*+|]\\d") |> str_replace("\\|","")
    print(part)
    #}
    results |> as.numeric()
    })
    combi  #|>  map(\(x) if (x == target) x else 0) |> reduce(max)
}) #|> reduce(sum) |> format(scientific = F)