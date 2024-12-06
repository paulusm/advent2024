library(readr)

input <- read_file("2024-12-05/input.txt") |> str_split("\\n\\n")

rules <- input[[1]][1] |> str_split("\n") |> pluck(1)
pages <- input[[1]][2] |> str_split("\n") |> pluck(1)

regexes <- rules |> map(\(x) paste0(".*", (x |>
                    str_split("\\|") |> pluck(1) |> str_flatten(collapse = ".*")),".*"))
pages |> map(\(x) {
    compliant <- T
    rules |> imap(\(y, i) {
        numbers <- y |> str_split("\\|") |> pluck(1)
        if (grepl(numbers[1], x) & grepl(numbers[2], x) )
        {
            # rule match
            if (x |> str_detect(regexes[[i]])){
                # rule adhered
            }
            else{
                # rule broken
                compliant <<- F
            }
        }
    })
    if(compliant)
        return((x |> substring(str_length(x)/2,str_length(x)/2+1)))
    else
        return(0)
}) |> as.numeric() |> sum() |> print()