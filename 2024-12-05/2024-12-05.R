library(readr)
library(purrr)
library(dplyr)
library(stringr)

input <- read_file("2024-12-05/input.txt") |> str_split("\\n\\n")

rules <- input[[1]][1] |> str_split("\n") |> pluck(1)
pages <- input[[1]][2] |> str_split("\n") |> pluck(1)

regexes <- rules |> map(\(x) paste0(".*", (x |>
                    str_split("\\|") |> pluck(1) |> str_flatten(collapse = ".*")),".*"))

getCompliance <- function(pagesToTest, rulesList){
    compliantPages <- c()
    noncompliantPages <- c()
    rulesthatapply <- list()
    pagesToTest |> imap(\(x, pagenum) {
        compliant <- T
        rulesList |> imap(\(y, i) {
            numbers <- y |> str_split("\\|") |> pluck(1)
            if (grepl(numbers[1], x) & grepl(numbers[2], x) )
            {
                if (!(x |> str_detect(regexes[[i]]))) {
                    # rule adhered
                    compliant <<- F
                    # rule match
                    rulesthatapply[[x]] <<- c(rulesthatapply[[x]], i)
                }
            }
        })
        if (compliant){
            compliantPages <<- c(compliantPages, x)
        }
        else{
            noncompliantPages <<- c(noncompliantPages, x)
        }
    })
    return(list(pass = compliantPages, fail = noncompliantPages, rulesFailed = rulesthatapply))
}

results <- getCompliance(pages, rules)

# Part One
results$pass  |>  map(\(x) x |> substring(str_length(x)/2,str_length(x)/2 + 1)) |>
                      as.numeric() |> sum() |> print()


# Part Two - Reordering can lead to new rules to be broken! Doh!
reOrderResults <- function(failedPages, rulesFailed){
    reorderedPages <- failedPages |> map(\(x) {
        reordered <- x
        rulesFailed[[x]] |> map(\(y) {
            reordered <<- reordered |> str_replace(rules[y] |> substr(1,2), rules[y] |> substr(4,5))
            reordered <<- reordered |> str_replace(rules[y] |> substr(4,5), rules[y] |> substr(1,2))
        })
        return(reordered )
    })
    testReordered <- getCompliance(reorderedPages, rules)
    if (length(testReordered$fail) > 0) {
        reReordered <- reOrderResults(testReordered$fail, testReordered$rulesFailed)
        return(c(reReordered, testReordered$pass))
    }
    return(reorderedPages)
}

reorderedResult <- reOrderResults(results$fail, results$rulesFailed)
reorderedResult |> map(\(x) x |> substring(str_length(x)/2,str_length(x)/2 + 1)) |> as.numeric() |> sum() |> print()