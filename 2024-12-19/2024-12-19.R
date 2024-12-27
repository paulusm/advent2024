library(readr)
library(stringr)
library(dplyr)
library(purrr)

#273 too low

input <- read_lines("2024-12-19/input.txt")

towels <- input[1] |> str_split(",") |> map(str_trim) |> unlist()
combis <- input[3:length(input)]

# sort towels size reverse order
towels <- towels[order(-nchar(towels))]

fitCombi <- function(theCombi, theTowel){
    theFit <- str_extract(theCombi, paste0("^", theTowel))
    return(theFit)
}

result <- c()

for (combi in combis) {
    fit <- combi
    towelFit <- TRUE
    while (fit != "" & towelFit == TRUE) {
        fitLength <- nchar(fit)
        towelFit <- FALSE
        for (towel in towels) {
            resp <- fitCombi(fit, towel)
            if (!is.na(resp)) {
                fit <- str_replace(fit, resp, "")
                break
           }
        }
        if (nchar(fit) < fitLength) towelFit <- TRUE
    }
    if (fit == "") {
        result <- c(result, TRUE)
    } else {
        result <- c(result, FALSE)
    }
}
print(result)

print(length(result[result == TRUE]))