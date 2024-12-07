library(readr)
library(stringr)
library(purrr)
library(tibble)

labmap <- read_table("2024-12-06/input.txt", col_names = F) |>
    map(\(x) strsplit(x, split = "")) |>
    unlist() |> matrix(nrow = 130, byrow = T)

colnames(labmap) <- c(1:ncol(labmap))
rownames(labmap) <- c(1:nrow(labmap))

mapFind <- function(matrixMap, item){
    return(matrixMap |> colnames() |> map( \(x) {
        matrixMap |> rownames() |> map(\(y) {
            if (matrixMap[y, x] == item)
                return(c(y, x))
            else
                return(NULL)
        })
    }) |> flatten() |> compact() )
}


mapMove <- function(matrixMap, pos, direction){
    newPos <- pos + direction
    if (newPos[1] > matrixMap |> nrow() | newPos[1] < 1 | newPos[2] > matrixMap |> ncol() | newPos[2] < 1)
        return(list(return = FALSE, map = matrixMap, position = pos,  direction = direction))
    if (matrixMap[newPos[1],newPos[2]] == "#") {
        rotation <- matrix(c(0,1,-1,0), nrow = 2)
        direction <- as.vector(direction %*% rotation)
        newPos <- pos
        }
    else if (matrixMap[newPos[1],newPos[2]] != "X") {
        matrixMap[newPos[1],newPos[2]] = "X"
        }
    return(list(return = TRUE, map = matrixMap, position = newPos,  direction = direction))
}

guardPos <- mapFind(labmap, "^") |> unlist() |> as.numeric()
guardDirection <- c(-1,0)

oneMove <- mapMove(labmap, guardPos, guardDirection)
while (oneMove$return == TRUE){
    oneMove <- mapMove(oneMove$map, oneMove$position, oneMove$direction)
}

moves <- oneMove$map |> mapFind("X") |> length()
# fudge as didn't include starting point 😮
moves + 1
