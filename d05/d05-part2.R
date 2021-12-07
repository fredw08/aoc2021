# read all line and grab max_x and max_y
input <- readLines("input.txt")
input <- lapply(input, function(x){ strsplit(gsub(" -> ", ",", x), ",") })
input <- matrix(input, ncol=4, byrow = TRUE)

for (i in seq(length(input))) {
  input[[i]] <- lapply(unlist(input[i]), as.integer)
}

max_x <- max(apply(input, c(1,2), function(x) max(unlist(x)[c(1,3)])))
max_y <- max(apply(input, c(1,2), function(x) max(unlist(x)[c(2,4)])))

# form a matrix with by max_x / max_y
board <- matrix(0, nrow=max_x, ncol=max_y)

# loop each line and parse
for (i in seq(length(input))) {
  x1 <- unlist(input[[i]][1])
  y1 <- unlist(input[[i]][2])
  x2 <- unlist(input[[i]][3])
  y2 <- unlist(input[[i]][4])
  
  # add to board if line is hori/vert, something like:
  # board[1, c(1:3)] <- unlist(lapply(board[1, c(1:3)], function(x) x + 1))
  if (x1==x2) {
    y_list <- c(min(y1,y2):max(y1,y2)) # ex. c(100:200) -> 100~200 element
    board[x1, y_list] <- unlist(lapply(board[x1, y_list], function(x) x + 1))
  }
  if (y1==y2) {
    x_list <- c(min(x1,x2):max(x1,x2))
    board[x_list, y1] <- unlist(lapply(board[x_list, y1], function(x) x + 1))
  }
  if (abs(x1-x2)==abs(y1-y2)) { # diagonal line detection
    points <- mapply(list, c(x1:x2), c(y1:y2), SIMPLIFY=F) # handle it like zip
    points <- matrix(unlist(points), ncol=2, byrow = TRUE) # form another matrix to step

    for (j in seq(nrow(points))) {
      x_point <- points[j,1]
      y_point <- points[j,2]
      board[x_point, y_point] <- unlist(board[x_point, y_point]) + 1
    }
  }
}

# count more than 2
length(board[board >= 2])
