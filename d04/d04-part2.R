input  <- readLines("input.txt")

# prepare input + ready the basic of boards (total 500's element)
draws  <- do.call("as.integer", strsplit(input[1], ","))
boards <- input[-1]
boards <- boards[!boards == ""]
boards <- lapply(boards, function(x){ gsub("  ", " ", gsub("^ +", "", x)) })
boards <- matrix(boards, ncol=5, byrow = TRUE)

# rebuild each boards into 5x5 board + fix empty space + convert into integer
n <- list()
for (i in seq(nrow(boards))) {
  d <- do.call("rbind", strsplit(as.character(boards[i, ]), " "))
  n[[i]] <- apply(d, 2, as.integer)
}

# have some temp var
draw_now <- list()
board_now <- list()
check <- list()

all_won_boards <- list() # extra num to hold all won boards
last_draw_now <- list()

# loop each draws and each boards, if hit any col/row all match, stop it
for (i in seq(length(draws))) {
  draw_now <- draws[1:i]
  for (j in seq(length(n))) {
    board_now <- n[j]
    
    check_list <- c(
     lapply(board_now, function(x) { all(x[1,] %in% draw_now) }),
     lapply(board_now, function(x) { all(x[2,] %in% draw_now) }),
     lapply(board_now, function(x) { all(x[3,] %in% draw_now) }),
     lapply(board_now, function(x) { all(x[4,] %in% draw_now) }),
     lapply(board_now, function(x) { all(x[5,] %in% draw_now) }),
     
     lapply(board_now, function(x) { all(x[,1] %in% draw_now) }),
     lapply(board_now, function(x) { all(x[,2] %in% draw_now) }),
     lapply(board_now, function(x) { all(x[,3] %in% draw_now) }),
     lapply(board_now, function(x) { all(x[,4] %in% draw_now) }),
     lapply(board_now, function(x) { all(x[,5] %in% draw_now) })
    )
    # when hit store board num if it not already exists
    if(any(check_list) & !(j %in% all_won_boards))
      all_won_boards <- c(all_won_boards, j)
  }
  if(any(check_list)) last_draw_now <- draw_now
  if(length(all_won_boards) == length(n)) break
}

last_before_won <- all_won_boards[length(all_won_boards)]
last_before_won

board_now <- n[unlist(last_before_won)]
draw_now  <- last_draw_now

logical  <- lapply(board_now, function(x) {x %in% draw_now})
filtered <- unlist(board_now)[!unlist(logical)]
sum(filtered)* draw_now[length(draw_now)]
