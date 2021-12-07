############################
# part 1
############################
# input <- "3,4,3,1,2"
input <- readLines("input.txt")
input <- unlist(lapply(strsplit(input, ","), as.integer))

step <- function(input) {
  zero_now <- input == 0                              # record all zero (serial of TRUE/FALSE)
  input[!zero_now] <- input[!zero_now] - 1            # minus 1 for non 0
  input[zero_now] <- 6                                # reset to 6            (re-production step for 0)
  input <- c(input, rep(8, length(input[zero_now])))  # append more of each 0 (re-production step for 0)
  input
}

# part 1
for (i in seq(80)) {
  input <- step(input)
  # cat("Loop ", i, " and length is ", length(input), "length of new fish: ", length(input[input == 8]), "\n")
}
# split at last step
length(input)
