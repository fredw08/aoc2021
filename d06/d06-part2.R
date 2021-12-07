############################
# part 2
############################
# input <- "3,4,3,1,2"
input <- readLines("input.txt")
input <- unlist(lapply(strsplit(input, ","), as.integer))

# drop array approach
# -------------------
# step <- function(input) {
#   zero_now <- input == 0                              # record all zero (serial of TRUE/FALSE)
#   input[!zero_now] <- input[!zero_now] - 1            # minus 1 for non 0
#   input[zero_now] <- 6                                # reset to 6            (re-production step for 0)
#   input <- c(input, rep(8, length(input[zero_now])))  # append more of each 0 (re-production step for 0)
#   input
# }

# form a matrix to hold existing fish on each of day cycle
d <- matrix(rep(0, 9)) # 1-9 means fish on that days (same to 0-8)

for (i in seq(9)) {
  d[i] = length(input[input==i-1])
}

for (days in seq(256)) {
  zero_now <- unlist(d[1])            # remember how many '0' fish
  d[1:8]   <- unlist(d[2:9])          # shift one day for day8~day1 to day7~day0
  d[7]     <- unlist(d[7]) + zero_now # reset 0's to 6's day (put back day6)
  d[9]     <- zero_now                # reproduction a new fish
}

options("digits" = 22) # extend R to show more digits, default is 7
sum(d) # total fish now
