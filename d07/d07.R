input <- "16,1,2,0,4,2,7,1,2,14"
input <- readLines("input.txt")
input <- unlist(lapply(strsplit(input, ","), as.integer))

max_h <- max(input)
min_h <- min(input)

# part 1
d <- list()
for (i in min_h:max_h) {
  d <- c(d, sum(abs(input - i))) # count every possible position and count total fuel
}
min(unlist(d)) # 336120

# part 2
d <- list()
for (i in min_h:max_h) {
  d <- c(d,
         sum(
           # instead of 1 step at a time, it will 1 more step at each time (e.g. 4 step = 4+3+2+1)
           # this is a binomial coefficient, can be found by (n^2 + n)/2
           ( (abs(input - i))^2 + abs(input - i) ) / 2
         ))
}
min(unlist(d)) # 96864235