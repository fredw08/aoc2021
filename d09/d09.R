# sample
input <-
  "2199943210
3987894921
9856789892
8767896789
9899965678"
input <- unlist(strsplit(input, '\n'))

# real one
input <- readLines("input.txt")

max_y <- nchar(input[1])
max_x <- length(input)
input <- unlist(lapply(input, \(x) strsplit(x, "")))
input <- unlist(lapply(input, \(x) as.integer(x)))

board <- matrix(input, ncol=max_y, byrow=TRUE)

low <- list()
for (x in 1:max_x) {
  for (y in 1:max_y) {
    low <- c(low,
             all(
               (if (y==1)     TRUE else board[x,y] < board[x,y-1]),   # L
               (if (y==max_y) TRUE else board[x,y] < board[x,y+1]),   # R
               (if (x==1)     TRUE else board[x,y] < board[x-1,y]),   # U
               (if (x==max_x) TRUE else board[x,y] < board[x+1,y])    # D
             )
    )
  }
}

low <- matrix(low, ncol=max_y, byrow=TRUE)

# part 1
sum(board[unlist(low)] + 1) # 541

# part 2
basin <- unlist(low) | unlist(board != 9) # low point OR not-9
basin_map <- basin
basin_map[basin_map == FALSE] <- 0 # form a map to hold all 0/[1-8]

basin_region_map <- basin_map               # form another map to record the region#
basin_region_map <- basin_region_map * 0    # reset to 0 for mapping

current_region <- 1

for (x in 1:max_x) {
  for (y in 1:max_y) {
    if (basin_map[x,y] == 1) { # only 1 need to concern

      if (y==1)     TRUE else { if (basin_region_map[x,y-1] != 0) { basin_region_map[x,y] <- basin_region_map[x,y-1] } } # follow L
      if (y==max_y) TRUE else { if (basin_region_map[x,y+1] != 0) { basin_region_map[x,y] <- basin_region_map[x,y+1] } } # follow R
      if (x==1)     TRUE else { if (basin_region_map[x-1,y] != 0) { basin_region_map[x,y] <- basin_region_map[x-1,y] } } # follow U
      if (x==max_x) TRUE else { if (basin_region_map[x+1,y] != 0) { basin_region_map[x,y] <- basin_region_map[x+1,y] } } # follow D

      # build a new region if non of it found, but not build a new first if L,U,R is 0, need to wait D
      if (basin_region_map[x,y] == 0) {
        basin_region_map[x,y] <- current_region
        current_region     <- current_region + 1
      }

      # advance to mark D just in case
      if (x==max_x) TRUE else {
        if ((basin_region_map[x,y] != 0 & basin_map[x+1,y] == 1)) {
          basin_region_map[x+1,y] <- basin_region_map[x,y]
        }
      }

      # delayed to mark U just in case
      if (x==1) TRUE else {
        if (basin_region_map[x,y] != 0 & basin_map[x-1,y] == 1 & basin_region_map[x-1,y] == 0) {
          basin_region_map[x-1,y] <- basin_region_map[x,y]
        }
      }
    }
  }
}

# 2nd round fix the duplicate region
for (x in 1:max_x) {
  for (y in 1:max_y) {
    if (basin_region_map[x,y] != 0) { # only 1 need to concern
      if (x==1) TRUE else {
        if (basin_region_map[x-1,y] != 0 & (basin_region_map[x-1,y] != basin_region_map[x,y]))
          basin_region_map[basin_region_map == basin_region_map[x,y]] <- basin_region_map[x-1,y]
      } # follow U

      if (y==1) TRUE else {
        if (basin_region_map[x,y-1] != 0 & (basin_region_map[x,y-1] != basin_region_map[x,y]))
          basin_region_map[basin_region_map == basin_region_map[x,y]] <- basin_region_map[x,y-1]
      } # follow L
    }
  }
}

# base on region map, build a list and aggregate the region by count
basin_region <- unlist(matrix(basin_region_map, ncol=1))[,1]
basin_region <- basin_region[basin_region != 0]
basin_region <- data.frame(region=basin_region)

count_by_region <- aggregate(basin_region$region, by=list(basin_region$region), FUN=length)
top_3_regions   <- (count_by_region$x[order(count_by_region$x, decreasing = TRUE)][1:3])

top_3_regions
top_3_regions[1]*top_3_regions[2]*top_3_regions[3] # 847504

# basin_map
# basin_region_map

# basin_map[1:30, 1:20]
# basin_region_map[1:30, 1:20]





