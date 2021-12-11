# sample
input <-
"[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"
input <- unlist(strsplit(input, '\n'))

input <- readLines("input.txt") # real one

# find matched bracket and remove it
cleanup <- function(line) {
  d <- list()
  for (i in 1:length(line)) {
    d <- unlist(c(d, line[[i]])) # insert to the end
    if (
      all(tail(d, 2) == c("(", ")")) | all(tail(d, 2) == c("[", "]")) |
      all(tail(d, 2) == c("{", "}")) | all(tail(d, 2) == c("<", ">"))
    ) { d <- head(d, -2) } # remove if pair matched
  }
  d
}

first_illegal <- function(line) {
  d <- cleanup(line)
  r <- d[d==")"|d=="]"|d=="}"|d==">"][1] # first closing because it shouldn't have any closing after cleanup
  if (is.na(r)) 0 else r                 # return 0 for calc at later step
}

res <- lapply(input,
              function(line) {
                l <- first_illegal(unlist(strsplit(line, ""))) # find all first illegal bracket
                l[l==")"] <- 3     # update marks for calc
                l[l=="]"] <- 57
                l[l=="}"] <- 1197
                l[l==">"] <- 25137
                lapply(l, \(x) as.integer(x)) # fix incorrect data type
              })

# part 1
sum(unlist(res)) # 323691

# part 2
# clean up, remove illegal line by reference "valid_map" (series of TRUE/FALSE indicate any closing bracket exist)
res       <- lapply(input, \(x) cleanup(unlist(strsplit(x, ""))))
valid_map <- unlist(lapply(res, \(x) (length(x[x==")"]) + length(x[x=="]"]) + length(x[x=="}"]) + length(x[x==">"])) == 0 ))
res       <- res[valid_map]

# reverse the order and replace the bracket with target score for latest calculation
res <- lapply(res,
              function(x) {
                x <- rev(unlist(x))
                x[x=="("] <- 1
                x[x=="["] <- 2
                x[x=="{"] <- 3
                x[x=="<"] <- 4
                lapply(list(x), as.integer)})

# follow the instruction to multiple all together
res <- lapply(res,
              function(x) {
                total <- 0
                x <- unlist(x)
                for(i in 1:length(x)) { total <- (total*5 + x[i]) }
                total
              })

# find the middle value
sort_res <- sort(unlist(res))
sort_res[ceiling(length(res) / 2)] # 2858785164