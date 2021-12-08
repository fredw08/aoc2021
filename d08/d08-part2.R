input <- readLines("input.txt")

sum_of_last_4 <- function(line) {
  # break down the line and find head 10 and last 4 elements
  line       <- lapply(line, \(x) strsplit(x, " "))
  head_10    <- head(unlist(line), 10)
  last_4     <- tail(unlist(line), 4)
  digits_len <- unlist(lapply(head_10, \(x) nchar(x)))

  # easy one to look (unique)
  d1 <- head_10[digits_len == 2]
  d7 <- head_10[digits_len == 3]
  d4 <- head_10[digits_len == 4]
  d8 <- head_10[digits_len == 7]

  # deduction for 6 digits first (i.e. 0/6/9)
  d9 <- head_10[digits_len == 6 &
                 unlist(lapply(head_10, \(x) all(
                   unlist(strsplit(paste(d1,d4,d7, sep=""), "")) %in% unlist(strsplit(x, "")))))]
  d0 <- head_10[digits_len == 6 & head_10 != d9 &
                 unlist(lapply(head_10, \(x) all(
                   unlist(strsplit(paste(d1,d7, sep=""), "")) %in% unlist(strsplit(x, "")))))]
  d6 <- head_10[head_10 != d0 & head_10 != d9 & digits_len == 6]

  # now for the 5 digits element (i.e. 2/3/5)
  d3 <- head_10[digits_len == 5 &
                 unlist(lapply(head_10, \(x) all(
                   unlist(strsplit(paste(d1,d7, sep=""), "")) %in% unlist(strsplit(x, "")))))]
  # critical corner to determine d2 because d2 and d5 is hard to tell yet
  lfc <- unlist(strsplit(d8,""))[!unlist(strsplit(d8,"")) %in% unlist(strsplit(d9,""))]

  d2 <- head_10[digits_len == 5 &
                 unlist(lapply(head_10, \(x) all(
                   c(lfc) %in% unlist(strsplit(x, "")))))]

  d5 <- head_10[digits_len == 5 & head_10 != d2 & head_10 != d3]

  # once d0~d9 is found, times to calculate tail 4 elements
  total      <- list()
  digit_list <- c(d0,d1,d2,d3,d4,d5,d6,d7,d8,d9)
  value_list <- c(0:9)

  for (i in 1:4) {
    total <- c(total,
                 value_list[
                   unlist(lapply(digit_list, \(x)
                                all(unlist(strsplit(last_4[i], "")) %in% unlist(strsplit(x, ""))) &
                                 nchar(x) == nchar(last_4[i])
                 ))])
  }
  d <- unlist(total)
  d[1]*1000 + d[2]*100 + d[3]*10 + d[4] # difficult to convert "1234" into 1234 numeric here
}

sum(unlist(lapply(input, \(x) sum_of_last_4(x)))) # 973292