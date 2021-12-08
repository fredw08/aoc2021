# input <- "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
input <- readLines("input.txt")

digits <- c("abcefg",
            "cf",
            "acdeg",
            "acdfg",
            "bcdf",
            "abdfg",
            "abdefg",
            "acf",
            "abcdefg",
            "abcdfg")

# find unique digit
digits_len <- unlist(lapply(digits, \(x) nchar(x)))
unique_len <- unlist(lapply(digits_len, \(x) length(digits[digits_len==x]) == 1))
unique_digit <- digits[unique_len]
unique_digit <- unlist(lapply(unique_digit, "nchar")) # actual 1/4/7/8

# make input as length of each word
input <- lapply(input, \(x) strsplit(x, " "))
input <- lapply(input, \(x) nchar(x[[1]]))

# look for any 1/4/7/8 digits
match_of_last_4 <- unlist(lapply(input, \(x) tail(unlist(x),4) %in% unique_digit))
length(match_of_last_4[match_of_last_4 == TRUE]) # 294

