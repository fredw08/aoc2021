#!/usr/bin/env Rscript
input <- scan("input.txt")

# part 1
result <- data.frame(x=input[2:length(input)],
                     y=input[1:length(input)-1])
result <- cbind(result, diff=result$x-result$y)
print(sum(result$diff > 0, na.rm=TRUE)) # 1602

# part 2
result <- data.frame(x=input[3:length(input)],
                     y=input[2:(length(input)-1)],
                     z=input[1:(length(input)-2)])

result <- cbind(result, a=(result$x + result$y + result$z))

result <- data.frame(a=result$a[2:length(result$a)],
                     b=result$a[1:length(result$a)-1])

result <- cbind(result, diff=result$a-result$b)
print(sum(result$diff > 0, na.rm=TRUE)) # 1633
