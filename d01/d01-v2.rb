# simplified version
input = File.open('input.txt').read.split("\n").map(&:to_i)

## A)---------------------
result = input.each_cons(2).count { |x, y| x < y }
puts "A: #{result}" # 1602

## B)---------------------

result = input.each_cons(3).map(&:sum).each_cons(2).count { |x, y| x < y }
puts "B: #{result}" # 1633
