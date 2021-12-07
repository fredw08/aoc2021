input = File.open('input.txt').read.split("\n").map(&:to_i)

## A)---------------------
result = []
input[1..-1].zip(input[0..-2]) do |x, y|
  result += [x - y]
end
puts "A: #{result.count(&:positive?)}" # 1602

## B)---------------------
result  = []
input_b = input.each_cons(3).to_a
input_b = input_b.map(&:sum)

input_b[1..-1].zip(input_b[0..-2]) do |x, y|
  result += [x.to_i - y.to_i]
end

puts "B: #{result.count(&:positive?)}" # 1633
