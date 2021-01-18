# Problem Statement: https://www.hackerrank.com/challenges/s10-basic-statistics/problem
# Difficulty: Easy
# Score: 30

R code:

input <- scan("stdin")[-1]

cat(mean(input), median(input), as.numeric(names(which.max(table(input)))), sep = "\n")
