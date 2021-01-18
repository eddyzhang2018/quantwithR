# Problem Statement: https://www.hackerrank.com/challenges/s10-weighted-mean/problem
# Difficulty: Easy
# Max Score: 30

- R code:

ipt <- matrix(scan("stdin")[-1], nrow = 2, byrow = TRUE)
opt <- sum(ipt[1,] * ipt[2,])/sum(ipt[2,])
cat(format(round(opt, 1), nsmall = 1))
