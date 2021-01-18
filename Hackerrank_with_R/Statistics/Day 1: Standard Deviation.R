# Problem Statement: https://www.hackerrank.com/challenges/s10-standard-deviation/problem
# Difficulty: Easy
# Max Score: 30

- R Code:

ipt <- scan("stdin")[-1]
opt <- sqrt(sd(ipt)^2*(length(ipt)-1)/length(ipt))
cat(format(round(opt, 1), nsmall = 1))
