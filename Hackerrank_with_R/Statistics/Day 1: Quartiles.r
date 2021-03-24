# Problem Statement: https://www.hackerrank.com/challenges/s10-quartiles/problem
# Difficulty: Easy
# Max Score: 30

ipt <- sort(scan("stdin")[-1]) # scan and read the data on Hackerrank

len_ipt <- length(ipt)

lower_bound <- floor(len_ipt/2)
upper_bound <- ceiling(len_ipt/2) + 1

q1 <- median(ipt[1:lower_bound])
q2 <- median(ipt)
q3 <- median(ipt[upper_bound:len_ipt])

cat(q1, q2, q3, sep = "\n")
