# Make a sequence of 100 x-values going from -4*sigma to 4*sigma
xvals <- seq(from = -4*sigma, to = 4*sigma, length.out = 100)

# Compute the density of a N(mu, sigma^2) distribution at xvals
ndens <- dnorm(xvals, mean = mu, sd = sigma)

# Plot ndens against xvals
plot(xvals, ndens, type="l")

# Compute the 99% VaR and 99% ES of a N(mu, sigma^2) distribution
VaR99 <- qnorm(0.99, mean = mu, sd = sigma)
ES99 <- ESnorm(0.99, mu = mu, sd = sigma)

# Draw vertical lines at VaR99 and ES99 in red and green
abline(v = VaR99, col = "red")
abline(v = ES99, col = "green")
