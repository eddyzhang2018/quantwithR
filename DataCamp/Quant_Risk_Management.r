

### SPOTTING A VOLATILE TIME SERIES

# Compute the length n of djx 
n <- length(djx)

#  Generate a normal sample of size n with parameters given by npars
ndata <- rnorm(n)*npars[2] + npars[1]

# Generate a t-distributed sample of size n with paramaters given by tpars
tdata <- rt(n, df = tpars[1])*tpars[3] + tpars[2]

# Make ndata and tdata into xts objects
ndatax <- xts(ndata, time(djx))
tdatax <- xts(tdata, time(djx))

# Merge djx, ndatax, and tdatax and plot
alldata <- merge(djx,ndatax, tdatax)
plot.zoo(alldata, type = "h", ylim = range(alldata))


### COMPUTING VaR and ES for NORMAL DISTRIBUTION

# Make a sequence of 100 x-values going from -4*sigma to 4*sigma
xvals <- seq(from = -4*sigma, to = 4*sigma, length.out = 100)

# Compute the density of a N(mu, sigma^2) distribution at xvals
ndens <- dnorm(xvals, mean = mu, sd = sigma)

# Plot ndens against xvals
plot(xvals, ndens, type = "l")

# Compute the 99% VaR and 99% ES of a N(mu, sigma^2) distribution
VaR99 <- qnorm(0.99, mean = mu, sd = sigma)
ES99 <- ESnorm(0.99, mu = mu, sd = sigma)

# Draw vertical lines at VaR99 and ES99 in red and green
abline(v = VaR99, col = "red")
abline(v = ES99, col = "green")


### Examining risk factors for international equity portfolio

# Plot the risk-factor data
plot.zoo(riskfactors)

# Calculate the log-returns, assign to returns, and plot
returns <- diff(log(riskfactors))[-1, ]
plot.zoo(returns)

# Use apply() to carry out the Jarque-Bera test for all 5 series
apply(returns, 2, jarque.test)

# Make a Q-Q plot against normal for the 5th return series and add a reference line
qqnorm(returns[, 5])
qqline(returns[, 5])

# Make a picture of the sample acfs for returns and their absolute values
acf(returns)
acf(abs(returns))
