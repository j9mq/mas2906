#Question 2


# Student ID = 210033852
d = 2
sigma = 1 + (d/2.5)



inv.cdf = function(n){
  
  # Generate n random variables, uniformally distributed between -1 and 1
  x = runif(n)
  
  # Apply CDF formula to random values x
  f = sqrt(-2 * sigma**2 * log(1-x))
  
  return(f)
}
 

hist(inv.cdf(n = 10000),
     col = 'forestgreen',
     xlab = 'X',
     ylab = 'Inverse CDF density',
     main = 'Histogram of the CDF of function f(x)',
     prob = TRUE,
)

curve(x/(sigma**2) * (exp(-(x**2) / (2*sigma**2))),
      col = 'red',
      add = TRUE,
)

# Randomly generated sample
x = runif(10000)

oe = mean(inv.cdf(10000)) # Observed expectation
ov = var(inv.cdf(10000)) # Observed variance

expectation = sigma * sqrt(pi/2)
variance = (4-pi)/2 * sigma**2

