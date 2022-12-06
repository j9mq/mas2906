#Question 3


lognorm = function(N, mu, sigma){
  
  # Generate normal random variables of N realisations
  x = rnorm(N, mean = mu, sd = sigma)
  y = exp(x)
  
  return(y)
}

"Array of mu values"
mu = c(0:5)

"Empty array which we will use in for loop"
logmean = rep(0, length(mu))
logmedian = rep(0, length(mu))

# Mean
for(n in 1:length(mu)){
  
  sample = lognorm(1000, mu = mu[n], sigma = 2)
  logmean[n] = log(mean(sample))
  
}

plot(mu, 
     logmean,
     col = 'blue',
     type = 'b',
     xlab = 'μ',
     ylab = 'Log of the sample mean',
     main = 'Plot of the log of the sample mean versus μ',
)

# Median
for(n in 1:length(mu)){
  
  sample = lognorm(1000, mu = mu[n], sigma = 2)
  logmedian[n] = log(median(sample))

}

plot(mu, 
     logmedian,
     col = 'forestgreen',
     type = 'b',
     xlab = 'μ',
     ylab = 'Log of the sample median',
     main = 'Plot of the log of the sample median versus μ',
)

