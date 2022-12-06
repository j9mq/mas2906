#Question 4


# Imported values
alpha = a
lambda = b
sigma = 1.8


pdf = function(n){
  
  # PDF of the Gamma distribution for Y = X / σ
  
  x = rgamma(n, shape = alpha, rate = lambda)
  Y = x/sigma
  
  p = (lambda**alpha * Y**(alpha - 1) * exp(-lambda * Y) / factorial(alpha))
  
  return(p)
}

# Randomly generated gamma values
x = rgamma(n = 10000, shape = alpha, rate = lambda)
Y = x/sigma

# E[Y] 
e_y = mean(Y)

# P(Y < E)
prob = length(Y[Y < e_y]) / length(Y)