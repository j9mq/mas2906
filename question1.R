#Question 1


set.seed(210033852)
a = sample(314, 1); b = sample(314, 1)

x = seq(0, 1, 0.001)

a = 10
b = 7
f = abx(a-1)  (1-xa)(b-1)

plot(x,
     f,
     xlab = 'x',
     ylab = 'f(x)',
     type = 'l',
     #main = 'Plot of f(x) = 70x^9(1-x^10)^6 against x',
)

abline(v = 1, lty = 2, col = 'red',)
abline(v = 0, lty = 2, col = 'red',)
abline(h = 4.839, lty = 2, col = 'red',)

# Generate randomly generated values for the grid
xseq = seq(0, 1, 0.0001)
yseq = seq(0, 4.839, 0.0001)

# Throw points at the grid
lines(sample(xseq, 1),
      sample(yseq, 1),
      type = 'p', pch = 'X',)

# Monte Carlo Integration estimate function
MCintegrate = function(N){
  no_of_hits = 0
  for(i in 1N){
    
    # Generate the xy co-ordinates efficiently
    x = runif(1, min = 0, max = 1)
    y = runif(1, min = 0, max = 4.839)
    
    # Evaluate if above or below curve
    a = 10
    b = 7
    f = abx(a-1)  (1-xa)(b-1)
    
    if(y < f){
      no_of_hits = no_of_hits + 1
    }
  }
  P = no_of_hits / N
  area_under_curve = P  4.839  1
  
  return(area_under_curve)
}

MCintegrate(1000000)
