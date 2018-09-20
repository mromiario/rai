 
f <- (function(x1, x2)
  -abs(sin(x1) * cos(x2) * exp(abs(1 - (sqrt((x1 ** 2) + (x2 ** 2)) / pi))))
)
 
# Initialize random solution
start <- list(x1 = -2, x2 = 5, temperature = 1000)
start <- c(start, list(acceptance = f(start$x1, start$x2)))
 
# Initialize current solution
current <- start
best <- c(current, acceptance = f(current$x1, current$x2))
 
# Generate random point (x1, x2)
generate <- (function() runif(1, -10, 10))
 
# Main simulated annealing algorithm
while (current$temperature > 0) {
  # Generate a new random solution
  newly <- list(x1 = generate(), x2 = generate())
  newly <- c(newly, acceptance = f(newly$x1, newly$x2))
  
  # Energy changes a.k.a \Delta E
  changes <- newly$acceptance - current$acceptance
  
  if (changes < 0) {
    current$x1 <- newly$x1
    current$x2 <- newly$x2
    
    current$acceptance <- newly$acceptance
    
    if (newly$acceptance < best$acceptance) {
      best$x1 <- newly$x1
      best$x2 <- newly$x2
      
      best$acceptance <- newly$acceptance
    }
  }
  else {
    # Considering move to the current (x1, x2)
    P <- exp((-changes) / current$temperature)
    R <- runif(1)
    
    if (R < P) {
      current$x1 <- newly$x1
      current$x2 <- newly$x2
    }
  }
  
  # Decrease the temperature randomly
  current$temperature <- current$temperature - runif(1, 0.0001, 0.01)
}

solution <- list(x1 = best$x1, x2 = best$x2, value = best$acceptance)

cat("Here is the solution:\n\n")
print(solution)

