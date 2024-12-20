# Define parameters
n <- 10^4
tries <- 10^4
epsilon <- 0.2
delta <- 0.2

# Function to compute the logarithmic integral
log_integral <- function(n) {
  integrate(function(x) 1 / log(x), lower = 2, upper = n)$value
}

# Compute Li(n)
Li_n <- log_integral(n)

# Function to simulate X_k
simulate_Xk <- function(k) {
  if (k < 3) return(k - 1)  # X1 = 0, X2 = 1
  rbinom(1, 1, 1 / log(k))
}

# Perform the simulation
deviations <- replicate(tries, {
  X <- sapply(1:n, simulate_Xk)
  pi_n <- sum(X)
  deviation <- (pi_n - Li_n) / (n^(1/2 + delta))
  deviation
})

# Compute the probability of exceeding epsilon
probability <- mean(deviations > epsilon)

# Output results
cat("Probability that the deviation exceeds epsilon:", probability, "\n")
