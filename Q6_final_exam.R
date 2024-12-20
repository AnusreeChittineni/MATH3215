# Constants
set.seed(123)  
lambda <- 1 / 3   
n_crashes <- 1000 
time_limit <- 10  
simulations <- 10^5 

simulate_crashes <- function() {
  crash_times <- cumsum(rexp(n_crashes, rate=lambda))
  sum(crash_times < time_limit)
}

crash_counts <- replicate(n_crashes, simulate_crashes())
expected_crashes <- mean(crash_counts)
cat("Expected number of crashes in 10 months (simulation):", expected_crashes, "\n")

# Used for the bonus part to calculate the integrals

# Define parameters
lambda <- (1 / 3)
time_limit <- 10

# Define gamma PDF and CDF
gamma_pdf <- function(x, shape, rate) {
  dgamma(x, shape=shape, rate=rate)
}
gamma_cdf <- function(x, shape, rate) {
  pgamma(x, shape=shape, rate=rate)
}

# Function to calculate P(N10 = k)
p_n10 <- function(k, lambda, time_limit) {
  integrate(function(x) {
    gamma_pdf(x, shape=k, rate=lambda) * 
      (1 - gamma_cdf(time_limit - x, shape=k+1, rate=lambda)) * (time_limit - x > 0)
  }, lower=0, upper=time_limit)$value
}

# Compute E(N10)
k_values <- 1:100  # Truncate the range to ensure convergence
p_values <- sapply(k_values, function(k) p_n10(k, lambda, time_limit))
expected_n10 <- sum(k_values * p_values)

cat("Expected number of crashes in 10 months (theoretical):", expected_n10, "\n")
