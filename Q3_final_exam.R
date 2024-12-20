# Set constants
set.seed(42)
n <- 20  
alpha1 <- 2
alpha2 <- 1
sigma2_eps <- 4
x <- 1:n  
num_simulations <- 10^5  

# Storage for results
alpha_sum_squares <- numeric(num_simulations) # alpha1_hat^2 + alpha2_hat^2
correlations <- numeric(num_simulations) # correlation coefficients

# Monte Carlo Loop
for (sim in 1:num_simulations) {
  
  # generate noise
  epsilon <- rnorm(n, mean = 0, sd = sqrt(sigma2_eps))
  
  # response var y
  y <- alpha1 + alpha2 * x + epsilon
  
  # fir regression model
  model <- lm(y ~ x)
  
  # estimated coefs
  alpha1_hat <- coef(model)[1]
  alpha2_hat <- coef(model)[2]
  
  # sum of squares
  alpha_sum_squares[sim] <- alpha1_hat^2 + alpha2_hat^2
  
  correlations[sim] <- cor(x, y)
}


# 1. PDF of alpha1^2 + alpha2^2
density_alpha <- density(alpha_sum_squares)

# Plot the density
plot(density_alpha, main = "PDF of alpha1^2 + alpha2^2",
     xlab = expression(hat(alpha)[1]^2 + hat(alpha)[2]^2),
     ylab = "Density", col = "blue", lwd = 2)
mean_alpha <- mean(alpha_sum_squares)
var_alpha <- var(alpha_sum_squares)

cat("Mean of alpha1^2 + alpha2^2:", mean_alpha, "\n")
cat("Variance of alpha1^2 + alpha2^2:", var_alpha, "\n")

# 2. PDF of correlation coefficient
density_r <- density(correlations)

# Plot the density
plot(density_r, main = "PDF of Correlation Coefficient",
     xlab = "r", ylab = "Density", col = "blue", lwd = 2)
mean_r <- mean(correlations)
var_r <- var(correlations)
cat("Mean of correlation coefficient:", mean_r, "\n")
cat("Variance of correlation coefficient:", var_r, "\n")

# Probability P(0.9 < r < 0.95)
prob_r <- mean(correlations > 0.9 & correlations < 0.95)
cat("P(0.9 < r < 0.95):", prob_r, "\n")
