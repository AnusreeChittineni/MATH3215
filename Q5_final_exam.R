# Parameters
n <- 120  
simulations <- 10^3  

# Function to create a symmetric random matrix
generate_symmetric_matrix <- function(n) {
  M <- matrix(rnorm(n * n), n, n)
  
  # ensure symmetry
  M[lower.tri(M)] <- t(M)[lower.tri(M)]  
  return(M)
}

# Collect all eigenvalues
all_eigenvalues <- c()

# Monte Carlo
for (i in 1:simulations) {
  M <- generate_symmetric_matrix(n)
  eigenvalues <- eigen(M)$values
  all_eigenvalues <- c(all_eigenvalues, eigenvalues)
}

# Plot approximated PDF
density_eigenvalues <- density(all_eigenvalues)

# Plot the density
plot(density_eigenvalues, main = "Approximated PDF of Eigenvalues",
     xlab = "Eigenvalue", ylab = "Density", col = "blue", lwd = 2)

# smooth density curve line
lines(density(all_eigenvalues), col = "red", lwd = 2)

# Compute mean and variance
mean_eigenvalues <- mean(all_eigenvalues)
var_eigenvalues <- var(all_eigenvalues)

# Print results
cat("Mean of eigenvalues:", mean_eigenvalues, "\n")
cat("Variance of eigenvalues:", var_eigenvalues, "\n")

# Confirm the variance with different matrices
test_variance <- function(dimensions) {
  variances <- c()
  for (n in dimensions) {
    eigenvalues <- c()
    for (i in 1:simulations) {
      M <- generate_symmetric_matrix(n)
      eigenvalues <- c(eigenvalues, eigen(M)$values)
    }
    variances <- c(variances, var(eigenvalues))
  }
  return(variances)
}

# Test for different matrix dims
dimensions <- c(50, 100, 120, 150,200)
variances <- test_variance(dimensions)

# Compare with theoretical variance
theoretical_variances <- 2 / dimensions
comparison <- data.frame(Dimension = dimensions, 
                         Simulated_Variance = variances, 
                         Theoretical_Variance = theoretical_variances)
print(comparison)
