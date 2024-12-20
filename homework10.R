m1=c(-4.69,0.78,1.08,1.74,0.9,-3.2,-1.2,2.75,1.75,-4.77,4.64,-0.48,-0.84,-0.2,1.09,0.73,-0.39,-0.98,-3.44,-0.19,-1.24,0.7,-2.28,0.39,1.13,-0.31,0.03,-2.84,-1.2,-1.67,0.82)

m2=c(0.75,2.18,1.02,0.11,4.55,-0.47,0.48,-1.01,-0.83,1.47,0.25,5.68,1.92,2.54,-0.05,5.17,2.11,3.54,0.61,2.41,0.75,2.96,1.13,5.67,-0.39,2.96,0.71,-0.37,3.15,-1.07,0.96)

m3=m1-m2

# problem 1

t_test_m1 <- t.test(m1, mu = 0, alternative = "two.sided")
t_test_m1

mean_m1 <- mean(m1)
sd_m1 <- sd(m1)
n1 <- length(m1)
mean_m1
sd_m1
n1

# Test statistic
t_stat <- mean_m1 / (sd_m1 / sqrt(n1))
t_stat

# Critical values for alpha = 0.05
alpha <- 0.05
t_critical <- qt(1 - alpha / 2, df = n1 - 1)
t_critical

# Two-tailed p-value
p_value <- 2 * pt(-abs(t_stat), df = n1 - 1)
p_value

list(t_stat = t_stat, t_critical = c(-t_critical, t_critical), p_value = p_value)

t_test_m2 <- t.test(m2, mu = 0, alternative = "two.sided")
t_test_m2

mean_m2 <- mean(m2)
sd_m2 <- sd(m2)
n2 <- length(m2)
mean_m2
sd_m2
n2

# Test statistic
t_stat2 <- mean_m2 / (sd_m2 / sqrt(n2))
t_stat2

# Critical values for alpha = 0.05
alpha2 <- 0.05
t_critical2 <- qt(1 - alpha / 2, df = n2 - 1)
t_critical2

# Two-tailed p-value
p_value2 <- 2 * pt(-abs(t_stat2), df = n2 - 1)
p_value2

list(t_stat = t_stat2, t_critical = c(-t_critical2, t_critical2), p_value = p_value2)



# problem 2

var1 <- var(m1)
var1
n1 <- length(m1)
chi_squared_stat <- (n1 - 1) * var1 / 1 # 1 is the hypothesized variance
p_value1 <- 2 * (1 - pchisq(abs(chi_squared_stat), df = n1 - 1))
list(chi_squared = chi_squared1, p_value = p_value1)

p_value_chi1 <- qchisq(0.975, df = 30)
p_value_chi1
p_value_chi2 <- qchisq(0.025, df = 30)
p_value_chi2

var1 <- var(m1)
var2 <- var(m2)

F_stat <- var1 / var2

df1 <- length(m1) - 1
df2 <- length(m2) - 1

p_value <- 2 * min(pf(F_stat, df1, df2, lower.tail = TRUE), pf(F_stat, df1, df2, lower.tail = FALSE))

list(F_statistic = F_stat, p_value = p_value)


# problem 3

var_m1 <- var(m1)
var_m2 <- var(m2)

pooled_variance <- ((n1 - 1) * var_m1 + (n2 - 1) * var_m2) / (n1 + n2 - 2)
pooled_sd <- sqrt(pooled_variance)
t_statistic <- (mean_m1 - mean_m2) / (pooled_sd * sqrt(1/n1 + 1/n2))
df <- n1 + n2 - 2
p_value <- 2 * (1 - pt(abs(t_statistic), df))
list(
  t_statistic = t_statistic,
  p_value = p_value,
  df = df
)

sigma1 <- 1
sigma2 <- 2
n1 <- 31
n2 <- 31

mean1 <- mean(m1)
mean2 <- mean(m2)

z <- (mean1 - mean2) /sqrt((sigma1^2/n1) + (sigma2^2 /n2))
p_value <- 2 * (1-pnorm(abs(z)))

z
p_value

# problem 4

result <- t.test(m1,m2, var.equal=FALSE)
result

s_x_squared <- var(m1)
s_y_squared <- var(m2)

n_x <- length(m1)
n_y <- length(m2)

sigma_0_squared <- 1
chi_squared_x <- (n_x -1) * s_x_squared / sigma_0_squared
chi_squared_y <- (n_y -1) * s_y_squared /sigma_0_squared

p_value_x <- 2* min(pchisq(chi_squared_x, df=n_x-1), 1-pchisq(chi_squared_x, df = n_x -1))
p_value_y <- 2 * min(pchisq(chi_squared_y, df = n_y -1), 1-pchisq(chi_squared_y, df= n_y -1))

list(chi_squared_X = chi_squared_x, p_value_X = p_value_x,
     chi_squared_Y = chi_squared_y, p_value_Y = p_value_y)


