x=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
Y=c(0.36,4.55,2.92,7.34,7.54,7.81,11.43,10.23,14.06,11.33,12.97,15.36,10.27,20.58,15.88,19.52,21.24,19.57,18.47,22.22)
C=unname(lm(Y~x)$coefficients)
n=length(x)
beta_hat=C[2]
alpha_hat=C[1]
Sxx=var(x)*(n-1)
Sxy=cov(x,Y)*(n-1)
Syy=var(Y)*(n-1)
SSR=Syy-(Sxy^2)/Sxx
sqrt(Sxx)*(beta_hat)/(sqrt(SSR/18))
t=sqrt(n*Sxx)*(alpha_hat-1)/(sqrt(SSR/(n-2))*sqrt(sum(x^2)))
qt(97.5/100,18)*sqrt(SSR/(n-2))*sqrt(1+1/n+(21-mean(x))^2/Sxx)
a=rep(1,20)

#problem 4
A=matrix(c(a,x,log(x)),ncol =3)
round(4*t(solve(t(A) %*% A)),3)

# problem 3 part 1

lm_model = lm(Y ~ x)  # Fit the linear model
summary(lm_model)

C = unname(lm_model$coefficients)  # Extract coefficients
beta_hat = C[2]
alpha_hat = C[1]

n = length(x)
Sxx = var(x) * (n - 1)
Sxy = cov(x, Y) * (n - 1)
Syy = var(Y) * (n - 1)
SSR = Syy - (Sxy^2) / Sxx

sigma_squared = SSR / (n - 2)
SE_beta = sqrt(sigma_squared / Sxx)
t_beta = beta_hat / SE_beta
p_value = 2 * pt(-abs(t_beta), df = n - 2)  # Two-sided p-value

t_critical = qt(0.975, df = n - 2)
CI_lower = beta_hat - t_critical * SE_beta
CI_upper = beta_hat + t_critical * SE_beta
confidence_interval = c(CI_lower, CI_upper)

list(beta_hat = beta_hat, t_beta = t_beta, p_value = p_value,
     confidence_interval = confidence_interval)

#problem 3 part 2

lm_model = lm(Y ~ x)

# Extract coefficients and standard errors
C = unname(lm_model$coefficients)
alpha_hat = C[1]
beta_hat = C[2]
se_alpha = summary(lm_model)$coefficients[1, 2]  # Standard error of alpha

# Test for H0: alpha = 1
t_alpha = (alpha_hat - 1) / se_alpha  # t statistic
p_value_alpha = 2 * pt(-abs(t_alpha), df = length(x) - 2)  # Two-sided p-value for alpha

# Calculate the 95% confidence interval for alpha
t_critical = qt(0.975, df = length(x) - 2)  # Critical t-value for 95% CI
CI_lower_alpha = alpha_hat - t_critical * se_alpha
CI_upper_alpha = alpha_hat + t_critical * se_alpha
confidence_interval_alpha = c(CI_lower_alpha, CI_upper_alpha)

# Output the result
list(t_alpha = t_alpha, p_value_alpha = p_value_alpha, confidence_interval_alpha = confidence_interval_alpha)

# problem 4 part 1
lm_model = lm(Y ~ x)

# Extract residuals and calculate RSS (Residual Sum of Squares)
residuals = lm_model$residuals
RSS = sum(residuals^2)

# Estimate of error variance (MSE)
n = length(x)
sigma_hat_squared = RSS / (n - 2)

# Test for H0: sigma^2 = 1
chisq_stat = (n - 2) * sigma_hat_squared  # Chi-squared statistic
p_value_sigma = 2 * (1 - pchisq(chisq_stat, df = n - 2))  # Two-sided p-value for sigma^2 = 1

# Confidence interval for sigma^2
alpha = 0.05
chi2_lower = qchisq(1 - alpha / 2, df = n - 2)
chi2_upper = qchisq(alpha / 2, df = n - 2)

CI_lower_sigma = (n - 2) * sigma_hat_squared / chi2_lower
CI_upper_sigma = (n - 2) * sigma_hat_squared / chi2_upper
confidence_interval_sigma = c(CI_lower_sigma, CI_upper_sigma)

# Output the results
list(
  chisq_stat = chisq_stat, 
  p_value_sigma = p_value_sigma, 
  confidence_interval_sigma = confidence_interval_sigma
)

# problem 4 part 2

lm_model = lm(Y ~ x)

# Get the estimated coefficients
alpha_hat = lm_model$coefficients[1]
beta_hat = lm_model$coefficients[2]

# Calculate residuals and RSS
residuals = lm_model$residuals
RSS = sum(residuals^2)

# Estimate of error variance (MSE)
n = length(x)
sigma_hat_squared = RSS / (n - 2)

# Calculate S_xx
S_xx = sum((x - mean(x))^2)

# New x value for prediction (x_21 = 21)
x_new = 21

# Predicted value Y_hat_21 for x_new = 21
Y_hat_21 = alpha_hat + beta_hat * x_new

# Find t-critical value for 95% prediction interval (two-sided, df = n-2)
t_critical = qt(0.975, df = n - 2)

# Calculate the prediction interval
prediction_interval = t_critical * sqrt(sigma_hat_squared * (1 + 1/n + (x_new - mean(x))^2 / S_xx))

# Lower and upper bounds of the 95% prediction interval for Y_21
lower_bound = Y_hat_21 - prediction_interval
upper_bound = Y_hat_21 + prediction_interval

# Output the results
list(
  Y_hat_21 = Y_hat_21, 
  lower_bound = lower_bound,
  upper_bound = upper_bound
)