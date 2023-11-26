
data(mtcars)

df <- with(mtcars, data.frame(y = mpg, x1 = disp, x2 = hp, x3 = wt))

nll_lm <- function(data, par) {
  beta <- par[1:(length(par) - 1)] 
  sigma <- par[length(par)]         
  
  if (sigma <= 0) return(Inf)  
  
  y_pred <- with(data, beta[1] + beta[2]*x1 + beta[3]*x2 + beta[4]*x3)
  residuals <- data$y - y_pred
  
  -sum(dnorm(residuals, mean = 0, sd = sigma, log = TRUE))
}
  
initial_params <- c(mean(df$y), rep(0, ncol(df) - 1), 1)

optim_result <- optim(par = initial_params, 
                      fn = nll_lm, data = df, 
                      method = "L-BFGS-B",
                      lower = c(-Inf, rep(-Inf, ncol(df) - 1), 1e-6),
                      upper = c(Inf, rep(Inf, ncol(df) - 1), Inf),
                      hessian = TRUE)

beta_hat <- optim_result$par[1:(length(optim_result$par) - 1)]
sigma_hat <- optim_result$par[length(optim_result$par)]


X <- cbind(1, df$x1, df$x2, df$x3)
y <- df$y
beta_matrix <- solve(t(X) %*% X) %*% t(X) %*% y

beta_hat

beta_matrix


residuals_matrix <- y - X %*% beta_matrix
sigma_matrix <- sqrt(sum(residuals_matrix^2) / (nrow(df) - length(beta_matrix)))

sigma_hat

sigma_matrix


hessian_matrix <- optim_result$hessian
var_covar_matrix <- solve(hessian_matrix[-length(optim_result$par), -length(optim_result$par)])
std_errors <- sqrt(diag(var_covar_matrix))

std_errors

model <- lm(mpg ~ disp + hp + wt, data = mtcars)


beta_hat_lm <- coef(model)


print(beta_hat_lm)


residuals <- residuals(model)
sigma_hat_lm <- sqrt(sum(residuals^2) / df.residual(model))


print(sigma_hat_lm)

