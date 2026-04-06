
library(nloptr)

set.seed(100)
N <- 100000
K <- 10
sigma <- 0.5

# Create X matrix (first column = ones, rest normally distributed)
X <- matrix(rnorm(N * K), nrow = N, ncol = K)
X[, 1] <- 1  # First column is all ones

# Create beta vector with specified values
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

# Create epsilon vector
eps <- rnorm(N, mean = 0, sd = sigma)

# Generate Y
Y <- X %*% beta + eps

beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y

# These estimates are really close to the true value of beta in (1)

# Gradient Descent
gradient <- function(beta) return(-2 * t(X) %*% (Y - X %*% beta))

alpha <- 0.0000003
iter  <- 500

set.seed(100)
beta     <- matrix(rnorm(K), ncol = 1)
beta.All <- matrix(NA, nrow = iter, ncol = K)

for(i in 1:iter){
  beta      <- beta - alpha * gradient(beta)
  beta.All[i,] <- t(beta)
}

print(beta)

# L-BFGS algorithm

objective <- function(beta, Y, X) return(sum((Y - X %*% beta)^2))

gradient  <- function(beta, Y, X) return(as.vector(-2 * t(X) %*% (Y - X %*% beta)))

result_lbfgs <- nloptr(
  x0          = rep(0, K),
  eval_f      = objective,
  eval_grad_f = gradient,   # reuse the one you already made
  opts        = list(algorithm = "NLOPT_LD_LBFGS", xtol_rel = 1e-8),
  Y = Y, X = X
)
print(result_lbfgs$solution)

# Nelder-Mead algorithm

result_nm <- nloptr(
  x0     = rep(0, K),
  eval_f = objective,
  opts   = list(algorithm = "NLOPT_LN_NELDERMEAD", xtol_rel = 1e-8, maxeval = 100000),
  Y = Y, X = X
)
print(result_nm$solution)


# The estimates are not too different, I had to set the maxeval = 100000 for Nedler though. 

# 8
objective_mle <- function(theta, Y, X) {
  beta <- theta[1:(length(theta) - 1)]
  sig  <- theta[length(theta)]
  N    <- nrow(X)
  return((N/2)*log(2*pi) + N*log(sig) + sum((Y - X %*% beta)^2) / (2*sig^2))
}

gradient_mle <- function(theta, Y, X) {
  grad <- as.vector(rep(0, length(theta)))
  beta <- theta[1:(length(theta) - 1)]
  sig  <- theta[length(theta)]
  grad[1:(length(theta) - 1)] <- -t(X) %*% (Y - X %*% beta) / (sig^2)
  grad[length(theta)]         <- dim(X)[1] / sig - crossprod(Y - X %*% beta) / (sig^3)
  return(grad)
}

result_mle <- nloptr(
  x0          = c(rep(0, K), 1),
  eval_f      = objective_mle,
  eval_grad_f = gradient_mle,
  lb          = c(rep(-Inf, K), 1e-4),  # sigma must stay positive
  opts        = list(algorithm = "NLOPT_LD_LBFGS", xtol_rel = 1e-8, maxeval = 10000),
  Y = Y, X = X
)
print(result_mle$solution)

# 9

model = lm(Y ~ X - 1)
modelsummary(model)

modelsummary(model, output = "PS8_LastName.tex")

