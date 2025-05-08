library(erf)
library(ggplot2)
library(dplyr)

# Function to model scale
scale_step <- function(X) {
  ## numeric_vecotr -> numeric_vector
  ## produce scale function: scale(X) = step function
  
  sigma_x <- 1 + 1 * (X > 0)
  
  return(sigma_x)
}

# Generate data
set.seed(42)

n <- 2000
p <- 10
X <-x_ts
Y <- df$engagement_rate

# Fit ERF
fit_erf <- erf(X, Y, intermediate_quantile = 0.8)

# Predict ERF
quantiles <- c(0.9, 0.99)
pred_erf <- predict(fit_erf, newdata = X, quantiles = quantiles)

true_quantiles <- matrix(rep(qnorm(quantiles), n), 
                         ncol = length(quantiles),
                         byrow = TRUE) * scale_step(X[, 1])

# Plot results
my_palette <- list(
  "red" = "#D55E00",
  "blue" = "#0072B2"
)

ggplot() +
  geom_point(aes(x = X[, 1], y = Y), alpha = .5, col = "grey") +
  geom_point(aes(x = X[, 1], y = pred_erf[, 2]), alpha = .5, 
             col = my_palette$blue) +
  geom_line(aes(x = X[, 1], y = true_quantiles[, 2]), col = my_palette$red, 
            linetype = "dashed", size = 1) +
  theme_bw()
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.