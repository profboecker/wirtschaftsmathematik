# clear data and load libraries
rm(list = ls())
library(tidyverse)
library(nlstools)

# Simulate polynomial data
set.seed(42)
error <- 2

## define quadratic function (renamed to avoid conflict with base R's poly())
quadratic <- function(x, a, b, c) {
  a * x^2 + b * x + c
}

## generate x values and calculate y values from the polynomial
x <- seq(from = -10, to = 10, by = 0.1)
y <- quadratic(x, 1, 2, 1) + rnorm(n = length(x), mean = 0, sd = error)

## create tibble directly
poly_data <- tibble(x, y)

# plot data for a first view
ggplot(poly_data, aes(x = x, y = y)) +
  geom_point()

# Start with linear model (which is obviously wrong)
linear_model <- lm(y ~ x, data = poly_data)

## show results from linear model
summary(linear_model)
par(mfrow = c(2, 2))
plot(linear_model)
par(mfrow = c(1, 1))

## reusable prediction data frame
pred_x <- data.frame(x = poly_data$x)

## Generate a prediction from the linear model and plot
pred_data <- mutate(pred_x, y = predict(linear_model, pred_x))

ggplot(pred_data, aes(x = x, y = y)) +
  geom_line() +
  geom_point(data = poly_data) +
  xlab("x") + ylab("y") +
  theme_bw(base_size = 16)

# use nonlinear model now
model <- nls(y ~ quadratic(x, a, b, c),
             data = poly_data,
             start = list(a = 0, b = 0, c = 0))

# Analyse residuals
model_diag <- nlsResiduals(model)
plot(model_diag)

summary(model)

# Generate a prediction from the nonlinear model and plot
pred_data <- mutate(pred_x, y = predict(model, pred_x))

ggplot(pred_data, aes(x = x, y = y)) +
  geom_line(color = "red") +
  geom_point(data = poly_data) +
  xlab("x") + ylab("y") +
  theme_bw(base_size = 16)
