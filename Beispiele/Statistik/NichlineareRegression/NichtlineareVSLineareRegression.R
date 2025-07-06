# clear data and load library
rm(list = ls())
library(tidyverse)

# Simulate polynomial data
error <- 2

## define polynomial
poly <- function(x, a, b, c) {
  a * x^2 + b * x + c
}

## generate x values and calculate y values from the polynomial
x <- seq(from = -10, to = 10, by = 0.1)
y <- poly(x, 1, 2, 1) + rnorm(mean = 0, sd = error, length(x))

## convert x and y into a tibble via dataframe
poly_data <- as_tibble(data.frame(x, y))

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

## Generate a prediction from the linear model
pred_x <- data.frame(x = poly_data$x)
pred_y <- predict(linear_model, pred_x)
pred_data <- mutate(pred_x, y = pred_y)

## plot data and prediction from linear model
ggplot(pred_data, aes(x = x, y = y)) +
  geom_line() +
  geom_point(data = poly_data) +
  xlab("x") + ylab("y") +
  theme_bw(base_size = 16)

# use nonlinear model now
model <- nls(y ~ poly(x, a, b, c),
             data = poly_data,
             start = list(a = 0, b = 0, c = 0))

print(model)

# Analyse residuals
library(nlstools)
model_diag <- nlsResiduals(model)
plot(model_diag)

summary(model)

# Generate a prediction from the model
pred_x <- data.frame(x = poly_data$x)
pred_y <- predict(model, pred_x)
pred_data <- mutate(pred_x, y = pred_y)

ggplot(pred_data, aes(x = x, y = y)) +
  geom_line(color = "red") +
  geom_point(data = poly_data) +
  xlab("x") + ylab("y") +
  theme_bw(base_size = 16)
