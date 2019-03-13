# Clear the workspace
rm(list=ls())
cat("\014") 
# Set random seed for reproducibility
set.seed(100)

# Libraries
library(splines)

# Covariate
x = seq(0, 4*pi, by=0.05)

# Response variable
eps = sqrt(0.2)
y = sin(x) + eps * rnorm(length(x))

# Scatter plot
plot(x, y, main="Scatter Plot", 
     xlab="X ", ylab="Y", pch=19)

# Fit a cubic spline
vector_knots = c(2,4,9,12)
df = data.frame(x,y)
# Model
# NOTE: use ns() for natural splines so that we dont overfit at the boundries of the X
# para for the bs(degree = 3)
fit=lm(y ~ ns(x , knots = vector_knots))

xlims = range(x)
x_grid = seq(from=xlims[1], to=xlims[2])

# Piece wise prediction using the fitted splines
pred = predict(fit, newdata = list(x), se=TRUE)
# Plot the results
plot(x ,y ,col="gray")
# lines
lines(x , pred$fit , lwd=2)
# Get the error of the model
'The Error of the model is: '
pred$residual.scale