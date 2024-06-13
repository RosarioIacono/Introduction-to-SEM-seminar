# Example of covariance based Structural Equation Modeling testing
# Source: https://cran.r-project.org/web/packages/lavaanPlot/vignettes/Intro_to_lavaanPlot.html
library(lavaan)
library(lavaanPlot)

model <- 'mpg ~ cyl + disp + hp
          qsec ~ disp + hp + wt'

fit <- sem(model, data = mtcars)
summary(fit)