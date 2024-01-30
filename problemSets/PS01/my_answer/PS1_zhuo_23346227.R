#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

ks_test_normal <- function(data) {
  # Create empirical distribution of observed data
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
  
  # Generate test statistic
  D <- max(abs(empiricalCDF - pnorm(data, mean = mean(data), sd = sd(data))))
  
  # Calculate the p-value using R's ks.test function
  p_value <- ks.test(data, "pnorm", mean = mean(data), sd = sd(data))$p.value
  
  return(list(D = D, p_value = p_value))
}

# To test this function:
set.seed(123)
cauchy_data <- rcauchy(1000, location = 0, scale = 1)
test_result <- ks_test_normal(cauchy_data)
test_result

#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# Define the loss function for OLS
loss_function <- function(params, data) {
  predictions <- params[1] + params[2] * data$x
  residuals <- data$y - predictions
  sum(residuals^2)
}

# Initial guesses for parameters
initial_params <- c(0, 0)  # Start with intercept = 0, slope = 0

# Minimize the loss function using BFGS
results_bfgs <- optim(initial_params, loss_function, data = data, method = "BFGS")

# OLS regression using lm for comparison
results_lm <- lm(y ~ x, data = data)

# Show results
list(
  BFGS = results_bfgs$par,
  LM = coef(results_lm)
)
