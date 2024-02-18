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

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))
# check the data
head(climateSupport)
summary(climateSupport)
str(climateSupport)
# Fit an additive model
library(mgcv)
#  Forced conversion from character vector to logical vector
as.logical(ifelse(climateSupport$choice == "Supported", 1, 0))
# Convert counties and sanctions to unordered factors
climateSupport$countries <- factor(climateSupport$countries, ordered = FALSE)
climateSupport$sanctions <- factor(climateSupport$sanctions, ordered = FALSE)
# check the data
str(climateSupport)
# Fit a logistic regression model
model <- glm(choice ~ ., 
             data = climateSupport,
             family = "binomial")
# Display summary output
summary(model)
#Testing the Global null Hypothesis and its p-value
#Testing the Global null Hypothesis
global_null_hypothesis <- summary(model)$null.deviance
cat("Global null hypothesis:", global_null_hypothesis, "\n")
# Obtain p-value
model_p_value <- summary(model)$coefficients[, "Pr(>|z|)"]
cat("Model p-value:", model_p_value, "\n")
# Obtain the number of independent variables in the model
df.null <- length(coef(model)) - 1
# Calculate the p-value of the global null hypothesis
global_null_p_value <- pchisq(summary(model)$null.deviance, df = df.null, lower.tail = FALSE)
cat("Global null hypothesis p-value:", global_null_p_value, "\n")
#2a:
# Extract coefficients from the model
coefficients <- coef(model)
# Extracting coefficients related to interaction terms
interact_coefficients <- coefficients[grep("countries160 of 192:sanctions", names(coefficients))]
# Calculate the probability logarithm at 5% and 15% sanction levels
log_odds_5percent <- coefficients["(Intercept)"] + coefficients["countries160 of 192"] + coefficients["sanctions5%"] 
log_odds_5percent
log_odds_15percent <- coefficients["(Intercept)"] + coefficients["countries160 of 192"] + coefficients["sanctions15%"] 
log_odds_15percent
# Calculate the change in probability
odds_change <- exp(log_odds_15percent - log_odds_5percent)
# Print results
cat("As sanctions increase from 5% to 15%, the probability of individuals supporting policies changes as follows:", odds_change, "\n")
#2b:Calculate the estimated probability
log_odds_80_no_sanctions <- coef(model)['(Intercept)'] + coef(model)['countries80 of 192']
prob_80_no_sanctions <- exp(log_odds_80_no_sanctions) / (1 + exp(log_odds_80_no_sanctions))
cat("Estimated probability of 80 countries without sanctions:", prob_80_no_sanctions, "\n")
# 2c:Test interaction
model_interaction <- glm(choice ~ countries*sanctions, 
                         data = climateSupport, 
                         family = "binomial")
summary(model_interaction)
# Use anova() to compare models or view AIC/BIC
anova(model, model_interaction, test="Chisq")

