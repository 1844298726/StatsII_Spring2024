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

lapply(c("nnet", "MASS"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)
# do some wrangling 
gdp_data$REG <- factor(gdp_data$REG, 
                               levels = c(0,1),
                               labels = c("Non-Democracy", "Democracy"))
gdp_data$OIL <- factor(gdp_data$OIL, 
                       levels = c(0,1),
                       labels = c("Below50%","Beyond50%"))


gdp_data$GDPWdiff <- factor(
  ifelse(gdp_data$GDPWdiff > 0, "positive",
         ifelse(gdp_data$GDPWdiff == 0, "no change", "negative")),
  levels = c( "negative","positive", "no change"),
  labels = c( "negative","positive", "no change")
)

# Set "no change" as the reference category
gdp_data$GDPWdiff <- relevel(gdp_data$GDPWdiff, ref = "no change")
# Set "Democracy" and "Beyond50%" as the reference categories
gdp_data$REG <- relevel(gdp_data$REG, ref = "Non-Democracy")
gdp_data$OIL <- relevel(gdp_data$OIL, ref = "Below50%")

# Constructing an unordered multiple logistic regression model
model1 <- multinom(GDPWdiff ~ REG + OIL, data = gdp_data)

# View Model Summary
summary(model1)
# Extract estimated values of intercept and coefficients
intercepts <- coef(model1)
# View estimated values of intercept and coefficients
print("Intercepts:")
print(intercepts)
#exponentiate coefficients
exp(coef(model1)[,c(1:3)])

# Encode GDPWdiff as an ordered factor variable GDPWdiff
gdp_data$GDPWdiff <- factor(gdp_data$GDPWdiff,
                            levels = c( "no change", "negative","positive"),
                            labels = c( "no change", "negative","positive"),
                            ordered = TRUE
)

# Constructing an ordered multiple logistic regression model
model2 <- polr(GDPWdiff ~ REG + OIL, data = gdp_data, Hess=T)

# View Model Summary
summary(model2)
# Extract estimated values of intercept and coefficients
intercepts <- coef(model2)
# View estimated values of intercept and coefficients
print("Intercepts:")
print(intercepts)

#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")
model3 <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data = mexico_elections, family = "poisson")
summary(model3)
hypothetical_district <- data.frame(competitive.district = 1, marginality.06 = 0, PAN.governor.06 = 1)
estimated_mean_visits <- predict(model3, newdata = hypothetical_district, type = "response")
estimated_mean_visits

