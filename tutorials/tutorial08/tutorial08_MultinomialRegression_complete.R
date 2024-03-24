##############
# Tutorial 5 Ordered and Multinomial Logistic Regression #
##############

library(MASS)
library(nnet)
library(ggplot2)

# EDA 数据探索分析
summary(workingMoms)
ftable(xtabs(~ gender + year + attitude, data = workingMoms))

# do some wrangling 进行数据整理
workingMoms$attitude <- factor(workingMoms$attitude, 
                               levels = c("SD", "D", "A", "SA"),
                               labels = c("Strongly Disagree",
                                          "Disagree",
                                          "Agree",
                                          "Strongly Agree"))
workingMoms$gender <- as.factor(workingMoms$gender)
workingMoms$race <- factor(workingMoms$race,
                           levels = c(0,1),
                           labels = c("Non-white", "White"))
workingMoms$year <- factor(workingMoms$year,
                           levels = c("Year1977", "Year1989"),
                           labels = c("1977", "1989"))

ftable(xtabs(~ gender + year + attitude, data = workingMoms))

ggplot(workingMoms, aes(attitude, prestige)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  scale_x_discrete(labels=function(x){sub("\\s", "\n", x)}) +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_grid(gender ~ year)

# a) Perform an ordered (proportional odds) logistic regression 进行有序（比例几率）逻辑回归

ord.log <- polr(attitude ~ ., data = workingMoms, Hess = TRUE)
summary(ord.log)

# Calculate a p value 计算 p 值
ctable <- coef(summary(ord.log))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

# Calculate confidence intervals 计算置信区间
(ci <- confint(ord.log))

# convert to odds ratio 转换为几率比
exp(cbind(OR = coef(ord.log), ci))

# How do we interpret these coefficients?

# b) fit a multinomial logit model
# set a reference level for the outcome
#如何解释这些系数？
#b) 拟合多项式逻辑回归模型
#为目标变量设定参考水平
workingMoms$attitude <- relevel(workingMoms$attitude, ref = "Strongly Disagree")

# run model
mult.log <- multinom(attitude ~ ., data = workingMoms)
summary(mult.log)
exp(coef(mult.log))

# get p values
z <- summary(mult.log)$coefficients/summary(mult.log)$standard.errors
(p <- (1 - pnorm(abs(z), 0, 1)) * 2)

# how do we interpret these coefficients?

# we can use predicted probabilities to help our interpretation 
#如何解释这些系数？
#我们可以使用预测概率来辅助解释
pp <- data.frame(fitted(mult.log))
head(data.frame(attitude = workingMoms$attitude,
                SD = pp$Strongly.Disagree,
                D = pp$Disagree,
                A = pp$Agree,
                SA = pp$Strongly.Agree))

# c) Consider gender as an interaction 考虑性别交互作用
mult.log.int <- multinom(attitude ~ gender * ., data = workingMoms)
summary(mult.log.int)

z.int <- summary(mult.log.int)$coefficients/summary(mult.log.int)$standard.errors
(p.int <- (1 - pnorm(abs(z.int), 0, 1)) * 2)

pp.int <- data.frame(fitted(mult.log))
head(data.frame(attitude = workingMoms$attitude,
                SD = pp.int$Strongly.Disagree,
                D = pp.int$Disagree,
                A = pp.int$Agree,
                SA = pp.int$Strongly.Agree))

