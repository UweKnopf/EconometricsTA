install.packages("wooldridge")
install.packages("tidyverse")
library(dplyr)
library(ggplot2)



CEOSAL <- wooldridge::ceosal2

summary(CEOSAL)

n_distinct(CEOSAL %>% filter(ceoten == 0))

max(CEOSAL$ceoten)

ggplot(CEOSAL, aes(x = salary, y = sales)) + geom_point()

#w: OLS
lm1 <- lm(salary ~ ceoten, CEOSAL)

mean_sal <- mean(CEOSAL$salary)
mean_ten <- mean(CEOSAL$ceoten)

b1 <- cov(CEOSAL$ceoten, CEOSAL$salary)/var(CEOSAL$ceoten)

b0 <- mean(CEOSAL$salary) - b1 * mean(CEOSAL$ceoten)

sum(lm1$residuals)

sum(lm1$residuals*CEOSAL$ceoten)

ggplot(CEOSAL, aes(x = salary, y = sales)) + geom_point() + 
  geom_smooth(method='lm') + annotate("point", x = mean_ten, y = mean_sal, colour = "red")

sal <- CEOSAL$salary

#1.3:Goodness of fit
TSS <- sum((sal - mean_sal)^2)
ESS <- sum((fitted(lm1) - mean_sal)^2)
RSS <- deviance(lm1)

TSS_calc <- ESS + RSS

r2 <- 1 - RSS/TSS
summary(lm1)

#4
library(stargazer)
library(gridExtra)

#CEOSAL <- CEOSAL %>% mutate(lceoten = ifelse(ceoten ==  log(CEOSAL$ceoten))

lm2_l_l <- lm(salary ~ sales, CEOSAL)
lm2_l_log <- lm(salary ~ lsales, CEOSAL)
lm2_log_l <- lm(lsalary ~ sales, CEOSAL)
lm2_log_log <- lm(lsalary ~ lsales, CEOSAL)

stargazer(lm2_l_l, lm2_l_log, lm2_log_l, lm2_log_log, type = "text")

g1 <- ggplot(CEOSAL, aes(x = salary, y = sales)) + geom_point()
g2 <- ggplot(CEOSAL, aes(x = lsalary, y = lsales)) + geom_point() + geom_smooth(method='lm')
grid.arrange(g1, g2, nrow = 1)

#5
summary(lm1)
vcov(lm1)
                                        

