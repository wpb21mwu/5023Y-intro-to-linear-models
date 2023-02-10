## Intro to Linear Models----
# Lesson 2

library(tidyverse)
library(GGally)
library(emmeans)
library(performance)

# Lm function---- 
lsmodel0 <- lm(formula = height ~ 1, data = darwin)

summary(lsmodel0) 
broom::tidy(lsmodel0) # Both these lines of code do the same thing one uses tidyverse the other uses base R

mean(darwin$height) # Selects just the mean

# Comparing Means----

lsmodel1 <- lm(height ~ type, data=darwin)

# note that the following is identical

# lsmodel1 <- lm(height ~ 1 + type, data=darwin)

broom::tidy(lsmodel1) # summerise key info about the information


