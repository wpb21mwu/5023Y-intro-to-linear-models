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

broom::tidy(lsmodel1) # summerise key info about the information, top line is the first variables mean the typeself line is the difference between the two means 

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height))

summary(lsmodel1) # tells us degrees of freedom and the proportion of variance explained by the model. 

darwin %>% 
  ggplot(aes(x=type, 
             y=height,
             colour=type))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw()

