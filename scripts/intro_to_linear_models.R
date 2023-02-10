## Intro to Linear Models----
# Lesson 2

library(tidyverse)
library(GGally)
library(emmeans)
library(performance)
library(broom.helpers)

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

# Confidence Intervals----

confint(lsmodel1) # Base R for calculating confidence intervals
broom::tidy(lsmodel1, conf.int=T) # Tidyverse for calculating confidence intervals

#Answering Questions----

GGally::ggcoef_model(lsmodel1,
                     show_p_values=FALSE, 
                     conf.level=0.95) # produces a graph of the estimated mean difference with an approx 95% CI. 
 #As we can see we are able to reject the null hypothesis at a 95% confidence level.

broom::tidy(lsmodel1, conf.int=T, conf.level=0.99)

means <- emmeans::emmeans(lsmodel1, specs = ~ type)

means

means %>% 
  as_tibble() %>% 
  ggplot(aes(x=type, 
             y=emmean))+
  geom_pointrange(aes(
    ymin=lower.CL, 
    ymax=upper.CL))

# Assumption Checking----

performance::check_model(lsmodel1) # Base R 
plot(lsmodel1) # tidyverse 

# Normal Distribution----
performance::check_model(lsmodel1, check=c("normality","qq"))
plot(lsmodel1, which=c(2,2))

#Equal Variance----
performance::check_model(lsmodel1, check="homogeneity")
plot(lsmodel1, which=c(1,3))

#Outliers----
performance::check_model(lsmodel1, check="outliers")
plot(lsmodel1, which=c(4,4))

# Summary----
darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_segment(aes(x=1, xend=2, y=20.192, yend=20.192-2.617), linetype="dashed")+
  stat_summary(fun.y=mean, geom="crossbar", width=0.2)