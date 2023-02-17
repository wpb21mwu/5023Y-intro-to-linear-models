# Lesson 3----

library(tidyverse)
library(GGally)
library(emmeans)
library(performance)
library(broom.helpers)

# Student T-test ----
#  t-distribution, a small-sample size version of the normal distribution, where tails are fatter if the degrees of freedome are small.

# The Base r way of finding the t-distribution at different degrees of freedom
x <- seq(-4, 4, length=100)
hx <- dnorm(x)

degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors) 

# Tidyverse way to find the comparisons of the t-distribution at different degrees of freedom
x <- seq(-4, 4, length=100)
z_dist <- dnorm(x)

values <- tibble(x,z_dist)

# map_dfc combines values returned into a dataframe
t <- map_dfc(degf, ~dt(x, .x))
colnames(t) <- degf

combined <- cbind(values,t)

combined %>% 
  pivot_longer(cols=!x, names_to="distribution") %>% 
  mutate(distribution=factor(distribution, levels=c("z_dist", "1", "3", "8", "30"))) %>%  
  mutate(distribution=fct_recode(distribution, "z distribution" = "z_dist", "df = 1" = "1", "df = 3" = "3", "df = 8" = "8", "df = 30" = "30")) %>% 
  ggplot(aes(x=x, y=value, colour=distribution))+
  geom_line(linetype="dashed")+
  theme_classic()

# Critical t value must be exceede for the test to be significant. Critical t value is decided by the degrees of freedom.

df <- c(1:30)

# map_dbl forces returned values to be a single vector of numbers (rather than a list)
critical_t <- map_dbl(df, ~qt(p=0.05/2, df=.x, lower.tail=FALSE))

tibble(df,critical_t) %>% 
  ggplot(aes(x=df, y=critical_t))+
  geom_point()+
  geom_line()+
  geom_hline(aes(yintercept=1.96), linetype="dashed", colour="red")+
  labs(x= "Degrees of Freedom",
       y= expression(paste("Critical value of ", italic("t"))))
# Sows the degrees of freedom and their critical t value. Can see that as the D.F. gets larger the critical t value gets closer to the 95% C.I.

lsmodel1 <- lm(height ~ type, data = darwin)

broom::tidy(lsmodel1) # Shows a summary of the model using broom, shows it as a table. 

tidy_model1 <- broom::tidy(lsmodel1)

tidy_model1[[2,2]] / tidy_model1[[2,3]] # Shows the observed t value.

####_________________----

# Paired T----

darwin %>% 
  mutate(pair = as_factor(pair)) %>% 
  lm(height ~ type + pair, data = .) %>% 
  broom::tidy()
# produces a tabel of the pairs and the information around them

# The second row now compares the mean heights of Crossed and Selfed plants when they are in the same pair
# Rows three to 16 compare the average difference of each pair (Crossed and Selfed combined) against pair 1

# Creating confidence intervals for the paired t-test
lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(1:2) # just show first two rows

m1 <- lm(height ~ type, data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(2:2) %>% 
  mutate(model="unpaired")

m2 <- lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(2:2) %>% 
  mutate(model="paired")

rbind(m1,m2) %>% 
  ggplot(aes(model, estimate))+
  geom_pointrange(aes(ymin=conf.high, ymax=conf.low))+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  theme_minimal()+
  coord_flip()
# This compares unpaired and paired models to see the difference in means with a 95% confidence interval. 
####______________----

# Effective Sizes----

# When our 95% confidence intervals do not overlap the intercept, this indicates we have difference in our means which is significant at α= 0.05. 
# The lower margin of our confidence intervals is the smallest/minimum effect size.
####______________----

# Type 1 and Type 2 errors----
# Type 1 error- The reality is that we know if we set an α = 0.05, that we run the risk of rejecting the null hypothesis incorrectly in 1 in 20 of our experiments
# Type 2 error- 
####_______________----

# Repeatability----

set.seed(1234)

myList <- vector("list", 20)
y <- tibble()

for (i in 1:length(myList)) { 
  
  x <-  rnorm(n=12, mean=2.6, sd=2.83)
  data <- tibble(x)
  temp <- lm(x~1, data=data) %>% 
    broom::tidy(conf.int=T) 
  y <- rbind(y,temp)  
  
}

y$`experiment number` <- rep(1:20)

# the new dataframe y contains the results of 20 new experiments

# Activity 1----
y %>% 
  mutate(`p value < 0.05` = if_else(p.value > 0.049, "non-significant", "significant")) %>% 
  group_by(`p value < 0.05`) %>% 
  summarise(`number of experiments`=n())

y %>% 
  ggplot(aes(x=`experiment number`, y=estimate))+
  geom_pointrange(aes(ymin = conf.low, ymax=conf.high))+
  labs(y = "Estimated mean effect of outcrossing")+
  geom_hline(linetype="dashed", yintercept=0.05)+
  theme_minimal()# produces a plot that shows estimated mean of outcrossing against the expected number. 


