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