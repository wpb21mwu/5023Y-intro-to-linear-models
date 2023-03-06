#Lesson 4, Regression----

#Packages----
library(tidyverse)
library(rstatix)
library(performance)
#___________________----

#Import Data----

janka <- read_csv("data/janka_data.csv")
summarise(janka)

#__________________----

#Exploratory Analysis----

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()
# Wood density and timber hardness appear to be positively related, and the relationship appears to be fairly linear. We can look at a simple strength of this association between dens and hardness using correlation.

# Generating Pearson's R:

janka %>% 
  cor_test(dens, hardness) # Perfect correlations are -1 and 1. All correlation will range between these two values. As the correlation value for janka is 0.97 we can say that there is a very strong positive correlation. 
#______________----

#Regression in R----

# We can fir a regression the same way we fit a linear model, taking care and noting that our predictor is now continuous and not categorical. 

janka_ls1 <- lm(hardness ~ dens, data = janka) # NOTE- make sure that on the left is the response variable and the right is the predictor variable. 

# specify linear model method for line fitting

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")

# Shaded area is our 95% C.I. It is wider at either end as there are two types of uncertainty being tested. When you combine both uncertainties of the prediction there is a spread between the high and low estimates. The further away from the center of the data you get (in either direction), the uncertainty of the slope becomes a large and more noticeable factor, thus the limits widen.

janka_ls1 %>% 
  broom::tidy()
#___________________----

#Mean Centered Regression----

dens_mean <- janka %>% 
  summarise(mean_dens=mean(dens))

janka %>% 
  mutate(centered_dens = dens-pull(dens_mean)) %>% 
  lm(hardness ~ centered_dens, data = .) %>% 
  broom::tidy()

broom::tidy(janka_ls1, conf.int=T, conf.level=0.95)

janka_ls1 %>% 
  broom::glance()
#___________________----

#Assumptions----

janka_ls1 %>% 
  broom::augment() %>% 
  head()

augmented_ls1 <- janka_ls1 %>% 
  broom::augment()

augmented_ls1 %>% 
  ggplot(aes(x=dens, 
             y=.fitted))+
  geom_line()+ 
  geom_point(aes(x=dens, 
                 y=hardness))+
  geom_segment(aes(x=dens, 
                   xend=dens, 
                   y=.fitted, 
                   yend=hardness), 
               linetype="dashed", colour="red") # Shows residuals from the regression line. A perfect correlation will have 0 residuals. This is very unlikely instead we are likely to see 1.that there is a 'normal distribution' to the residuals e.g. more residuals close to the mean, and fewer further away in a rough z-distribution. 2. We also want to see homogeneity of the residuals e.g. it would be a bad model if the average error was greater at one end of the model than the other. This might mean we have more uncertainty in the slope of the line for large values over small values or vice versa.


# A line connecting all the data points in order 
p1 <- augmented_ls1 %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_line()+
  ggtitle("Full Data")

# Plotting the fitted values against the independent e.g. our regression line
p2 <- augmented_ls1 %>% 
  ggplot(aes(x=dens, y=.fitted))+
  geom_line()+
  ggtitle("Linear trend")

# Plotting the residuals against the fitted values e.g. remaining variance
p3 <- augmented_ls1 %>% 
  ggplot(aes(x=.fitted, y=.resid))+
  geom_hline(yintercept=0, colour="white", size=5)+
  geom_line()+
  ggtitle("Remaining \npattern")


library(patchwork)
p1+p2+p3







