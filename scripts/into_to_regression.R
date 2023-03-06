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
