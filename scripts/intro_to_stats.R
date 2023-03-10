## Intro to Linear Models----
## Lesson 1

library(tidyverse)
library(here)
library(kableExtra)

darwin <- read_csv(here("data", "darwin.csv")) # imports the data from the data folder onto the console.

####___________ ----
#Standard Checks----

# check the structure of the data
glimpse(darwin)

# check data is in a tidy format
head(darwin)

# check variable names
colnames(darwin)


# clean up column names

darwin <- janitor::clean_names(darwin)

# check for duplication
darwin %>% 
  duplicated() %>% 
  sum()

# check for typos - by looking at impossible values
darwin %>% 
  summarise(min=min(height, na.rm=TRUE), 
            max=max(height, na.rm=TRUE))

# check for typos by looking at distinct characters/values

darwin %>% 
  distinct(pair)

darwin %>% 
  distinct(type)

# missing values
darwin %>% 
  is.na() %>% 
  sum()

# quick summary

summary(darwin)

####__________________----
# Making a plot using the darin data----
darwin %>% 
  ggplot(aes(x=type,
             y=height))+
  geom_point()

####_________________----
#Comparing Groups----
#Summerising the data

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height)) # Keep tables for very simple sets of data

# make a new object
darwin_summary <-darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height)) # new object summerieses the SD and mean in a table (simple data)

# make a summary plot
darwin_summary %>% 
  ggplot(aes(x=type,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw() # presents the SD and mean in a plot showing the differences more visually

# use kable extra functions to make a nice table (could be replaced with kable() if needed)
darwin_summary %>% 
  kbl(caption="Summary statistics of crossed and selfed maize plants") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left") # makes the data able to be viewed in the viewer tab of the workspace

####____________----
#Differences----

# pivot data to wide format then subtract Selfed plant heights from Crossed plant heights

darwin_wide <- darwin %>% 
  pivot_wider(names_from = type, values_from = height) %>% 
  mutate(difference = Cross - Self)

difference_summary <- darwin_wide %>% 
  summarise(mean=mean(difference),
            sd=sd(difference),
            n=n())

difference_summary # mean difference in SD of paired plants

difference_summary %>% 
  mutate(se= sd/sqrt(n)) # calculates the SE of the group sample 

####_______________----
#Uncertainty levels and Certain Limit Theorem ----

#Null hypothesis - there is no difference in the mean height of self vs crossed plants

#Alternate hypothesis - inbreeding reduces the fitness of the selfed plants, observed as selfed plants on average being smaller than crossed plants

#Create a sequence of 100 equally spaced numbers between -4 and 4
x <- seq(-4, 4, length=100)

#create a vector of values that shows the height of the probability distribution
#for each value in x
y <- dnorm(x)

#plot x and y as a scatterplot with connected lines (type = "l") and add
#an x-axis with custom labels
plot(x,y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c("-3s", "-2s", "-1s", "mean", "1s", "2s", "3s"))

# Confidence Intervals
lowerCI <- 2.62-(2*1.22) # calculating 95% confidence intervals

upperCI <- 2.62+(2*1.22)

lowerCI
upperCI


