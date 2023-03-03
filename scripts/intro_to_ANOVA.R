#Lesson 5----

library(tidyverse)
library(GGally)
library(emmeans)
library(performance)
library(broom.helpers)

#___________________----

#Maize Data----

lsmodel1 <- lm(height ~ type, data = darwin)

#__________________----

#ANOVA Tables----

#SST = the sum of squared differences between the data points and the grand mean

#SSR = the sum of the squared differences between the grand mean and the predicted position on the linear model

#SSE = the sum of the squared differences between the predicted position and the observed position

anova(lsmodel1)
# The second column gives the degrees of freedom, The third column is the sum of squares in the first row this is the SSR, and the second row SSE (remember that SST = SSR + SSE), The fourth column is the mean sum of squares MSR = SSR/(k-1) & MSE = SSE/(N-k). The fifth column is our F statistic (the signal-to-noise ratio). It is calculated by dividing the treatment variance by the residual error variance. F= MSR/MSE 
summary(lsmodel1)

pf(5.9395, 1, 28, lower.tail=FALSE)
# This uses the F-value, degrees of freedom and the lower tail to calculate the exact P-value, as shown in the ANOVA table. Easy to see naked P-values but need to know what test they came from, observed value of the test and the degrees of freedom. 
# Example of a good way to write up using the P-value: The self pollinated maize plants measured an average of 17.6 [16-19.1] (mean[95% CI]) inches high, while the cross-pollinated plants had a mean height of 20.2 [18.6-21.7] inches - a difference of 2.6 [-0.4-4.8] inches (one-way ANOVA: F1,28 = 5.9, P = 0.02).

#__________________----
# Two Way ANOVA----

lsmodel2 <- lm(height ~ type + as.factor(pair), data = darwin)
anova(lsmodel2)
#_________________----

#Summary----
#ANOVA tables can be built for any linear model. The tables partition the variance into signal(s) and noise, which can be compared using an F-test. For complex analyses where many pairwise comparisons could be performed, an initial F-test can provide the initial evidence for whether there are any differences at all, reducing the risk of overtesting and the false positives that can be generated.
#_________________----


