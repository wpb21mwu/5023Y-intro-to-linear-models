#Lesson 6----

#Packages----

library(tidyverse)
library(GGally)
library(emmeans)
library(performance)
library(broom.helpers)
library(here)

#_____________________----

#Data Import----

biomass <- read_csv(here::here("data", "biomass_data.csv"))

glimpse(biomass)

biomass %>% 
  summarise(min=min(Biomass.m2, na.rm=TRUE), 
            max=max(Biomass.m2, na.rm=TRUE))

summary(biomass)

#____________________----

#One-Way ANOVA----

ls_1 <- lm(Biomass.m2 ~ FL, data = biomass)
summary(ls_1)

broom::tidy(ls_1, conf.int = T) # Gives us presise confidence intervals. 

GGally::ggcoef_model(ls_1,
                     show_p_values=FALSE,
                     signif_stars = FALSE,
                     conf.level=0.95) # We can visualise these differences with a coefficient plot, and see clearly that adding light by itself produce a slight increase in the mean biomass of our samples but the 95% confidence interval includes zero, so we don't have any real confidence that this is a consistent/true effect.

# combine the average mean differences of the light effect and fertiliser effect
coef(ls_1)[2] + coef(ls_1)[3] 

# compare this to the average difference of the combined treatment
coef(ls_1)[4]

#The mean biomass of the combined treatment is well above what we would expect from the additive effects alone. This suggests there may be a positive interaction (that light and fertiliser treatments produce a sum effect that is greater than could be predicted by looking at their individual effects).

#______________________----

#Testing for Interactions----

biomass %>% ggplot(aes(x=Fert, y=Biomass.m2, colour = Light, fill = Light, group = Light))+
  geom_jitter(width=0.1) +
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 3,
    shape = 23
  )+stat_summary(
    geom = "line",
    fun = "mean",
    size = 1, linetype = "dashed"
  )

ls_2 <- lm(Biomass.m2 ~ Fert + # main effect
             Light + # main effect
             Fert:Light, # interaction term
           data = biomass)

summary(ls_2)


GGally::ggcoef_model(ls_2,
                     show_p_values=FALSE,
                     signif_stars = FALSE,
                     conf.level=0.95)


#Model estimates and confidence intervals----

# model 1
coef(ls_1)[4]

# model 2
coef(ls_2)[2] + coef(ls_2)[3] + coef(ls_2)[4]
#_________________________----

#ANOVA Tables----

drop1(ls_2, test = "F") #In order to report an F statistic for the interaction effect, we need to carry out an F-test of two models, one with and one without the interaction effect. In order to report an F statistic for the interaction effect, we need to carry out an F-test of two models, one with and one without the interaction effect. 

# we have to remove the interaction term before we can keep using drop1()

ls_3 <- lm(Biomass.m2 ~ Fert + Light, data = biomass)

drop1(ls_3, test = "F") #  include reports of the main effect then estimates and confidence intervals should come from the full model, but we need to produce an interaction free model to produce accurate F-values (especially for unbalanced designs, see below).
#_________________----

#Balanced/ Unbalanced Designs----

#In an unbalanced design when you run the anova() function on your model, the order in which your variables were included can have an effect e.g. lm(Fert + Light) would give a different anova table to lm(Light + Fert). 


