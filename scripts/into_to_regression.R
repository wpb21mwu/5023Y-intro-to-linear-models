#Lesson 4, Regression----

#Packages----
library(tidyverse)
library(rstatix)
library(performance)
#___________________----

#Import Data----

janka <- read_csv("data/janka_data.csv")
summarise(janka)
