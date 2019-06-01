library("plyr")
library("ggplot2")

setwd("M:/Thesis documents/data")

participant_data <- read.csv("participant_data_combined.csv")
vr_data <- read.csv("participant_data_vr.csv")

#Age descriptives
age_mean <- mean(data$age)
age_std <- sd(data$age)
min_age <-  min(data$age)
max_age <- max(data$age) #this is an outlier
ggplot() +  geom_histogram(data, mapping = aes(age), binwidth = 2)

#Gender descriptives
#not yet complete!
gender <- count(data$gender)
male_percentage <- 37/80*100
female_percentage <- 43/80*100
#1=male, 2=female, 3=unkown
gender_vr <- count(vr_data$gender)

#PCA knowledge in advance
pca_knowledge <- count(data$pca_knowledge) 
ggplot() +  geom_histogram(data, mapping = aes(pca_knowledge), binwidth = 1)
#1=no, 2=recognize, 3=know

#VR experience
vr_experience <- count(vr_data$vr_experience)
ggplot() +  geom_histogram(vr_data, mapping = aes(vr_experience), binwidth = 1)

