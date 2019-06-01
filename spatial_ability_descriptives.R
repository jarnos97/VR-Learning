library("plyr")
library("ggplot2")
library("psych")
library("kazaam")

setwd("M:/Thesis documents/data")

data <- read.csv("participant_data_combined.csv")
participant_vr <- read.csv("participant_data_vr.csv")
spatial_ability <- read.csv("spatial_ability_data.csv")
spatial_ability_paper <- read.csv("spatial_ability_data_paper.csv")
spatial_ability_vr <- read.csv("spatial_ability_data_vr.csv")

#plotting spatial ability distribution
qplot(spatial_ability$Spatial_Ability_Score,
      geom="histogram",
      binwidth = 0.15,  
      main = "Histogram of Spatial Ability", 
      xlab = "Means",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(0,1))

ggplot(spatial_ability, aes(sample = Spatial_Ability_Score)) +
  stat_qq(color='blue') +
  stat_qq_line(color='blue')+
  ggtitle("Q-Q plot of Spatial Ability")

#looking at mean and sd of spatial ability
mean_spatial_ability <- mean(spatial_ability$Spatial_Ability_Score)
sd_spatial_ability <- sd(spatial_ability$Spatial_Ability_Score)

#Looking at spatial ability per gender
spatial_gender <- data.frame(spatial_ability[,1:2], data[3])

spatial_gender2 <- split(spatial_gender, spatial_gender$gender)

spatial_men_mean <- mean(spatial_gender2$`1`$Spatial_Ability_Score)
spatial_men_sd <- sd(spatial_gender2$`1`$Spatial_Ability_Score)
spatial_female_mean <- mean(spatial_gender2$`2`$Spatial_Ability_Score)
spatial_female_sd <- sd(spatial_gender2$`2`$Spatial_Ability_Score)

#performing t-test to test if the difference between gender and sa is significant
t.test(spatial_gender$Spatial_Ability_Score ~ spatial_gender$gender)

#Determine spatial ability per conditon
  #paper
mean(spatial_ability_paper$Spatial_Ability_Score)
sd(spatial_ability_paper$Spatial_Ability_Score)
median(spatial_ability_paper$Spatial_Ability_Score)
min(spatial_ability_paper$Spatial_Ability_Score)
max(spatial_ability_paper$Spatial_Ability_Score)
skew(spatial_ability_paper$Spatial_Ability_Score)
kurtosi(spatial_ability_paper$Spatial_Ability_Score)

  #VR
mean(spatial_ability_vr$Spatial_Ability_Score)
sd(spatial_ability_vr$Spatial_Ability_Score)
median(spatial_ability_vr$Spatial_Ability_Score)
min(spatial_ability_vr$Spatial_Ability_Score)
max(spatial_ability_vr$Spatial_Ability_Score)
skew(spatial_ability_vr$Spatial_Ability_Score)
kurtosi(spatial_ability_vr$Spatial_Ability_Score)
