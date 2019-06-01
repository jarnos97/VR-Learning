library('irr') 
library('ggplot2')

setwd("M:/Thesis documents/data")

#Measuring interrater reliability
interrater_data <- read.csv("interrater_data.csv")
interrater_data2 <- read.csv("interrater_data2.csv")
understanding <- read.csv("understanding_results_final.csv")
understanding_paper <- read.csv("understanding_results_paper.csv")
understanding_vr <- read.csv("understanding_results_vr.csv")
spatial_ability <- read.csv("spatial_ability_data.csv")

overall_agreement <- agree(interrater_data)
overall_kappa <- kappa2(interrater_data)

q1_agreement <- agree(interrater_data2[,1:2])
q1_kappa <- kappa2(interrater_data2[,1:2])

q2_agreement <- agree(interrater_data2[,3:4])
q2_kappa <- kappa2(interrater_data2[,3:4])

q3_agreement <- agree(interrater_data2[,5:6])
q3_kappa <- kappa2(interrater_data2[,5:6])

q4_agreement <- agree(interrater_data2[,7:8])
q4_kappa <- kappa2(interrater_data2[,7:8])

q5_agreement <- agree(interrater_data2[,9:10])
q5_kappa <- kappa2(interrater_data2[,9:10])

q6_agreement <- agree(interrater_data2[,11:12])
q6_kappa <- kappa2(interrater_data2[,11:12])

q7_agreement <- agree(interrater_data2[,13:14])
q7_kappa <- kappa2(interrater_data2[,13:14])

q8_agreement <- agree(interrater_data2[,15:16])
q8_kappa <- kappa2(interrater_data2[,15:16])

# Looking at the distribution of the scores
qplot(understanding$Total,
      geom="histogram",
      binwidth = 1,  
      main = "Histogram PCA understanding", 
      xlab = "Means",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(0,10))

ggplot(understanding, aes(sample = Total)) +
  stat_qq(color='blue') +
  stat_qq_line(color='blue')+
  ggtitle("Q-Q plot of PCA understanding")

#Mean and sd overall score
mean_pca_understanding <- mean(understanding$Total)
sd_pca_understanding <- sd(understanding$Total)

#Mean score per question:
mean_q1 <- mean(understanding$Q1)
mean_q2 <- mean(understanding$Q2)
mean_q3 <- mean(understanding$Q3)
mean_q4 <- mean(understanding$Q4) #visual question
mean_q5 <- mean(understanding$Q5)
mean_q6 <- mean(understanding$Q6)
mean_q7 <- mean(understanding$Q7) #visual question
mean_q8 <- mean(understanding$Q8) #visual question

#Looking distribution and average per condition
  #paper
mean_pca_understanding_paper <- mean(understanding_paper$Total)
sd_pca_understanding_paper <- sd(understanding_paper$Total)

qplot(understanding_paper$Total,
      geom="histogram",
      binwidth = 1,  
      main = "Histogram PCA understanding in Paper Condition", 
      xlab = "Means",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(0,10))

ggplot(understanding_paper, aes(sample = Total)) +
  stat_qq(color='blue') +
  stat_qq_line(color='blue')+
  ggtitle("Q-Q plot of PCA understanding in Paper Condition")

mean_q1_paper <- mean(understanding_paper$Q1)
mean_q2_paper <- mean(understanding_paper$Q2)
mean_q3_paper <- mean(understanding_paper$Q3)
mean_q4_paper <- mean(understanding_paper$Q4) #visual question
mean_q5_paper <- mean(understanding_paper$Q5)
mean_q6_paper <- mean(understanding_paper$Q6)
mean_q7_paper <- mean(understanding_paper$Q7) #visual question
mean_q8_paper <- mean(understanding_paper$Q8) #visual question

  #VR
mean_pca_understanding_vr <- mean(understanding_vr$Total)
sd_pca_understanding_vr <- sd(understanding_vr$Total)

qplot(understanding_vr$Total,
      geom="histogram",
      binwidth = 1,  
      main = "Histogram PCA understanding in VR Condition", 
      xlab = "Means",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(0,10))

ggplot(understanding_vr, aes(sample = Total)) +
  stat_qq(color='blue') +
  stat_qq_line(color='blue')+
  ggtitle("Q-Q plot of PCA understanding in VR Condition")

#Looking at mean score and sd per question
mean_q1_vr <- mean(understanding_vr$Q1)
mean_q2_vr <- mean(understanding_vr$Q2)
mean_q3_vr <- mean(understanding_vr$Q3)
mean_q4_vr <- mean(understanding_vr$Q4) #visual question
mean_q5_vr <- mean(understanding_vr$Q5)
mean_q6_vr <- mean(understanding_vr$Q6)
mean_q7_vr <- mean(understanding_vr$Q7) #visual question
mean_q8_vr <- mean(understanding_vr$Q8) #visual question

sd_q1_vr <- sd(understanding_vr$Q1)
sd_q2_vr <- sd(understanding_vr$Q2)
sd_q3_vr <- sd(understanding_vr$Q3)
sd_q4_vr <- sd(understanding_vr$Q4) #visual question
sd_q5_vr <- sd(understanding_vr$Q5)
sd_q6_vr <- sd(understanding_vr$Q6)
sd_q7_vr <- sd(understanding_vr$Q7) #visual question
sd_q8_vr <- sd(understanding_vr$Q8) #visual question

#Making a correlation matrix between spatial ability and pca
cor_data <- cbind(spatial_ability$Spatial_Ability_Score, understanding$Total)
colnames(cor_data) <- c('spatial_ability_score', "pca_understanding_score")
cor_data <- data.frame(cor_data)
cor(cor_data)

  #visualize with scatterplot
ggplot(cor_data, aes(x=spatial_ability_score, y=pca_understanding_score)) + geom_point()


# all descriptives for understanding per condtion
  #paper
mean(understanding_paper$Total)
sd(understanding_paper$Total)
median(understanding_paper$Total)
min(understanding_paper$Total)
max(understanding_paper$Total)
skew(understanding_paper$Total)
kurtosi(understanding_paper$Total)

  #VR
mean(understanding_vr$Total)
sd(understanding_vr$Total)
  median(understanding_vr$Total)
min(understanding_vr$Total)
max(understanding_vr$Total)
skew(understanding_vr$Total)
kurtosi(understanding_vr$Total)