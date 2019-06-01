#Flow experience
library("plyr")
library("ggplot2")
library("psych")
library("kazaam")
library(xlsx)

setwd("M:/Thesis documents/data")
flow_data_combined <- read.csv("flow_data.csv")
flow_vr <- read.csv("flow_data_vr.csv")
flow_paper <- read.csv("flow_data_paper.csv")

#calculate mean and standard deviation per question per condition
# 1=strongly dissagree, 5=strongly agree
q1_m_paper <- mean(flow_paper$Q1, na.rm = TRUE)
q1_sd_paper <- sd(flow_paper$Q1, na.rm = TRUE)
q1_m_vr <- mean(flow_vr$Q1, na.rm = TRUE)
q1_sd_vr <- sd(flow_vr$Q1, na.rm = TRUE)

q2_m_paper <- mean(flow_paper$Q2, na.rm = TRUE)
q2_sd_paper <- sd(flow_paper$Q2, na.rm = TRUE)
q2_m_vr <- mean(flow_vr$Q2, na.rm = TRUE)
q2_sd_vr <- sd(flow_vr$Q2, na.rm = TRUE)

q3_m_paper <- mean(flow_paper$Q3, na.rm = TRUE)
q3_sd_paper <- sd(flow_paper$Q3, na.rm = TRUE)
q3_m_vr <- mean(flow_vr$Q3, na.rm = TRUE)
q3_sd_vr <- sd(flow_vr$Q3, na.rm = TRUE)

q4_m_paper <- mean(flow_paper$Q4, na.rm = TRUE)
q4_sd_paper <- sd(flow_paper$Q4, na.rm = TRUE)
q4_m_vr <- mean(flow_vr$Q4, na.rm = TRUE)
q4_sd_vr <- sd(flow_vr$Q4, na.rm = TRUE)

q5_m_paper <- mean(flow_paper$Q5, na.rm = TRUE)
q5_sd_paper <- sd(flow_paper$Q5, na.rm = TRUE)
q5_m_vr <- mean(flow_vr$Q5, na.rm = TRUE)
q5_sd_vr <- sd(flow_vr$Q5, na.rm = TRUE)

q6_m_paper <- mean(flow_paper$Q6, na.rm = TRUE)
q6_sd_paper <- sd(flow_paper$Q6, na.rm = TRUE)
q6_m_vr <- mean(flow_vr$Q6, na.rm = TRUE)
q6_sd_vr <- sd(flow_vr$Q6, na.rm = TRUE)

q7_m_paper <- mean(flow_paper$Q7, na.rm = TRUE)
q7_sd_paper <- sd(flow_paper$Q7, na.rm = TRUE)

q7_m_vr <- mean(flow_vr$Q7, na.rm = TRUE)
q7_sd_vr <- sd(flow_vr$Q7, na.rm = TRUE)

q8_m_paper <- mean(flow_paper$Q8, na.rm = TRUE)
q8_sd_paper <- sd(flow_paper$Q8, na.rm = TRUE)
q8_m_vr <- mean(flow_vr$Q8, na.rm = TRUE)
q8_sd_vr <- sd(flow_vr$Q8, na.rm = TRUE)

q9_m_paper <- mean(flow_paper$Q9, na.rm = TRUE)
q9_sd_paper <- sd(flow_paper$Q9, na.rm = TRUE)
q9_m_vr <- mean(flow_vr$Q9, na.rm = TRUE)
q9_sd_vr <- sd(flow_vr$Q9, na.rm = TRUE)

q10_m_paper <- mean(flow_paper$Q10, na.rm = TRUE)
q10_sd_paper <- sd(flow_paper$Q10, na.rm = TRUE)
q10_m_vr <- mean(flow_vr$Q10, na.rm = TRUE)
q10_sd_vr <- sd(flow_vr$Q10, na.rm = TRUE)

q11_m_paper <- mean(flow_paper$Q11, na.rm = TRUE)
q11_sd_paper <- sd(flow_paper$Q11, na.rm = TRUE)
q11_m_vr <- mean(flow_vr$Q11, na.rm = TRUE)
q11_sd_vr <- sd(flow_vr$Q11, na.rm = TRUE)

q12_m_paper <- mean(flow_paper$Q12, na.rm = TRUE)
q12_sd_paper <- sd(flow_paper$Q12, na.rm = TRUE)
q12_m_vr <- mean(flow_vr$Q12, na.rm = TRUE)
q12_sd_vr <- sd(flow_vr$Q12, na.rm = TRUE)

q13_m_paper <- mean(flow_paper$Q13, na.rm = TRUE)
q13_sd_paper <- sd(flow_paper$Q13, na.rm = TRUE)
q13_m_vr <- mean(flow_vr$Q13, na.rm = TRUE)
q13_sd_vr <- sd(flow_vr$Q13, na.rm = TRUE)

q14_m_paper <- mean(flow_paper$Q14, na.rm = TRUE)
q14_sd_paper <- sd(flow_paper$Q14, na.rm = TRUE)
q14_m_vr <- mean(flow_vr$Q14, na.rm = TRUE)
q14_sd_vr <- sd(flow_vr$Q14, na.rm = TRUE)

q15_m_paper <- mean(flow_paper$Q15, na.rm = TRUE)
q15_sd_paper <- sd(flow_paper$Q15, na.rm = TRUE)
q15_m_vr <- mean(flow_vr$Q15, na.rm = TRUE)
q15_sd_vr <- sd(flow_vr$Q15, na.rm = TRUE)

q16_m_paper <- mean(flow_paper$Q16, na.rm = TRUE)
q16_sd_paper <- sd(flow_paper$Q16, na.rm = TRUE)
q16_m_vr <- mean(flow_vr$Q16, na.rm = TRUE)
q16_sd_vr <- sd(flow_vr$Q16, na.rm = TRUE)

q17_m_paper <- mean(flow_paper$Q17, na.rm = TRUE)
q17_sd_paper <- sd(flow_paper$Q17, na.rm = TRUE)
q17_m_vr <- mean(flow_vr$Q17, na.rm = TRUE)
q17_sd_vr <- sd(flow_vr$Q17, na.rm = TRUE)

q18_m_paper <- mean(flow_paper$Q18, na.rm = TRUE)
q18_sd_paper <- sd(flow_paper$Q18, na.rm = TRUE)
q18_m_vr <- mean(flow_vr$Q18, na.rm = TRUE)
q18_sd_vr <- sd(flow_vr$Q18, na.rm = TRUE)

q19_m_paper <- mean(flow_paper$Q19, na.rm = TRUE)
q19_sd_paper <- sd(flow_paper$Q19, na.rm = TRUE)
q19_m_vr <- mean(flow_vr$Q19, na.rm = TRUE)
q19_sd_vr <- sd(flow_vr$Q19, na.rm = TRUE)

q20_m_paper <- mean(flow_paper$Q20, na.rm = TRUE)
q20_sd_paper <- sd(flow_paper$Q20, na.rm = TRUE)
q20_m_vr <- mean(flow_vr$Q20, na.rm = TRUE)
q20_sd_vr <- sd(flow_vr$Q20, na.rm = TRUE)

q21_m_paper <- mean(flow_paper$Q21, na.rm = TRUE)
q21_sd_paper <- sd(flow_paper$Q21, na.rm = TRUE)
q21_m_vr <- mean(flow_vr$Q21, na.rm = TRUE)
q21_sd_vr <- sd(flow_vr$Q21, na.rm = TRUE)

q22_m_paper <- mean(flow_paper$Q22, na.rm = TRUE)
q22_sd_paper <- sd(flow_paper$Q22, na.rm = TRUE)
q22_m_vr <- mean(flow_vr$Q22, na.rm = TRUE)
q22_sd_vr <- sd(flow_vr$Q22, na.rm = TRUE)

q23_m_paper <- mean(flow_paper$Q23, na.rm = TRUE)
q23_sd_paper <- sd(flow_paper$Q23, na.rm = TRUE)
q23_m_vr <- mean(flow_vr$Q23, na.rm = TRUE)
q23_sd_vr <- sd(flow_vr$Q23, na.rm = TRUE)

q24_m_paper <- mean(flow_paper$Q24, na.rm = TRUE)
q24_sd_paper <- sd(flow_paper$Q24, na.rm = TRUE)
q24_m_vr <- mean(flow_vr$Q24, na.rm = TRUE)
q24_sd_vr <- sd(flow_vr$Q24, na.rm = TRUE)

q25_m_paper <- mean(flow_paper$Q25, na.rm = TRUE)
q25_sd_paper <- sd(flow_paper$Q25, na.rm = TRUE)
q25_m_vr <- mean(flow_vr$Q25, na.rm = TRUE)
q25_sd_vr <- sd(flow_vr$Q25, na.rm = TRUE)

q26_m_paper <- mean(flow_paper$Q26, na.rm = TRUE)
q26_sd_paper <- sd(flow_paper$Q26, na.rm = TRUE)
q26_m_vr <- mean(flow_vr$Q26, na.rm = TRUE)
q26_sd_vr <- sd(flow_vr$Q26, na.rm = TRUE)

q27_m_paper <- mean(flow_paper$Q27, na.rm = TRUE)
q27_sd_paper <- sd(flow_paper$Q27, na.rm = TRUE)
q27_m_vr <- mean(flow_vr$Q27, na.rm = TRUE)
q27_sd_vr <- sd(flow_vr$Q27, na.rm = TRUE)

q28_m_paper <- mean(flow_paper$Q28, na.rm = TRUE)
q28_sd_paper <- sd(flow_paper$Q28, na.rm = TRUE)
q28_m_vr <- mean(flow_vr$Q28, na.rm = TRUE)
q28_sd_vr <- sd(flow_vr$Q28, na.rm = TRUE)

q29_m_paper <- mean(flow_paper$Q29, na.rm = TRUE)
q29_sd_paper <- sd(flow_paper$Q29, na.rm = TRUE)
q29_m_vr <- mean(flow_vr$Q29, na.rm = TRUE)
q29_sd_vr <- sd(flow_vr$Q29, na.rm = TRUE)

q30_m_paper <- mean(flow_paper$Q30, na.rm = TRUE)
q30_sd_paper <- sd(flow_paper$Q30, na.rm = TRUE)
q30_m_vr <- mean(flow_vr$Q30, na.rm = TRUE)
q30_sd_vr <- sd(flow_vr$Q30, na.rm = TRUE)

q31_m_paper <- mean(flow_paper$Q31, na.rm = TRUE)
q31_sd_paper <- sd(flow_paper$Q31, na.rm = TRUE)
q31_m_vr <- mean(flow_vr$Q31, na.rm = TRUE)
q31_sd_vr <- sd(flow_vr$Q31, na.rm = TRUE)

q32_m_paper <- mean(flow_paper$Q32, na.rm = TRUE)
q32_sd_paper <- sd(flow_paper$Q32, na.rm = TRUE)
q32_m_vr <- mean(flow_vr$Q32, na.rm = TRUE)
q32_sd_vr <- sd(flow_vr$Q32, na.rm = TRUE)

#Mean and sd per dimension (8) and per condition
mean_challenge_skill_paper <- mean(q1_m_paper, q9_m_paper, q17_m_paper, q25_m_paper)
sd_challenge_skill_paper <- ( q1_sd_paper+ q9_sd_paper+ q17_sd_paper+ q25_sd_paper)/4

mean_challenge_skill_vr <- mean(q1_m_vr, q9_m_vr, q17_m_vr, q25_m_vr)
sd_challenge_skill_vr <- ( q1_sd_vr+ q9_sd_vr+ q17_sd_vr+ q25_sd_vr)/4

mean_clear_goals_paper <- mean(q2_m_paper, q10_m_paper, q18_m_paper, q26_m_paper)
sd_clear_goals_paper <- (q2_sd_paper+ q10_sd_paper +q18_sd_paper+ q26_sd_paper)/4

mean_clear_goals_vr <- mean(q2_m_vr, q10_m_vr, q18_m_vr, q26_m_vr)
sd_clear_goals_vr <- (q2_sd_vr+ q10_sd_vr +q18_sd_vr+ q26_sd_vr)/4

mean_feedback_paper <- mean(q3_m_paper, q11_m_paper, q19_m_paper, q27_m_paper)
sd_feedback_paper <- (q3_sd_paper + q11_sd_paper+ q19_sd_paper+ q27_sd_paper)/4

mean_feedback_vr <- mean(q3_m_vr, q11_m_vr, q19_m_vr, q27_m_vr)
sd_feedback_vr <- (q3_sd_vr + q11_sd_vr+ q19_sd_vr+ q27_sd_vr)/4

mean_concentration_paper <- mean(q4_m_paper, q12_m_paper, q20_m_paper, q28_m_paper)
sd_concentration_paper <- (q4_sd_paper+ q12_sd_paper+ q20_sd_paper+ q28_sd_paper)/4

mean_concentration_vr <- mean(q4_m_vr, q12_m_vr, q20_m_vr, q28_m_vr)
sd_concentration_vr <- (q4_sd_vr+ q12_sd_vr+ q20_sd_vr+ q28_sd_vr)/4

mean_control_paper <- mean(q5_m_paper, q13_m_paper, q21_m_paper, q29_m_paper)
sd_control_paper <- (q5_sd_paper+ q13_sd_paper+ q21_sd_paper+ q29_sd_paper)/4

mean_control_vr <- mean(q5_m_vr, q13_m_vr, q21_m_vr, q29_m_vr)
sd_control_vr <- (q5_sd_vr+ q13_sd_vr+ q21_sd_vr+ q29_sd_vr)/4

mean_self_consciousness_paper <- mean(q6_m_paper, q14_m_paper, q22_m_paper, q30_m_paper)
sd_self_consciousness_paper <- (q6_sd_paper+ q14_sd_paper+ q22_sd_paper+ q30_sd_paper)/4

mean_self_consciousness_vr <- mean(q6_m_vr, q14_m_vr, q22_m_vr, q30_m_vr)
sd_self_consciousness_vr <- (q6_sd_vr+ q14_sd_vr+ q22_sd_vr+ q30_sd_vr)/4

mean_time_paper <- mean(q7_m_paper, q15_m_paper, q23_m_paper, q31_m_paper)
sd_time_paper <- (q7_sd_paper+ q15_sd_paper+ q23_sd_paper+ q31_sd_paper)/4

mean_time_vr <- mean(q7_m_vr, q15_m_vr, q23_m_vr, q31_m_vr)
sd_time_rr <- (q7_sd_vr+ q15_sd_vr+ q23_sd_vr+ q31_sd_vr)/4

mean_autotelic_paper <- mean(q8_m_paper, q16_m_paper, q24_m_paper, q32_m_paper)
sd_autotelic_paper <- (q8_sd_paper+ q16_sd_paper+ q24_sd_paper+ q32_sd_paper)/4

mean_autotelic_vr <- mean(q8_m_vr, q16_m_vr, q24_m_vr, q32_m_vr)
sd_autotelic_vr <- (q8_sd_vr+ q16_sd_vr+ q24_sd_vr+ q32_sd_vr)/4

#Making boxplots to compare each dimension per condition 
boxplot_challenge_skill <- read.csv('boxplot_challenge_skill.csv')
ggplot(boxplot_challenge_skill, aes(x = dimension_condition, y = score)) +
  geom_boxplot(color='blue')

boxplot_goals <- read.csv('boxplot_goals.csv')
ggplot(boxplot_goals, aes(x = dimension_condition, y = score)) +
  geom_boxplot(color='blue')

boxplot_feedback <- read.csv('boxplot_feedback.csv')
ggplot(boxplot_feedback, aes(x = dimension_condition, y = score)) +
  geom_boxplot(color='blue')

boxplot_concentration <- read.csv('boxplot_concentration.csv')
ggplot(boxplot_concentration, aes(x = dimension_condition, y = score)) +
  geom_boxplot(color='blue')

boxplot_control <- read.csv('boxplot_control.csv')
ggplot(boxplot_control, aes(x = dimension_condition, y = score)) +
  geom_boxplot(color='blue')

boxplot_self_consciousness <- read.csv('boxplot_self_consciousness.csv')
ggplot(boxplot_self_consciousness, aes(x = dimension_condition, y = score)) +
  geom_boxplot(color='blue')

boxplot_time <- read.csv('boxplot_time.csv')
ggplot(boxplot_time, aes(x = dimension_condition, y = score)) +
  geom_boxplot(color='blue')

boxplot_autotelic <- read.csv('boxplot_autotelic.csv')
ggplot(boxplot_autotelic, aes(x = dimension_condition, y = score)) +
  geom_boxplot(color='blue')

#Mean flow score (regardless of condition)
flow_score <- data.frame(ID=flow_data_combined[,1], Means=rowMeans(flow_data_combined[,-1], na.rm = TRUE))
mean_flow_score <- mean(flow_score$Means, na.rm = TRUE)
sd_flow_score <- sd(flow_score$Means, na.rm = TRUE)

#saving flow_score as data file
write.xlsx(flow_score, "M:/Thesis documents/data/flow_score.xlsx")

#flow per participant
flow_score_paper <- data.frame(ID=flow_paper[,1], Means=rowMeans(flow_paper[,-1], na.rm = TRUE))
qplot(flow_score_paper$Means,
      geom="histogram",
      binwidth = 0.5,  
      main = "Histogram for Flow in Paper Condition", 
      xlab = "Means",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(1,5))

ggplot(flow_score_paper, aes(sample = Means)) +
  stat_qq(color='blue') +
  stat_qq_line(color='blue')+
  ggtitle("Q-Q plot for Flow in Paper condition")

flow_score_vr <- data.frame(ID=flow_vr[,1], Means=rowMeans(flow_vr[,-1], na.rm = TRUE))
qplot(flow_score_vr$Means,
      geom="histogram",
      binwidth = 0.5,  
      main = "Histogram for Flow in VR Condition", 
      xlab = "Means",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(1,5))

ggplot(flow_score_vr, aes(sample = Means)) +
  stat_qq(color='blue') +
  stat_qq_line(color='blue')+
  ggtitle("Q-Q plot for Flow in VR condition")
  
# calculating all descriptives for flow per condition
  #paper
mean(flow_score_paper$Means, na.rm = TRUE)
sd(flow_score_paper$Means, na.rm = TRUE)
median(flow_score_paper$Means, na.rm = TRUE)
min(flow_score_paper$Means, na.rm = TRUE)
max(flow_score_paper$Means, na.rm = TRUE)
skew(flow_score_paper$Means, na.rm = TRUE)
kurtosi(flow_score_paper$Means, na.rm = TRUE)
  
  #VR
mean(flow_score_vr$Means, na.rm = TRUE)
sd(flow_score_vr$Means, na.rm = TRUE)
median(flow_score_vr$Means, na.rm = TRUE)
min(flow_score_vr$Means, na.rm = TRUE)
max(flow_score_vr$Means, na.rm = TRUE)
skew(flow_score_vr$Means, na.rm = TRUE)
kurtosi(flow_score_vr$Means, na.rm = TRUE)

#
#
#Calculating cronbach's alpha (construct validity) 
flow_data_no_id <- data.frame(flow_data_combined[,-1])#deleting the ID
alpha(flow_data_no_id, na.rm = TRUE, check.keys = TRUE) #raw_alpha = 0.92

#Making a correlation matrix
cor(na.omit(flow_data_no_id))

#per dimension
challenge_skill <- data.frame(flow_data_no_id[,c(1,9,17,25)])
clear_goals <- data.frame(flow_data_no_id[,c(2,10,18,26)])
feedback <- data.frame(flow_data_no_id[,c(3,11,19,27)])
concentration <- data.frame(flow_data_no_id[,c(4,12,20,28)])
control <- data.frame(flow_data_no_id[,c(5,13,21,29)])
self_consciousness <- data.frame(flow_data_no_id[,c(6,14,22,30)])
time <- data.frame(flow_data_no_id[,c(7,15,23,31)])
autotelic <- data.frame(flow_data_no_id[,c(8,16,24,32)])

alpha(challenge_skill, na.rm = TRUE, check.keys = TRUE) #raw_alpha = 0.77
alpha(clear_goals, na.rm = TRUE, check.keys = TRUE) #raw_alpha = 0.81
alpha(feedback, na.rm = TRUE, check.keys = TRUE) #raw_alpha = 0.87
alpha(concentration, na.rm = TRUE, check.keys = TRUE) #raw_alpha = 0.84
alpha(control, na.rm = TRUE, check.keys = TRUE) #raw_alpha = 0.81
alpha(self_consciousness, na.rm = TRUE, check.keys = TRUE) #raw_alpha = 0.77
alpha(time, na.rm = TRUE, check.keys = TRUE) #raw_alpha = 0.67
alpha(autotelic, na.rm = TRUE, check.keys = TRUE) #raw_alpha = 0.0.87

#Calculating the mean flow score per dimension (regardless of condition)
a <- mean(challenge_skill$Q1, na.rm = TRUE)
b <- mean(challenge_skill$Q9, na.rm = TRUE)
c <-mean(challenge_skill$Q17, na.rm = TRUE)
d <-mean(challenge_skill$Q25, na.rm = TRUE)
mean_challenge_skill <- mean(a,b,c,d)

a <- mean(clear_goals$Q2, na.rm = TRUE)
b <- mean(clear_goals$Q10, na.rm = TRUE)
c <-mean(clear_goals$Q18, na.rm = TRUE)
d <-mean(clear_goals$Q26, na.rm = TRUE)
mean_clear_goals <- mean(a,b,c,d)

a <- mean(feedback$Q3, na.rm = TRUE)
b <- mean(feedback$Q11, na.rm = TRUE)
c <-mean(feedback$Q19, na.rm = TRUE)
d <-mean(feedback$Q27, na.rm = TRUE)
mean_feedback <- mean(a,b,c,d)

a <- mean(concentration$Q4, na.rm = TRUE)
b <- mean(concentration$Q12, na.rm = TRUE)
c <-mean(concentration$Q20, na.rm = TRUE)
d <-mean(concentration$Q28, na.rm = TRUE)
mean_concentration <- mean(a,b,c,d)

a <- mean(control$Q5, na.rm = TRUE)
b <- mean(control$Q13, na.rm = TRUE)
c <-mean(control$Q21, na.rm = TRUE)
d <-mean(control$Q29, na.rm = TRUE)
mean_control <- mean(a,b,c,d)

a <- mean(self_consciousness$Q6, na.rm = TRUE)
b <- mean(self_consciousness$Q14, na.rm = TRUE)
c <-mean(self_consciousness$Q22, na.rm = TRUE)
d <-mean(self_consciousness$Q30, na.rm = TRUE)
mean_self_consciousness <- mean(a,b,c,d)

a <- mean(time$Q7, na.rm = TRUE)
b <- mean(time$Q15, na.rm = TRUE)
c <-mean(time$Q23, na.rm = TRUE)
d <-mean(time$Q31, na.rm = TRUE)
mean_time <- mean(a,b,c,d)

a <- mean(autotelic$Q8, na.rm = TRUE)
b <- mean(autotelic$Q16, na.rm = TRUE)
c <-mean(autotelic$Q24, na.rm = TRUE)
d <-mean(autotelic$Q32, na.rm = TRUE)
mean_autotelic <- mean(a,b,c,d)

#Making a histogram and qq-plot for each dimension
  #first I redefine all dimension dataframes
challenge_skill <- data.frame(flow_data_combined[,c(1,2,10,18,26)])
clear_goals <- data.frame(flow_data_combined[,c(1,3,11,19,27)])
feedback <- data.frame(flow_data_combined[,c(1,4,12,20,28)])
concentration <- data.frame(flow_data_combined[,c(1,5,13,21,29)])
control <- data.frame(flow_data_combined[,c(1,6,14,22,30)])
self_consciousness <- data.frame(flow_data_combined[,c(1,7,15,23,31)])
time <- data.frame(flow_data_combined[,c(1,8,16,24,32)])
autotelic <- data.frame(flow_data_combined[,c(1,9,17,25,33)])

  #Then we make a mean dimension score per participant
challenge_skill_score <- data.frame(ID=challenge_skill[,1], Means=rowMeans(challenge_skill[,-1], na.rm = TRUE))
clear_goals_score <- data.frame(ID=clear_goals[,1], Means=rowMeans(clear_goals[,-1], na.rm = TRUE))
feedback_score <- data.frame(ID=feedback[,1], Means=rowMeans(feedback[,-1], na.rm = TRUE))
concentration_score <- data.frame(ID=concentration[,1], Means=rowMeans(concentration[,-1], na.rm = TRUE))
control_score <- data.frame(ID=control[,1], Means=rowMeans(control[,-1], na.rm = TRUE))
self_consciousness_score <- data.frame(ID=self_consciousness[,1], Means=rowMeans(self_consciousness[,-1], na.rm = TRUE))
time_score <- data.frame(ID=time[,1], Means=rowMeans(time[,-1], na.rm = TRUE))
autotelic_score <- data.frame(ID=autotelic[,1], Means=rowMeans(autotelic[,-1], na.rm = TRUE))

  #Now we can make the histogram per dimension
qplot(challenge_skill_score$Means,
      geom="histogram",
      binwidth = 0.5,  
      main = "Histogram for Challenge-Skill score", 
      xlab = "Means",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(1,5))

qplot(clear_goals_score$Means,
      geom="histogram",
      binwidth = 0.5,  
      main = "Histogram for Clear Goals score", 
      xlab = "Means",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(1,5))

qplot(feedback_score$Means,
      geom="histogram",
      binwidth = 0.5,  
      main = "Histogram for Unambiguous Feedback score", 
      xlab = "Means",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(1,5))

qplot(concentration_score$Means,
      geom="histogram",
      binwidth = 0.5,  
      main = "Histogram for Concentration score", 
      xlab = "Means",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(1,5))

qplot(control_score$Means,
      geom="histogram",
      binwidth = 0.5,  
      main = "Histogram for Sense of Control score", 
      xlab = "Means",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(1,5))

qplot(self_consciousness_score$Means,
      geom="histogram",
      binwidth = 0.5,  
      main = "Histogram for Loss of Self-Consciousness score", 
      xlab = "Means",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(1,5))

qplot(time_score$Means,
      geom="histogram",
      binwidth = 0.5,  
      main = "Histogram for Transfomrmation of Time score", 
      xlab = "Means",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(1,5))

qplot(autotelic_score$Means,
      geom="histogram",
      binwidth = 0.5,  
      main = "Histogram for Autotelic score", 
      xlab = "Means",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(1,5))

  #And qq-plots
ggplot(challenge_skill_score, aes(sample = Means)) +
  stat_qq(color='blue') +
  stat_qq_line(color='blue')+
  ggtitle("Q-Q plot for Challenge-Skill Score")


ggplot(clear_goals_score, aes(sample = Means)) +
  stat_qq(color='blue') +
  stat_qq_line(color='blue')+
  ggtitle("Q-Q plot for Clear Goals Score")

ggplot(feedback_score, aes(sample = Means)) +
  stat_qq(color='blue') +
  stat_qq_line(color='blue')+
  ggtitle("Q-Q plot for Unambiguous Feedback Score")

ggplot(concentration_score, aes(sample = Means)) +
  stat_qq(color='blue') +
  stat_qq_line(color='blue')+
  ggtitle("Q-Q plot for Concentration Score")

ggplot(control_score, aes(sample = Means)) +
  stat_qq(color='blue') +
  stat_qq_line(color='blue')+
  ggtitle("Q-Q plot for Sense of Control Score")

ggplot(self_consciousness_score, aes(sample = Means)) +
  stat_qq(color='blue') +
  stat_qq_line(color='blue')+
  ggtitle("Q-Q plot for Self-Consciousness Score")

ggplot(time_score, aes(sample = Means)) +
  stat_qq(color='blue') +
  stat_qq_line(color='blue')+
  ggtitle("Q-Q plot for Transformation of Time Score")

ggplot(autotelic_score, aes(sample = Means)) +
  stat_qq(color='blue') +
  stat_qq_line(color='blue')+
  ggtitle("Q-Q plot for Autotelic Experience Score")