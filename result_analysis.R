library(data.table)
library(MASS)

setwd("M:/Thesis documents/data")

data <- read.csv("data_complete.csv")

data$condition.f <- factor(data$condition)

#Looking at hte effect of spatial ability on understanding
model7 <- lm(understanding_score ~ sa_score, data = data)
summary(model7) #not significant

#Now only look at the paper condition to check if the VR condition might have canceled out the effect of Flow. 
data_temp <- split.data.frame(data, f=data$condition)
data_paper <- data.frame(data_temp[1])
data_paper <- data_paper[-c(12), ] #row 15 is actually twelve here, because of the split
data_vr <- data.frame(data_temp[2])
model8 <- lm(X1.understanding_score ~ X1.sa_score, data = data_paper)
summary(model8) 

#Performing T-tests for the condition on spatial ability and flow and understanding??
t.test(data$flow_score ~ data$condition)
t.test(data$sa_score ~ data$condition)
t.test(data$understanding_score ~ data$condition) 

#Making a correaltion matrix for Flow, Spatial Ability and Understanding. OVerall, per condtion and per dimension of flow.
  #overal
cor_data <- data.frame(data[c(2,3,5)]) #making a subset of the data we need
cor(na.omit(cor_data))

  #per condition
data_cor2 <- split.data.frame(data, f=data$condition) #splitting data based on condition
data_cor_paper <- data.frame(data_cor2[1])
cor_data_paper <- data.frame(data_cor_paper[c(2,3,5)])
data_cor_vr <- data.frame(data_cor2[2])
cor_data_vr <- data.frame(data_cor_vr[c(2,3,5)])

cor(na.omit(cor_data_paper))
cor(na.omit(cor_data_vr))

  #per dimension of flow
flow_data <- read.csv("flow_data.csv") #loading the flow data

data["challenge_score"]<- rowMeans(flow_data[c(2,10,18,26)], na.rm = TRUE) #adding the mean of every dimension to the data frame
data["goals_score"]<- rowMeans(flow_data[c(3,11,19,27)], na.rm = TRUE)
data["feedback_score"]<- rowMeans(flow_data[c(4,12,20,28)], na.rm = TRUE)
data["concentration_score"]<- rowMeans(flow_data[c(5,13,21,29)], na.rm = TRUE)
data["control_score"]<- rowMeans(flow_data[c(6,14,22,30)], na.rm = TRUE)
data["self_consciousness_score"]<- rowMeans(flow_data[c(7,15,23,31)], na.rm = TRUE)
data["time_score"]<- rowMeans(flow_data[c(8,16,24,32)], na.rm = TRUE)
data["autotelic_score"]<- rowMeans(flow_data[c(9,17,25,33)], na.rm = TRUE)

challenge_cor <- data.frame(data[c(5,7)])
cor(na.omit(challenge_cor))

goals_cor <- data.frame(data[c(5,8)])
cor(na.omit(goals_cor))

feedback_cor <- data.frame(data[c(5,9)])
cor(na.omit(feedback_cor))

concentration_cor <- data.frame(data[c(5,10)])
cor(na.omit(concentration_cor))

control_cor <- data.frame(data[c(5,11)])
cor(na.omit(control_cor))

self_consciousness_cor <- data.frame(data[c(5,12)])
cor(na.omit(self_consciousness_cor))

time_cor <- data.frame(data[c(5,13)])
cor(na.omit(time_cor))

autotelic_cor <- data.frame(data[c(5,14)])
cor(na.omit(autotelic_cor))

#Making the full model
#making the interaction variable
data["condtion_sa"] <- data$condition*data$sa_score 
data$condtion_sa <- scale(data$condtion_sa, center= TRUE, scale = TRUE)

#Before making the new regression model, we'll center the data and generate z-scores by scaling the data
data$flow_score <- scale(data$flow_score, center = TRUE, scale = TRUE)
data$sa_score <- scale(data$sa_score, center = TRUE, scale = TRUE)

#Using backward selection:
#Delete row 15, since Flow score is missing.
data <- data[-c(15), ]

#Making the full model with z-scores
full.model <- lm(understanding_score ~ condition.f + sa_score + flow_score + condtion_sa, data = data)
summary(full.model) 

#stepwise regression
step.model <- stepAIC(full.model, direction = "backward", trace = TRUE)
summary(step.model)

#Now we'll run the regression again, but without the participants that had knowledge about PCA in advance. Which are participant 26 and 28.
data <- data[-c(26,28)]

full.model2 <- lm(understanding_score ~ condition.f + sa_score + flow_score + condtion_sa, data = data)
summary(full.model2) 

step.model2 <- stepAIC(full.model2, direction = "backward", trace = TRUE)

# Trying the regression with only challenge-skill dimension, since this is the only one with decent correlation with understandin score
full.model3 <- lm(understanding_score ~ condition.f + sa_score + challenge_score + condtion_sa, data = data)
summary(full.model3) 

step.model3 <- stepAIC(full.model3, direction = "backward", trace = TRUE)
summary(step.model3)


