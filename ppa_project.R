library(dplyr)
library(ggplot2)
library(dplyr)
library(rpart)
library(rattle)
library(rpart.plot)
library(e1071)
library(caTools)
library(caret)
library(dplyr)
library(ROCR)
library(dummies)

data<- read.csv("bigml_59c28831336c6604c800002a.csv")
summary(data)


data_1 <- data %>% select(-c(state,area.code,phone.number)) 


summary(data_1)

apply(data_1,MARGIN = 2, FUN = function(x) length(which(is.na(x))))
apply(data,MARGIN = 2, FUN = function(x) length(which(x == "")))


data_1$international.plan<- ifelse(data_1$international.plan=='yes',1,0)
data_1$voice.mail.plan<- ifelse(data_1$voice.mail.plan=='yes',1,0)
data_1$churn<- ifelse(data_1$churn=='False',0,1)
summary(data_1)

data_2<-data_1%>%select(-c(churn))

data_3<-scale(data_2)
data_4<-cbind(data_3,churn=data_1$churn)

data_3$churn <-data_2$churn
summary(data_4)

names(data_4)[names(data_4) == 'V18'] <- 'churn'

View(data_4)
data_5<-sample.split(data_4["churn"],SplitRatio = 0.7)
training_data <- subset(data_4,split == "TRUE")
testing_data <- subset(data_4,split == "FALSE")
dim(training_data)
dim(testing_data)
View(training_data)

data_6<-as.data.frame(data_4)

model1 <- glm(churn~international.plan+voice.mail.plan+total.intl.calls+customer.service.calls+number.vmail.messages, data = data_6, family = binomial)

summary(model1)
testing_data<-as.data.frame(testing_data)
res <- predict(model1,testing_data,type="response")
head(res)
table(ActualValues = testing_data$churn,PredictedValues = res > 0.8)
table(ActualValues = testing_data$churn,PredictedValues = res > 0.7)
View(testing_data)
#ROCR

ROCRpred <- prediction(res,data_6$churn)
ROCRpref <- performance(ROCRpred,"tpr","fpr")
plot(ROCRpref,colorize=TRUE)
install.packages('ROCR')


model <- rpart(churn~international.plan+voice.mail.plan+total.intl.calls+customer.service.calls+number.vmail.messages,data=data_6,method="class",parms=list(split="gini"))

pred_tree_gini<- predict(model,testing_data,type="class")

fancyRpartPlot(model)

table(Actualvalue =testing_data$churn, prediction = pred_tree_gini)


res = predict(model,testing_data,type="response")
table(Actualvalue =testing_data$churn,prediction = res >0.2)



