rm(list=ls())
library(tidyverse)
#install.packages('fifer')
library(fifer)

bank.data <- read.csv("C:\\Users\\madha\\OneDrive\\Documents\\UC-BANA\\Spring Sem 2018\\Data Mining I\\bankruptcy.csv")
samp<-floor(0.8 * nrow(bank.data))
colnames(bank.data)
str(bank.data)
summary(bank.data) #mean bankruptcy 14%

bank.years<-bank.data %>% 
  group_by(FYEAR) %>% 
  summarise(amt = sum(DLRSN))
par(mar=c(1,1,1,1))
hist(bank.years$amt,axes=T, xlab = 'Years', ylab = 'bankruptcies')
index.train <- sample(seq_len(nrow(bank.data)), size = samp)
bank.train <- bank.data[-index.train, ]
bank.test <- bank.data[index.train, ]
#bank.train <- stratified(df = bank.data, group = "DLRSN", size = .8)
#bank.test <-  bank.data[- as.numeric(row.names(bank.train)),]

#1 GLM
bank.glm0 <- glm(DLRSN ~ . - CUSIP -FYEAR, family = binomial, bank.train)
bank.glm.bic<- step(bank.glm0, k = log(nrow(bank.train)))
summary(bank.glm.bic)
AIC(bank.glm.bic) #2410.6
BIC(bank.glm.bic) #2468.04

#insample
prob.glm1b.insample <- predict(bank.glm.bic, type = "response")
predicted.glm1b.insample <- prob.glm1b.insample > 0.06
predicted.glm1b.insample <- as.numeric(predicted.glm1b.insample)
table(bank.train$DLRSN, predicted.glm1b.insample, dnn = c("Actual", "Predicted"))
mean(ifelse(bank.train$DLRSN != predicted.glm1b.insample, 1, 0)) #AMR

#outsample
prob.glm0.outsample <- predict(bank.glm.bic, newdata =bank.test, type = "response")
predicted.glm0.outsample <- prob.glm0.outsample > 0.06
predicted.glm0.outsample <- as.numeric(predicted.glm0.outsample)
table(bank.test$DLRSN, predicted.glm0.outsample, dnn = c("Truth", "Predicted"))
mean(ifelse(bank.test$DLRSN != predicted.glm0.outsample, 1, 0)) #AMR

#ROC curve
library("verification")
par(mar=c(1,1,1,1))
rocplot=roc.plot(bank.test$DLRSN == "1", prob.glm0.outsample)
rocplot$roc.vol

#2 CART
library(rpart)
bank.rpart <- rpart(formula = DLRSN ~ . - CUSIP -FYEAR, data = bank.train, method = "class",
                    parms = list(loss = matrix(c(0, 15, 1, 0), nrow = 2)))
plot(bank.rpart)
text(bank.rpart)
plotcp(bank.rpart)
bank.rpart2 <- prune(bank.rpart,cp=0.011)
#insample
bank.train.rpart = predict(bank.rpart2, bank.train, type = "prob")
#pred.train = prediction(bank.train.rpart[, 2], bank.train$DLRSN)
bank.train.pred.rpart2 = as.numeric(bank.train.rpart[, 2] > 0.06)
table(bank.train$DLRSN, bank.train.pred.rpart2, dnn = c("Truth", "Predicted"))
mean(ifelse(bank.train$DLRSN != bank.train.pred.rpart2, 1, 0)) #AMR

par(mar=c(1,1,1,1))
roc.plot(bank.train$DLRSN == "1", bank.train.pred.rpart2)

#outofsample
library(ROCR)
library(verification)
bank.data$DLRSN=as.list(bank.data$DLRSN)
bank.test.rpart = predict(bank.rpart2, bank.test, type = "prob")
#pred = prediction(bank.test.rpart[, 2], bank.test$DLRSN)
bank.test.pred.rpart2 = as.numeric(bank.test.rpart[, 2] > 0.06)
table(bank.test$DLRSN, bank.test.pred.rpart2, dnn = c("Truth", "Predicted"))
mean(ifelse(bank.test$DLRSN != bank.test.pred.rpart2, 1, 0)) #AMR

roc.plot(bank.test$DLRSN == "1", bank.test.pred.rpart2)
roc.plot(bank.test$DLRSN == "1", bank.test.pred.rpart2)$roc.vol

#3 GAM -----------------------------*************
library(mgcv)
str(bank.data)
bank.gam <-gam(DLRSN~s(R1)+s(R2)+s(R3)+s(R4)+s(R5)+s(R6)+s(R7)+s(R8)+s(R9)+s(R10),family = binomial, data=bank.train, select = TRUE)
par(mar=c(1,1,1,1))
plot(bank.gam, shade = TRUE, seWithMean = TRUE, scale = 0, pages=1)
AIC(bank.gam)
BIC(bank.gam)
bank.gam$deviance
bank.gam2 <-gam(DLRSN~R1+s(R2)+s(R3)+R4+R5+s(R6)+R7+s(R8)+s(R9)+s(R10),family = binomial, data=bank.train, select = TRUE)
plot(bank.gam2, shade = TRUE, seWithMean = TRUE, scale = 0, pages=1)

#In-sample
pcut.gam <- 0.06
prob.gam.in <- predict(bank.gam2, bank.train, type="response")
pred.gam.in <- (prob.gam.in >= pcut.gam) * 1
table(bank.train$DLRSN, pred.gam.in, dnn = c("Obs.", "Prediction"))
mean(ifelse(bank.train$DLRSN != pred.gam.in, 1, 0))

#Out-of-sample
prob.gam.out <- predict(bank.gam, bank.test, type ="response")
pred.gam.out <- (prob.gam.out >= pcut.gam) * 1
table(bank.test$DLRSN, pred.gam.out, dnn = c("Obs.", "Prediction"))
mean(ifelse(bank.test$DLRSN != pred.gam.out, 1, 0))

roc.plot(bank.test$DLRSN == "1", pred.gam.out)


#4 LDA- normality test
par(mar=c(1,1,1,1))
par(mfrow=c(1,1))
qqnorm(bank.data$R1)
qqnorm(bank.data$R2)
qqnorm(bank.data$R3)
qqnorm(bank.data$R4)
qqnorm(bank.data$R5)
qqnorm(bank.data$R6)
qqnorm(bank.data$R7)
qqnorm(bank.data$R8)
qqnorm(bank.data$R9)
qqnorm(bank.data$R10)

#quadratic
bank.qda <- qda(DLRSN ~ . -CUSIP -FYEAR, data = bank.train)
prob.qda.in <- predict(bank.qda, data = bank.train)
pcut.qda <- 0.06
pred.qda.in <- (prob.qda.in$posterior[, 2] >= pcut.lda) * 1
table(bank.train$DLRSN, pred.qda.in, dnn = c("Obs", "Pred"))
mean(ifelse(bank.train$DLRSN != pred.qda.in, 1, 0))
#outofsample
qda.out <- predict(bank.qda, newdata = bank.test)
cut.lda <- 0.06
pred.qda.out <- as.numeric((qda.out$posterior[, 2] >= cut.lda))
table(bank.test$DLRSN, pred.qda.out, dnn = c("Obs", "Pred"))
mean(ifelse(bank.test$DLRSN != pred.qda.out, 1, 0))
roc.plot(bank.test$DLRSN == "1", pred.qda.out)
roc.plot(bank.train$DLRSN == "1", pred.qda.in)
summary(bank.gam)


#4 NNet
library(nnet)
library(neuralnet)
bank.nnet <- nnet(DLRSN ~ . -CUSIP -FYEAR, data = bank.train, size = 1, maxit = 500, type='class')
prob.nnet = predict(bank.nnet, bank.test)
pred.nnet = as.numeric(prob.nnet > 0.06)
table(bank.test$DLRSN, pred.nnet, dnn = c("Observation", "Prediction"))
mean(ifelse(bank.test$DLRSN != pred.nnet, 1, 0))

n=8
bank.nnet.tune <- nnet(DLRSN ~ . -CUSIP -FYEAR, 
                   size = n, data = bank.train, maxit=1000, decay=0.006, linout = TRUE)   
prob.nnet.tune.train = predict(bank.nnet.tune, bank.train)
pred.nnet.tune.train = as.numeric(prob.nnet.tune.train > 0.06)
table(bank.test$DLRSN, pred.nnet.tune.train, dnn = c("Observation", "Prediction"))
mean(ifelse(bank.train$DLRSN != pred.nnet.tune.train, 1, 0))

#outofsample
prob.nnet.tune = predict(bank.nnet.tune, bank.test)
pred.nnet.tune = as.numeric(prob.nnet.tune > 0.06)
table(bank.test$DLRSN, pred.nnet.tune, dnn = c("Observation", "Prediction"))
mean(ifelse(bank.test$DLRSN != pred.nnet.tune, 1, 0))

bank.data$DLRSN=as.factor(bank.data$DLRSN)
roc.plot(bank.test$DLRSN == "1", pred.nnet.tune)

nn <- neuralnet(DLRSN ~ R1+R2+R3+R4+R5+R6+R7+R8+R9+R10,
                data=bank.train,hidden=c(8,1),linear.output=T) 
plot(nn)
