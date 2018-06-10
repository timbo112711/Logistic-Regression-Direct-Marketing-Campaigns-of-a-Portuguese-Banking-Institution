#Load Packages 
library(glmnet)
library(ROCR)
library(MASS)

#Set wroking directory
setwd("C:\\Users\\TMccw\\OneDrive\\DataScience\\Applied Statistics\\Project3\\Data")

#Bring in dataset
bank <- read.table("bank-full2.csv", header = TRUE, sep = ',')

#Check dataset
head(bank)
dim(bank)
str(bank)
summary(bank)
        
##glmnet

# Split data into "training" (90%) and "test" (10%)
ind <- sample(2, nrow(bank), replace = TRUE, prob = c(0.9, 0.1))  #split data into "training" and "test" samples       
train.bank <- bank[ind==1,]                                       #populate "training" set with 80% of data ramdonly selected 
test.bank <- bank[ind==2,]  

# Take response out and convert to factor
train.bank.y <- train.bank$y
train.bank.y <- as.factor(as.character(train.bank.y))

train.bank.y <- train.bank$y # make set with just response variable
bank.train.y <- as.factor(as.character(bank.train.y)) # convert response to 

# glmnet requires a matrix
train.bank <- as.matrix(train.bank)

cvfit <- cv.glmnet(train.bank, train.bank.y, type.measure = "class", nlambda = 1000)
plot(cvfit)
coef(cvfit, s = "lambda.min")

#Get training set predictions...We know they are biased but lets create ROC's.
#These are predicted probabilities from logistic model  exp(b)/(1+exp(b))
fit.pred <- predict(cvfit, newx = dat.train.x, type = "response")


#Compare the prediction to the real outcome
head(fit.pred)
head(dat.train.y)

#Create ROC curves
pred <- prediction(fit.pred[,1], dat.train.y)
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values

#Plot ROC
plot(roc.perf)
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))


#Get Validation Set I
dat.val1 <- dat[whichroc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
                (dat$Set == "Validation I"),]
dat.val1.x <- dat.val1[,c(6:ncol(dat))]
dat.val1.x <- as.matrix(dat.val1.x)

dat.val1.y <- dat.val1$Censor
dat.val1.y <- as.factor(as.character(dat.val1.y))


#Run model from training set on valid set I
fit.pred1 <- predict(cvfit, newx = dat.val1.x, type = "response")

#ROC curves
pred1 <- prediction(fit.pred1[,1], dat.val1.y)
roc.perf1 = performance(pred1, measure = "tpr", x.measure = "fpr")
auc.val1 <- performance(pred1, measure = "auc")
auc.val1 <- auc.val1@y.values
plot(roc.perf1)
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.val1[[1]],3), sep = ""))


#Rinse and Repeat for valid set II and III
dat.val2 <- dat[which(dat$Set == "Validation II"),]
dat.val2.x <- dat.val2[,c(6:ncol(dat))]
dat.val2.x <- as.matrix(dat.val2.x)

dat.val2.y <- dat.val2$Censor
dat.val2.y <- as.factor(as.character(dat.val2.y))

fit.pred2 <- predict(cvfit, newx = dat.val2.x, type = "response")

pred2 <- prediction(fit.pred2[,1], dat.val2.y)
roc.perf2 = performance(pred2, measure = "tpr", x.measure = "fpr")
auc.val2 <- performance(pred2, measure = "auc")
auc.val2 <- auc.val2@y.values
plot(roc.perf2)
abline(a=0, b= 1)
text(x = .42, y = .6,paste("AUC = ", round(auc.val2[[1]],3), sep = ""))

#Valid set III
dat.val3 <- dat[which(dat$Set == "Validation III"),]
dat.val3.x <- dat.val3[,c(6:ncol(dat))]
dat.val3.x <- as.matrix(dat.val3.x)

dat.val3.y <- dat.val3$Censor
dat.val3.y <- as.factor(as.character(dat.val3.y))


fit.pred3 <- predict(cvfit, newx = dat.val3.x, type = "response")

pred3 <- prediction(fit.pred3[,1], dat.val3.y)
roc.perf3 = performance(pred3, measure = "tpr", x.measure = "fpr")
auc.val3 <- performance(pred3, measure = "auc")
auc.val3 <- auc.val3@y.values
plot(roc.perf3)
abline(a=0, b= 1)
text(x = .4, y = .6,paste("AUC = ", round(auc.val3[[1]],3), sep = ""))


## LDA

#Training Set
dat.train <- dat[which(dat$Set == "Training"),]
dat.train.x <- dat.train[,6:ncol(dat)]

dat.train.y <- dat.train$Censor
dat.train.y <- as.factor(as.character(dat.train.y))

fit.lda <- lda(dat.train.y ~ ., data = dat.train.x)
pred.lda <- predict(fit.lda, newdata = dat.train.x)

preds <- pred.lda$posterior
preds <- as.data.frame(preds)

pred <- prediction(preds[,2],dat.train.y)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf)
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))


#Valid set I
dat.val1 <- dat[which(dat$Set == "Validation I"),]
dat.val1.x <- dat.val1[,c(6:ncol(dat))]

dat.val1.y <- dat.val1$Censor
dat.val1.y <- as.factor(as.character(dat.val1.y))


pred.lda1 <- predict(fit.lda, newdata = dat.val1.x)

preds1 <- pred.lda1$posterior
preds1 <- as.data.frame(preds1)

pred1 <- prediction(preds1[,2],dat.val1.y)
roc.perf = performance(pred1, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred1, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf)
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))


#Valid set II
dat.val2 <- dat[which(dat$Set == "Validation II"),]
dat.val2.x <- dat.val2[,c(5:ncol(dat))]

dat.val2.y <- dat.val2$Censor
dat.val2.y <- as.factor(as.character(dat.val2.y))

pred.lda2 <- predict(fit.lda, newdata = dat.val2.x)

preds2 <- pred.lda2$posterior
preds2 <- as.data.frame(preds2)

pred2 <- prediction(preds2[,2],dat.val2.y)
roc.perf = performance(pred2, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred2, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf)
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))


#Valid set III
dat.val3 <- dat[which(dat$Set == "Validation III"),]
dat.val3.x <- dat.val3[,c(5:ncol(dat))]

dat.val3.y <- dat.val3$Censor
dat.val3.y <- as.factor(as.character(dat.val3.y))


pred.lda3 <- predict(fit.lda, newdata = dat.val3.x)

preds3 <- pred.lda3$posterior
preds3 <- as.data.frame(preds3)

pred3 <- prediction(preds3[,2],dat.val3.y)
roc.perf = performance(pred3, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred3, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf)
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))





#Suppose we did not have all of these validation data sets.  We can assess how well our model building process works through Cross validation.
#The idea is that we can get an idea of how well the approach is going to perform on new data not yet collected.
#We will use AUC as the performance matrix.

nloops<-50   #number of CV loops
ntrains<-dim(dat.train.x)[1]  #No. of samples in training data set
cv.aucs<-c()

for (i in 1:nloops){
  index<-sample(1:ntrains,60)
  cvtrain.x<-as.matrix(dat.train.x[index,])
  cvtest.x<-as.matrix(dat.train.x[-index,])
  cvtrain.y<-dat.train.y[index]
  cvtest.y<-dat.train.y[-index]
  
  cvfit <- cv.glmnet(cvtrain.x, cvtrain.y, family = "binomial", type.measure = "class") 
  fit.pred <- predict(cvfit, newx = cvtest.x, type = "response")
  pred <- prediction(fit.pred[,1], cvtest.y)
  roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
  auc.train <- performance(pred, measure = "auc")
  auc.train <- auc.train@y.values
  
  cv.aucs[i]<-auc.train[[1]]
}

hist(cv.aucs)
summary(cv.aucs)


#Doing the same procedure for random allocation of response values.
#Good practice when number of yes/no is not balanced.


nloops<-50   #number of CV loops
ntrains<-dim(dat.train.x)[1]  #No. of samples in training data set
cv.aucs<-c()
dat.train.yshuf<-dat.train.y[sample(1:length(dat.train.y))]

for (i in 1:nloops){
  index<-sample(1:ntrains,60)
  cvtrain.x<-as.matrix(dat.train.x[index,])
  cvtest.x<-as.matrix(dat.train.x[-index,])
  cvtrain.y<-dat.train.yshuf[index]
  cvtest.y<-dat.train.yshuf[-index]
  
  cvfit <- cv.glmnet(cvtrain.x, cvtrain.y, family = "binomial", type.measure = "class") 
  fit.pred <- predict(cvfit, newx = cvtest.x, type = "response")
  pred <- prediction(fit.pred[,1], cvtest.y)
  roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
  auc.train <- performance(pred, measure = "auc")
  auc.train <- auc.train@y.values
  
  cv.aucs[i]<-auc.train[[1]]
}

hist(cv.aucs)
summary(cv.aucs)

