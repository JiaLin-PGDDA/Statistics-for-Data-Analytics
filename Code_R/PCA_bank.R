# read csv file:
# 1. get user's home directory
home = setwd(Sys.getenv("HOME"));
home
# 2. construct path to file
fpath = file.path(home, "Desktop", "SAP_CA2_040123/datasets/Bank.csv")
# read file without header
df_bank <- read.csv(file=fpath, header = TRUE)
head(df_bank)

unique(df_bank$age)
unique(df_bank$job)
unique(df_bank$marital)
unique(df_bank$education)
unique(df_bank$contact)
unique(df_bank$month)
unique(df_bank$poutcome)

df_bank$default <- ifelse(df_bank$default=="yes",1,0)
df_bank$housing <- ifelse(df_bank$housing=="yes",1,0)
df_bank$loan    <- ifelse(df_bank$loan=="yes",1,0)

head(df_bank)

bank <- (df_bank[,-c(1,17)])
# Select Numeric Columns
library("dplyr")
numericItems <- select_if(bank, is.numeric)
numericItems
# PCA (Principal Component Analysis)
library(psych)
library(haven)
#Scree plots of data or correlation matrix 
#compared to random ``parallel" matrices
fa.parallel(numericItems,fa="pc",n.iter=100)

pc.numericItems<-principal(numericItems,nfactors=3,rotate="none")
rc.numericItems<-principal(numericItems,nfactors=3,rotate="varimax")
fa.diagram(rc.numericItems)
# Keep: previous, campaign, balance
# Delete: pdays, day, duration, loan, default, housing
rc.numericItems$score
NC<-pc.numericItems$score # NC stands for new components

df_bank["pre_compaign"] <-NC[,1]
df_bank["dur_compaign"] <-NC[,2]
df_bank["finance"]      <-NC[,3]

head(df_bank)

df_bank_pca <-select(df_bank, c(age, 
                                 job, 
                                 marital, 
                                 education, 
                                 contact, 
                                 month, 
                                 poutcome, 
                                 finance, 
                                 pre_compaign, 
                                 dur_compaign,
                                 y))
head(df_bank_pca)

df_bank_pca$y       <- ifelse(df_bank_pca$y=="yes",1,0)
head(df_bank_pca)

library(caret)
Train <- createDataPartition(df_bank_pca$y, p=0.8, list=FALSE)
training <- df_bank_pca[Train,]
test <- df_bank_pca[-Train,]
dim(training)
dim(test)

#Logistic Regression
#Fitting the model
bankfit<-glm(y~age+job+marital+education+contact+month+poutcome
             +finance+pre_compaign+dur_compaign
             ,data =training, family=binomial)

#Statistical Tests for Individual Predictors
#Variable Importance
varImp(bankfit)
#Compare different models
bankfit1<-glm(y~job+marital+education # age
              +contact
              +month
              +poutcome
              +finance
              +pre_compaign
              +dur_compaign
              ,data =training, family=binomial)
bankfit2<-glm(y~job+education # marital
              +contact
              +month
              +poutcome
              +finance
              +pre_compaign
              +dur_compaign
              ,data =training, family=binomial)
bankfit3<-glm(y~job #education
              +contact
              +month
              +poutcome
              +finance
              +pre_compaign
              +dur_compaign
              ,data =training, family=binomial)
bankfit4<-glm(y~#job
               contact
              +month
              +poutcome
              +finance
              +pre_compaign
              +dur_compaign
              ,data =training, family=binomial)
#Goodness of Fit
#Pseudo R^2
#install.packages("pscl")
library(pscl)
pR2(bankfit)
pR2(bankfit1)#age
pR2(bankfit2)#marital
pR2(bankfit3)#education
pR2(bankfit4)#job

# Comparing models with ANOVA
anova(bankfit, bankfit1, test ="Chisq")
anova(bankfit, bankfit2, test ="Chisq")
anova(bankfit, bankfit3, test ="Chisq")
anova(bankfit, bankfit4, test ="Chisq")
# Delete age
# Use bankfit1:
# IV: marital, education, job, month, contact, balance, campaign, previous, poutcome
summary(bankfit1)

#Odds Ratios
coef(bankfit1)
exp(coef(bankfit1))

#Examine predicted probabilities and classifications
pred_probs<-predict(bankfit1, type="response")
pred_probs
pred_class<-ifelse(pred_probs<0.5, "0","1")
pred_class

#confusionMatrix function in caret package
library(e1071)
bought<-as.factor(training$y)
pred_classf<-as.factor(pred_class)
confusionMatrix(bought,pred_classf,positive = "1")

# Another way to create confusion matrix
# Use Table function to create confusion matrix
table(training$y,pred_class)# accuracy of the model
names(bankfit1)

###############################################################
#Cross Validation
#Apply bankfit1 to test dataset
###############################################################
pred_test<-predict(bankfit1, test, type="response")
pred_class_test<-ifelse(pred_test<0.5, "0","1")
bought_test<-as.factor(test$y)
pred_classf_test<-as.factor(pred_class_test)
confusionMatrix(bought_test,pred_classf_test,positive = "1")

