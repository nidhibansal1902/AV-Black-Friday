
#Loading libraries
library(caret)
library(caTools)
library(dplyr)
library(ggplot2)
library(randomForest)
library(e1071)
library(xgboost)
library(dummies)

#Load data
setwd("Black Friday")
train_bf<-read.csv("train.csv")
test_bf<-read.csv("test.csv")
test_bf$Purchase<-"NA"

total_bf<- rbind(train_bf,test_bf)

#setting variables data type
str(total_bf)
total_bf$Product_ID<-as.integer(total_bf$Product_ID)
levels(train_bf$Age)

total_bf$Marital_Status<-as.factor(total_bf$Marital_Status)
total_bf$Purchase<-as.numeric(total_bf$Purchase)


#Missing Values
colSums(total_bf=="")
colSums(is.na(total_bf))

total_bf$Product_Category_2<- ifelse(is.na(total_bf$Product_Category_2),
                                           ave(total_bf$Product_Category_2, FUN=function(x) mean(x, na.rm=TRUE)), 
                                           total_bf$Product_Category_2)

total_bf$Product_Category_3<- ifelse(is.na(total_bf$Product_Category_3),
                                     ave(total_bf$Product_Category_3, FUN=function(x) mean(x, na.rm=TRUE)), 
                                     total_bf$Product_Category_3)


ggplot()+
  geom_bar(data=total_bf[1:550068,],aes(x=Product_Category_1,fill=factor(Age)))


ggplot()+
  geom_bar(data=total_bf[1:550068,],aes(x=Product_Category_2,fill=factor(Marital_Status)))



#Feature engineering
total_bf$Gender<-ifelse(total_bf$Gender== "F",1,0)
total_bf<-dummy.data.frame(total_bf, names=c("City_Category"), sep="_")

total_bf$Stay_In_Current_City_Years<- as.character(total_bf$Stay_In_Current_City_Years)
total_bf$Stay_In_Current_City_Years[total_bf$Stay_In_Current_City_Years=="4+"] <- 4
total_bf$Stay_In_Current_City_Years<- as.integer(total_bf$Stay_In_Current_City_Years)


total_bf$Age<-as.character(total_bf$Age)
total_bf$Age[total_bf$Age=="0-17"]<-15
total_bf$Age[total_bf$Age=="18-25"]<-21
total_bf$Age[total_bf$Age=="26-35"]<-30
total_bf$Age[total_bf$Age=="36-45"]<-40
total_bf$Age[total_bf$Age=="46-50"]<-48
total_bf$Age[total_bf$Age=="51-55"]<-53
total_bf$Age[total_bf$Age=="55+"]<-60
total_bf$Age<-as.integer(total_bf$Age)

#Using knn for finding missing values
library(class)

data2<-total_bf[,3:10]
data2_yes<-data2[!is.na(data2$Product_Category_2),]
data2_no<-data2[is.na(data2$Product_Category_2),]

classf1 <- knn(train = data2_yes[, -6],
                   test = data2_no[, -6],
                   cl = data2_yes[, 6],
                   k = 10,
                   prob = TRUE)
# does not work


#Feature scaling
ggplot(total_bf[1:550068,],aes(x=factor(Gender), y=Purchase))+
  geom_point(aes(y=Purchase),colour='red')

ggplot(total_bf[1:550068,],aes(x=factor(Age), y=Purchase))+
  geom_point(aes(y=Purchase),colour='red')

ggplot(total_bf[1:550068,],aes(x=factor(City_Category), y=Purchase))+
  geom_point(aes(y=Purchase),colour='red')

ggplot(total_bf[1:550068,],aes(x=factor(Stay_In_Current_City_Years), y=Purchase))+
  geom_point(aes(y=Purchase),colour='red')

ggplot(total_bf[1:550068,],aes(x=factor(Marital_Status), y=Purchase))+
  geom_point(aes(y=Purchase),colour='red')

ggplot(total_bf[1:550068,],aes(x=Product_Category_1, y=Purchase))+
  geom_point(aes(y=Purchase),colour='red')


aov(total_bf[1:550068,]$Purchase ~ total_bf[1:550068,]$Gender + total_bf[1:550068,]$Age +total_bf[1:550068,]$Occupation )

#Predictive Modeling

#regression
data_bf<-total_bf[,3:12]

reg<- lm(data_bf[1:550068,]$Purchase ~ . , data=data_bf[1:550068,])

anova(reg, test = 'Chisq')

train_predict<- predict(reg,data_bf[1:550068,-10] )
test_predict<- predict(reg,data_bf[550069:783667,-10] )


ggplot() +
  geom_point(aes(x = data_bf[1:550068,9], y = data_bf[1:550068,10]),
             colour = 'yellow') +
  geom_line(aes(x = data_bf[1:550068,9], y = train_predict),
            colour = 'blue') 


ggplot() +
  geom_point(aes(x = data_bf[1:550068,3], y = data_bf[1:550068,10]),
             colour = 'red') +
  geom_line(aes(x = data_bf[550069:783667,3], y = test_predict),
            colour = 'blue') 


#Random forest
  Predict_rf<-randomForest(formula=data_bf[1:55006,]$Purchase ~. , data=data_bf[1:55006,],
                           nTree=500)
y_pred_rf<-predict(Predict_rf,data_bf[550069:783667,-10] )


#Naive Bayes

model <- naiveBayes(Purchase~., data = data_bf[1:5500,])
class(model) 
pred <- predict(model,data_bf[5501:11001,-10])
table(data_bf[5501:11001,10],pred)



#XGboost
data_bf<-total_bf[,3:14]
model_xgb1 <- xgboost(data.matrix(data_bf[1:550068,-12]),data.matrix(data_bf[1:550068,12]),cv=5,objective="reg:linear",nrounds=500,max.depth=10,eta=0.1,colsample_bytree=0.5,seed=235,metric="rmse",importance=1)

test_xgb1 <- predict(model_xgb1,data.matrix(data_bf[550069:783667,-12]))
test_bf$Purchase<-test_xgb1


solution <- data.frame('User_ID' = test_bf$User_ID, 'Product_ID'=test_bf$Product_ID,'Purchase'=test_bf$Purchase)

head(solution)

# Write it to file
write.csv(solution, 'D:/Data Science/Analytics Vidya/Practice_projects/Black Friday/Submission.csv', row.names = F)


ggplot() +
  geom_point(aes(x = train_bf[,3], y = train_bf$Purchase),
             colour = 'yellow') +
  geom_line(aes(x = test_bf[,3], y = test_bf$Purchase),
            colour = 'blue') 