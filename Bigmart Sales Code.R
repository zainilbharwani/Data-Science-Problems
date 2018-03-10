train<-read.csv('Train_UWu5bXk.csv')
str(train)
attach(train)
summary(train)

train$Outlet_Establishment_Year<-as.factor(train$Outlet_Establishment_Year)
str(train)

train[train=='']<-NA
sum(is.na(train))
colSums(is.na(train))

library(dplyr)
filtered_df<-filter(train[,1:2], train$Item_Identifier=='DRA24')
library(DMwR)
filtered_df<-centralImputation(filtered_df)

filtered_df<-data.frame()
for(i in levels(train$Item_Identifier)){
  imp_df<-filter(train[,1:2], train$Item_Identifier==i)
  imp_df<-centralImputation(imp_df)
  filtered_df<-rbind(filtered_df, imp_df)
}

train<-train[order(train$Item_Identifier),]

##Final data frame
mydata<-cbind(filtered_df, train[,3:12])
sum(is.na(mydata))
colSums(is.na(mydata))

plot(x=mydata$Outlet_Type, y=mydata$Outlet_Size)

indices<-which(mydata$Outlet_Type=='Grocery Store')
mydata$Outlet_Size[indices]<-'Small'
sum(is.na(mydata))
colSums(is.na(mydata))

sum(is.na(mydata[which(mydata$Outlet_Type=='Grocery Store'),'Outlet_Size']))
sum(is.na(mydata[which(mydata$Outlet_Type=='Supermarket Type1'),'Outlet_Size']))

str(mydata)

sum(is.na(mydata))
str(mydata)
summary(mydata$Outlet_Size)

library(missForest)
miss_imp<-missForest(mydata[,2:12], maxiter = 2)
missforest_data<-data.frame(Item_Identifier = mydata$Item_Identifier)
missforest_data<-cbind(missforest_data,miss_imp$ximp)

plot(x=missforest_data$Outlet_Type, y=missforest_data$Outlet_Size)

sum(is.na(missforest_data))
str(missforest_data)

missforest_data$Item_Identifier<-NULL
levels(missforest_data$Outlet_Size)[1]<-'Small'

####Model Building####
library(caret)
trainrows<-createDataPartition(missforest_data$Item_Outlet_Sales, p=0.8, list = F)
traindata<-missforest_data[trainrows,]
valdata<-missforest_data[-trainrows,]

model_lm<-lm(Item_Outlet_Sales~., data = traindata)
summary(model_lm)

output_lm_train<-predict(model_lm, newdata = traindata)
output_lm_val<-predict(model_lm, newdata = valdata)

library(DMwR)
regr.eval(traindata$Item_Outlet_Sales, output_lm_train)
regr.eval(valdata$Item_Outlet_Sales, output_lm_val)

library(rpart)
library(rpart.plot)
model_rpart<-rpart(Item_Outlet_Sales~., data = traindata)
model_rpart

rpart.plot(model_rpart, cex = 0.6)
plotcp(model_rpart)

output_rpart_train<-predict(model_rpart, newdata = traindata)
output_rpart_val<-predict(model_rpart, newdata = valdata)

regr.eval(traindata$Item_Outlet_Sales, output_rpart_train)
regr.eval(valdata$Item_Outlet_Sales, output_rpart_val)

library(randomForest)
model_randomforest<-randomForest(Item_Outlet_Sales~., data = traindata, ntree = 50)
model_randomforest
plot(model_randomforest)

output_randomforest_train<-predict(model_randomforest, newdata = traindata)
output_randomforest_val<-predict(model_randomforest, newdata = valdata)

regr.eval(traindata$Item_Outlet_Sales, output_randomforest_train)
regr.eval(valdata$Item_Outlet_Sales, output_randomforest_val)

library(gbm)
model_gbm<-gbm(Item_Outlet_Sales~., data = traindata, n.trees = 8000)
model_gbm

output_gbm_train<-predict(model_gbm, newdata = traindata, n.trees = 8000)
output_gbm_val<-predict(model_gbm, newdata = valdata, n.trees = 8000)

regr.eval(traindata$Item_Outlet_Sales, output_gbm_train)
regr.eval(valdata$Item_Outlet_Sales, output_gbm_val)
