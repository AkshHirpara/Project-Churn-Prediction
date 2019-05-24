#CHURN PROJECT

#Clear R Environment
rm(list = ls())


#Lets Load Important LIBRARIES
X = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", 
      "C50", "dummy",
      "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees',"mlr",
      "gridExtra","outliers","partitions","class","e1071")
library(caret)
library(scales)
library(psych)
library(gplots)
lapply(X, require, character.only = TRUE)
rm(X)


#Lets set the Directory
setwd("F:/Data Scientist/Project_Churn")
getwd()



#Lets Load our Data, We have got both Train and Test Data hence we will load both files and check out the data.
df=read.csv("Test_data.csv")
df1=read.csv("Train_data.csv")
Traindf1=df1
Testdf=df

#Lets Remove (.) and Replace with (_)
names(df)=gsub('\\.','_',names(df))
names(df1)= gsub('\\.','_',names(df1))

#Lets check the data first
head(df)
head(df1)

#Lets see the Structure of Data
str('df')
str('df1')

#Lets Start Data Preprocessing by checking out the format and changing the data format.
str(df1$state)
summary(df)
str(df)

df1$area_code=as.numeric(df1$area_code)
df$area_code=as.numeric(df$area_code)
df$phone_number=as.numeric(df$phone_number)
df1$phone_number=as.numeric(df1$phone_number)

str(df1$area_code)
str(df1$phone_number)
class(df1$phone_number)

# Lets check if there are any Missing Values on both Train and Test Data
sum(is.na(df1))
sum(is.na(df))
###############################################################################################
#Lets Perform Outliers Analysis
#As we know our data requires to be levelled so as to simplify our operations such as Outliers Analysis

#For Performing Outliers Analysis we need to peform similar operation
#On Train Data
#####################################################################################################
for(i in 1:ncol(df1)){
  
  if(class(df1[,i]) == 'factor'){
    
    df1[,i] = factor(df1[,i], labels=(1:length(levels(factor(df1[,i])))))
    
  }
}

#On Test Data

for(i in 1:ncol(df)){
  
  if(class(df[,i]) == 'factor'){
    
    df[,i] = factor(df[,i], labels=(1:length(levels(factor(df[,i])))))
    
  }
}

sum(is.na(df))

#Now for Outliers Analysis , We need to seperate Categorical and Continuous Variables
num_index=sapply(df1,is.numeric)
num_data=df1[,num_index]
num_col=colnames(num_data)

num_index1=sapply(df,is.numeric)
num_data1=df[,num_index1]
num_col1=colnames(num_data1)

#Now we can plot Box Plot
#First on Train Data
library('ggplot2')
library('gridExtra')  
  for(i in 1:length(num_col))
 {
 assign(paste0("gn",i),ggplot(aes_string(y= (num_col[i]),x='Churn'),data=subset(df1))+
      stat_boxplot(geom = "errorbar",width=0.5) +
      geom_boxplot(outlier.colour = "red",fill='grey',outlier.size=1,notch = FALSE)+
      theme(legend.position = "bottom")+
      labs(y=num_col[i],x='Churn Data')+
      ggtitle(paste("Box Plot for Churn of Train Data"),num_col[i]))  
    }

#Now we have to plot every Plots Together
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,gn8,gn9,ncol=3)
gridExtra::grid.arrange(gn10,gn11,gn12,ncol=3)
gridExtra::grid.arrange(gn13,gn14,gn15,ncol=3)

#For Test Data

for(i in length(num_col1))
 {
 assign(paste0('gn',i),ggplot(aes_string(y= (num_col1[i]),x='Churn'),data=subset(df))+
  stat_boxplot(geom = 'errorbar',width=1)+
  geom_boxplot(outlier.colour = "black",fill='red',outlier.size = 1,notch = FALSE)+
  theme(legend.position = "bottom")+
    labs(y=num_col1[i],x='Churn Data')+
  ggtitle(paste("Box Plot for Test Data"),num_col1[i]))
  }

#Now Lets Plot every point Together
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)  
gridExtra::grid.arrange(gn7,gn8,gn9,ncol=3)
gridExtra::grid.arrange(gn10,gn11,gn12,ncol=3)
gridExtra::grid.arrange(gn13,gn14,gn15,ncol=3)

sum(is.na(df1))
sum(is.na(df))

#Now Lets Remove Outliers Data, We wont do that because our data is already imbalanced
#From Train Data
#for (i in num_col){
 # print(i)
  #val=df1[,i][df1[,i] %in% boxplot.stats(df1[,i])$out]
  #print(length(val))
  #df1=df1[which(!df1[,i] %in% val),]
#}

sum(is.na(df1))

#From Test Data
#for(i in num_col1){
 # print(i)
  #val=df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  #print(length(val))
  #df=df[which(!df[,i] %in% val),]
#}

sum(is.na(df))

##############################################################################################
#Seperating categorical data
cat_index= sapply(df1, is.factor)   #train data
cat_col = df1[,cat_index]
colnames(cat_col)

cat_index01= sapply(df, is.factor)    #test data
cat_col01 = df[,cat_index01]
colnames(cat_col01)

#Now we will perform FEATURE SELECTION/VARIABLE SELECTION

#For Train Data, Continuous Variable
#Correlation Analysis_Train Data
library('corrgram')
library('knitr')
corrgram(df1[,num_col],order=F,
         upper.panel=panel.pie,text.panel=panel.txt,main='Correlation Plot_Train')

#For Test Data
corrgram(df[,numeric_testdata],order = F,
        upper.panel=panel.pie,text.panel=panel.txt,main='Correlation Plot_Test')

sum(is.na(df1))

#Chi Sqaure Test 

#For Train Data

for(i in 1:4)
{
  print(names(cat_col)[i])
  print(chisq.test(table(cat_col$Churn,cat_col[,i])))
}


#For Test Data

for(i in 1:4)
{
  print(names(cat_col01)[i])
  print(chisq.test(table(cat_col01$Churn,cat_col01[,i])))
}


############################################################################################

#Now We have to perform Dimension Reduction
#We can see chi sqaure value, we doonot need to remove any categorical variable
#But according to correlational analysis we need to remove variable named AREA CODE



#############################################################################################
#Lets do Feature Scaling/Engineering by Standardization

#For Train Data

for (i in num_col){
  print(i)
  df1[,i]=(df1[,i] -mean(df1[,i]))/sd(df1[,i])

}

#For Test Data

for(i in num_col1){
  print(i)
  df[,i]=(df[,i] -mean(df[,i]))/sd(df[,i])
}


#Now we will use TABLEAU for VISUALIZATIONS
#Attached is  the link of Tableau Public
#https://public.tableau.com/profile/akshay.hirapara#!/vizhome/Dashboard_Churn_2/Dashboard2
#https://public.tableau.com/profile/akshay.hirapara#!/vizhome/Dashboard_Churn/Dashboard1

####################################################################################################



#Lets Make final set of data
#For Train Data
Train_Data=subset(df1,select = -c(state,total_day_charge,total_eve_charge,total_night_charge,total_intl_charge,phone_number))

Test_Data=subset(df,select= -c(state,total_day_charge,total_eve_charge,total_night_charge,total_intl_charge,phone_number))

########################################################################################################################################33
#Lets go for Sampling

set.seed(101)
split=createDataPartition(Train_Data$Churn,p=0.6,list = FALSE)
traindata1=Train_Data[split,]
testdata1=Train_Data[-split,]

table(traindata$Churn)


#Remove Target Class Imbalance Problem
library('ROSE')

traindata1=ROSE(Churn~.,data=traindata1,p=0.4,seed=101)$data
table(traindata1$Churn)

#Test_Data_Sm=ROSE(Churn~.,data = Test_Data,p=0.3,seed = 101)$data
#table((Train_Data$Churn))


#############################################################################################
#Now our data is prepared for Model Building.So lets start building model!
###############################################################################################
#First Lets create a Function for Confusion Matrix
calculate_data <- function(cm){
  TN = cm[1,1]
  FP = cm[1,2]
  FN = cm[2,1]
  TP = cm[2,2]
  # #calculations
  print(paste0('Accuracy :- ',((TN+TP)/(TN+TP+FN+FP))*100))
  print(paste0('FNR :- ',((FN)/(TP+FN))*100))
  print(paste0('FPR :- ',((FP)/(TN+FP))*100))
  print(paste0('recall//TPR//sensitivity :-  ',((TP)/(TP+FN))*100))
  print(paste0('Specificity :-  ',((TN)/(TN+FP))*100))
  plot(cm)
}
##############################################################################################


#Lets Start with Random Forest
library('randomForest')
library('inTrees')
library('e1071')

options(max.print = 100000000)
#For TRAIN DATA

Rf_modelforTrainData=randomForest(Churn~.,Train_Data,importance=TRUE,ntree=500,type='class')
plot(Rf_modelforTrainData)
Rf_modelforTrainData
listoftrees=RF2List(Rf_modelforTrainData)

rules=extractRules(listoftrees,Train_Data[,-15])
rules[1:2,]

readrules=presentRules(rules,colnames(Train_Data))
readrules[1:2,]

#Lets Predict Test Values
rf_predictfortrain=predict(Rf_modelforTrainData,Test_Data[,-15])
colnames(Test_Data[,-15])
#rf_predictfortrain

#Lets build confusion matrix for Train Data
library('data.table')
Conf_rf=table(Test_Data$Churn,rf_predictfortrain)
confusionMatrix(Conf_rf)
calculate_data(Conf_rf)

#Accuracy=95.80%
#FNR=29.91%
#Recall=70.53

############################################################################################
#Logistic Regression

#For Train Data
logistic_model=glm(Churn~.,Train_Data,family = 'binomial')
summary(logistic_model)

#Lets Predict Test Data

logistic_prediction=predict(logistic_model,Test_Data[-15],type = 'response')
logistic_prediction=ifelse(logistic_prediction>0.5,2,1)
conf_logistic=table(Test_Data$Churn,logistic_prediction)
calculate_data(conf_logistic)
#Accuracy=87.16%
#FNR=80.35%
#Recall=19.64

#############################################################################################
#Let's build NAIVE BAYES MODEL

#For Train  Data
library('klaR')

naive_model=naiveBayes(Churn~.,data = Train_Data,type='Class')
naive_model
naive_predict=predict(naive_model,Test_Data[-15])
conf_naive=table(Test_Data$Churn,naive_predict)
calculate_data(conf_naive)

#Accuracy=88.70%
#FNR=74.10%
#Recall=25.89%


##############################################################################################
#Lets Build KNN MODEL

#For k=3
knn_predict=knn(Train_Data[-15],Test_Data[-15],cl=Train_Data$Churn,k=3)
knn_predict
conf_KNN=table(Test_Data$Churn,knn_predict)
calculate_data(conf_KNN)
#Accuracy=88.02%
#FNR=73.66%

###############################################################################################
#Lets Start with Random Forest after applying ROSE
library('randomForest')
library('inTrees')
library('e1071')

options(max.print = 100000000)
#For TRAIN DATA

rf=randomForest(Churn~.,traindata1,importance=TRUE,ntree=500,type='class')
plot(rf)

listoftrees01=RF2List(rf)

rules01=extractRules(listoftrees01,traindata1[,-15])
rules01[1:2,]

readrules01=presentRules(rules01,colnames(traindata1))
readrules01[1:2,]

#Lets Predict Test Values
rf_predict=predict(rf,testdata1[,-15])

#rf_predict

#Lets build confusion matrix for Train Data
library('data.table')

Conf_rf01=table(rf_predict,testdata1$Churn)
confusionMatrix(Conf_rf01)
calculate_data(Conf_rf01)

#Accuracy=88.14%
#FNR=42.79%


############################################################################################
#Logistic Regression
#For Train Data
logistic_model01=glm(Churn~.,traindata1,family = 'binomial')
#summary(logistic_model)

#Lets Predict Test Data

logistic_prediction01=predict(logistic_model,testdata1[-15],type = 'response')
logistic_prediction01=ifelse(logistic_prediction01>0.5,2,1)
conf_logistic01=table(testdata1$Churn,logistic_prediction01)
calculate_data(conf_logistic01)
#Accuracy=82.22%
#FNR=42.48%

#############################################################################################
#Let's build NAIVE BAYES MODEL

#For Train  Data
library('klaR')
naive_model01=naiveBayes(Churn~.,data = traindata1,type='Class')

#naive_model
naive_predict01=predict(naive_model01,testdata1[-15])
conf_naive01=table(testdata1$Churn,naive_predict01)
calculate_data(conf_naive01)

#Accuracy=84.47%
#FNR=46.11%


##############################################################################################
#Lets Build KNN MODEL

#For k=3
knn_predict01=knn(traindata1[-15],testdata1[-15],cl=traindata1$Churn,k=3)
knn_predict01
conf_KNN01=table(testdata1$Churn,knn_predict01)
calculate_data(conf_KNN01)
#Accuracy=79.59%
#FNR=58.54%

#############################################################################################
#On basis of above observation lets apply RF on actual test data
#Below model was used as an experiment to see what happens when an already predicted model is applied on Foreign Data

#FinalModel=predict(rf,Test_Data[,-15],type = 'prob')
#calculate_data(conf_final)
#Accuracy=89.20%
#FNR=42.71%

###################################################################################3
#Lets Plot ROC curve and Calculate AUC metric
#install.packages('pROC')
library('ROCR')
 #AUC = 0.88

plot(performance(prediction(FinalModel[,2],Test_Data$Churn),'tpr','fpr'))

###############################################################################################
# We have used to methods to calculate Accuracy,FNR and other parameters. 
  #In both mehtods RANDOM FOREST gave best results. 

#Out of both method we will finalize the method where we did not perform Over Sampling OR Under Sampling
  #We got better results without performing Over-Sampling or Under-Sampling

write.csv(Train_Data,"Final.csv")
write.csv(Test_Data,"Finaltest.csv")
