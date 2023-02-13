#Created: 1/4/2021
#Created By: Michael Laidlaw
#Purpose: Generate a critical care risk score
library(RODBC)
library(recipes)
library(caret)
library(ggplot2)
library(plotly)
library(sqldf)
library(pROC)
set.seed(1121)
#------------------------------------------------------------
#Get the Data
#-------------------------------------------------------------
conn<-odbcConnect("MeditechDRzcus")
data<-sqlQuery(conn,"exec [ML_CritCarePrediction_Test]")
data_backup<-data
odbcCloseAll()
#-------------------------------------------------------------
#Cleanse the data
#-------------------------------------------------------------
data$Lab_ALT_Init=as.double(data$Lab_ALT_Init)
data$Lab_ALT_Max=as.double(data$Lab_ALT_Max)
data$Lab_PLT_Init<-as.double(data$Lab_PLT_Init)
data$Lab_PLT_Min=as.double(data$Lab_PLT_Min)
data$Lab_K_Init=as.double(data$Lab_K_Init)
data$Lab_NA_Init=as.double(data$Lab_NA_Init)
data$Lab_BUN_Init=as.double(data$Lab_BUN_Init)
data$Lab_BUN_Max=as.double(data$Lab_BUN_Max)
data$Lab_TP_Init=as.double(data$Lab_TP_Init)
data$Lab_GLU_Init=as.double(data$Lab_GLU_Init)
data$Lab_GLU_Max=as.double(data$Lab_GLU_Max)
data$Polst=as.numeric(data$Polst)
data$AgeAtAdmin=as.numeric(data$AgeAtAdmin)
data$BPM=as.numeric(data$BPM)
data$Pulse_Initial=as.numeric(data$Pulse_Initial)
crit_recipe<-recipe(CriticalCareTransfer~.,data=data)
print(crit_recipe$var_info,n=nrow(crit_recipe$var_info))
crit_steps<-crit_recipe %>% step_knnimpute(all_numeric())
crit_prepped_data<-prep(crit_steps,training=data)
preprocessdata<-bake(crit_prepped_data,data)
#--------------------------------------------------------------------------
#Generate the model
#--------------------------------------------------------------------------
train_control <- trainControl(method="repeatedcv", repeats=2, summaryFunction=twoClassSummary, classProbs=T,
                              savePredictions = T)
dataSplit<-createDataPartition(preprocessdata$CriticalCareTransfer,p=.7,list=FALSE)
trainPart<-preprocessdata[dataSplit,]
testPart<-preprocessdata[-dataSplit,]
testPart2<-preprocessdata[-dataSplit,]
model<-train(CriticalCareTransfer~.,data=trainPart,method="rf",trControl = train_control, cutoff=c(.1,.9))
#model<-train(CriticalCareTransfer~.,data=trainPart,method="glm",trControl = train_control) #, cutoff=c(.3,.7))
#---------------------------------------------------------------------------
#Evaluate the Model
#---------------------------------------------------------------------------
predvals<-predict(model,newdata=testPart)
testPart$Predicted<-predict(model,newdata=testPart, type="prob")
testPart$prob_y<-predvals
TestingOrd<-sqldf('select CriticalCareTransfer from testPart order by prob_y desc')
selectedIndices <- model$pred$mtry == model$finalModel$mtry
plot.roc(model$pred$obs[selectedIndices],
         model$pred$Y[selectedIndices])
confusionMatrix(testPart$CriticalCareTransfer,testPart$prob_y



