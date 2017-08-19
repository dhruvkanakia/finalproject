setwd("D:/Final")
# install.packages("ROCR")
library(class)
library(caret)
library(leaps)
library(forecast)
library(e1071)
library(ROCR) 
library(ROSE)
library(randomForest)
library (neuralnet)
library(nnet)
help(nnet)
# riskFactors <- read.table("BCSC_risk_factors_summarized.csv",sep = ",")
# head(riskFactors)
# riskFactors<-riskFactors[-1,]
# names(riskFactors)<- c("year","age_group_5_years","race_eth","first_degree_hx","age_menarche"                       ,"age_first_birth","BIRADS_breast_density","current_hrt","menopaus","bmi_group","biophx","breast_cancer_history","count")

risk <- read.csv("Hist.csv")
# risk <- read.table("risk.TXT",sep = "")

head(risk)
risk<- risk[,2:15]
head(risk) 
# positiveCases<-orginalData[orginalData$cancerStatus=='1',]
# nrow(positiveCases)
# summary(positiveCases$count)
# unique((positiveCases$count))

# write.csv(risk, file = "risk.csv")
# names(risk) <-c("menopaus","agegrp","density","race","Hispanic","bmi","agefirst",
#                 "noOfFirstDegRelativesCan","prevcanProc","lastMamm","surgMeno","hrt","invasive"
#                 ,"cancerStatus","type","count")

# table(risk$agegrp)
# mean(risk$menopaus)
# canstatusTrue<-(risk[risk$cancerStatus=="1",])
# table(canstatusTrue$menopaus)
# #unique(risk$cancerStatus)
# #lm.fit <-lm(cancerStatus~.,data=risk)
# #summary(lm.fit)
# #step=step(lm.fit,direction = "backward")
# 
# risk[,"menopaus"]
# 
# risk[,"AgeGroup2"]<-ifelse(risk$agegrp=="2", 1, 0)
# risk[,"AgeGroup3"]<-ifelse(risk$agegrp=="3", 1, 0)
# risk[,"AgeGroup4"]<-ifelse(risk$agegrp=="4", 1, 0)
# risk[,"AgeGroup5"]<-ifelse(risk$agegrp=="5", 1, 0)
# risk[,"AgeGroup6"]<-ifelse(risk$agegrp=="6", 1, 0)
# risk[,"AgeGroup7"]<-ifelse(risk$agegrp=="7", 1, 0)
# risk[,"AgeGroup8"]<-ifelse(risk$agegrp=="8", 1, 0)
# risk[,"AgeGroup9"]<-ifelse(risk$agegrp=="9", 1, 0)
# risk[,"AgeGroup10"]<-ifelse(risk$agegrp=="10", 1, 0)
# # risk<- risk[,-2]
# # risk<- risk[,-15]


chopFn<- function(x,y){
  size<- floor(y * nrow(x))
  less <- sample(seq_len(nrow(x)), size = size)
  testData=x[less,]
  return(testData)
}


testData=chopFn(risk,0.75)
trainData=chopFn(risk,0.25)
#table(risk$count)
# unique(risk$agefirst)
# plot(risk$agegrp,risk$count)


# predVariable = predict(lm.fit, testData)
# summary(predVariable)
# accuracy(predVariable,testData$cancerStatus)
# plot(predVariable,testData$cancerStatus, col='blue',main='Real vs predicted Linear',xlab="Predicted",ylab="Actual")

# table(risk$agegrp)
# unique(risk$agegrp)

###################
lg.fits<- glm(cancerStatus~.,data=risk)
summary(lg.fits)
lg.fit<- glm(cancerStatus ~ menopaus+agegrp + race + Hispanic + 
               bmi + agefirst + noOfFirstDegRelativesCan +invasive+
               prevcanProc+lastMamm,data=risk)
summary(lg.fit)
step=step(lg.fits,direction='backward')
step = step(lg.fit)
test.p <- predict(lg.fit,testData)
pred<-rep(0,length(test.p))
pred[test.p >=0.5]<-1 

confusionMatrix(pred,testData$cancerStatus)

body(predict.lm)
#########Under Sampling ########


#length(lg.fit$coefficients) > lg.fit$rank
(table(risk$cancerStatus))
set.seed(1234)
under_Sample<-ovun.sample(cancerStatus~.,data=risk,method = "under")$data
table(under_Sample$cancerStatus)     
table(risk$cancerStatus)     

trainData_US=chopFn(under_Sample,0.75)
testData_US=chopFn(under_Sample,0.25)

write.csv(under_Sample, file = "under_Sample.csv")

#########Under Sampling ########
########Logistic Regression UnderSammpling############
lg.US <-glm(cancerStatus~menopaus+agegrp + race + Hispanic + 
              bmi + agefirst + noOfFirstDegRelativesCan +invasive+
              prevcanProc+lastMamm,data=trainData_US)



test.P_US <- predict(lg.US,testData_US)

pred.test_US <- predict(lg.US,testData_US)
pred_US<-rep(0,length(pred.test_US))
pred_US[pred.test_US >=0.5]<-1 

CM_lg.US<-confusionMatrix(pred_US,testData_US$cancerStatus)

CM_lg.US





########Logistic Regression UnderSammpling############



######Random Forest UnderSammpling#########
rf.US <-randomForest(cancerStatus ~ menopaus+agegrp + race + Hispanic + 
                       bmi + agefirst + noOfFirstDegRelativesCan +invasive+
                       prevcanProc+lastMamm,ntree=500,
                  data=trainData_US,importance=T)
#typeof(trainData$cancerStatus)
trainData_US$cancerStatus<-as.factor(trainData_US$cancerStatus)
testData_US$cancerStatus<- as.factor(testData_US$cancerStatus)
#str(trainData_US)
test.RF_US <- predict(rf.US,testData_US)
#head(test.RF)
predRF_US<-rep(0,length(test.RF_US))
predRF_US[test.RF_US!= 0]<- 1 

CM_rf.US<-confusionMatrix(predRF_US,testData_US$cancerStatus)
CM_rf.US
predictionRF_US<- prediction(as.numeric(test.RF_US),as.numeric(testData_US$cancerStatus))
performanceRF_US<-performance(predictionRF_US,measure="tpr",x.measure = "fpr")
plot(performanceRF_US,main="ROC Curve",xlab="1-Specificity",ylab="Senssitivity")

######Random Forest UnderSammpling#########


str(under_Sample)
unique(under_Sample$noOfFirstDegRelativesCan)


maxs_US <- apply(under_Sample[,c(1:16)],2,max)
mins_US <- apply(under_Sample[,c(1:16)],2,min)

# typeof(Q1LESSDATA$OriginalLoanTerm)

scaled.data_US <- as.data.frame(scale(under_Sample[,c(1:16)],center = mins_US,scale = maxs_US-mins_US))
# cancerStatus<- as.numeric(under_Sample$cancerStatus)
# scaled.data<-cbind(cancerStatus,scaled.data_US)
# str(scaled.data_US)
# head(cancerStatus)
# str(scaled.data)
# require(reshape2)
# melt(scaled.data)

head(scaled.data)
write.csv(scaled.data_US, file = "scaleddata_US.csv")




######Neural Network Under Sampling#######
head(trainData_US)
maxs_US <- apply(trainData_US,2,max)
mins_US <- apply(trainData_US,2,min)

# typeof(Q1LESSDATA$OriginalLoanTerm)
length(mins_US)==ncol(trainData_US)
scaled.data_US <- as.data.frame(scale(trainData_US,center = mins_US,scale = maxs_US-mins_US))
#trainData_US$cancerStatus<- as.numeric(trainData_US$cancerStatus)
# scaled.data=cbind(cancerStatus,scaled.data)
# print(head(scaled.data,10))

# unique(Q1LESSDATA$SuperConfirmingFlag)
library(neuralnet)
nn.US <- neuralnet(cancerStatus ~ density + race + Hispanic + 
                     bmi + agefirst + noOfFirstDegRelativesCan +
                     prevcanProc+lastMamm+type+count+AgeGroup2+
                     AgeGroup4+AgeGroup5+AgeGroup6+AgeGroup7+
                     AgeGroup8+AgeGroup9,scaled.data_US,
                hidden=c(3),stepmax = 1e9,threshold = 0.01,linear.output = T)

summary(nn)


maxs_test_US <- apply(testData_US,2,max)
mins_test_US <- apply(testData_US,2,min)

# typeof(Q1LESSDATA$OriginalLoanTerm)

scaled.data_Test_US <- as.data.frame(scale(testData_US,center = mins_test_US,scale = maxs_test_US-mins_test_US))
write.csv(scaled.data_Test_US,'neural_test_undersampled.csv')

# cancerStatus<- as.numeric(testData$cancerStatus)
# scaled.data_Test=cbind(cancerStatus,scaled.data_Test)

# columnames<-names(scaled.data_Test[,c(1:13,15:25)])
# scaled.data_Test<-scaled.data_Test[,c(1:13,15:25)]
# head(scaled.data_Test)
# scaled.data_Test<-scaled.data_Test[,-2]

usedColumns<-c("density","race","Hispanic",
               "bmi","agefirst","noOfFirstDegRelativesCan","prevcanProc",
               "lastMamm","type","count","AgeGroup2",
               "AgeGroup4","AgeGroup5","AgeGroup6"
               ,"AgeGroup7","AgeGroup8","AgeGroup9")

predictednn_US <- compute(nn.US,scaled.data_Test_US[,usedColumns])
# accuracy(predictednn,scaled.data_Test[,1])
# typeof(scaled.data_Test[,1])
#print(head(predictednn$net.result))
predictednn_US$net.result <- sapply(predictednn_US$net.result,round,digits=1)
table(testData_US$cancerStatus,predictednn_US$net.result)
######Neural Network#######

##########Naive Bayes Under Sampling############

library(e1071)
nb.US <- naiveBayes(cancerStatus ~  menopaus+agegrp + race + Hispanic + 
                      bmi + agefirst + noOfFirstDegRelativesCan +invasive+
                      prevcanProc+lastMamm,data=trainData_US)

trainData_US$cancerStatus<-as.factor(trainData_US$cancerStatus)
table(trainData_US$cancerStatus)
pred.nb.US<- predict(nb.US,testData_US)
#help("naive")

CM_nb.US<-confusionMatrix(pred.nb.US,testData_US$cancerStatus)
CM_nb.US

##########Naive Bayes Under Sampling############



##########KNN under Sampling############
dim(under_Sample)
dim(trainData_US)
dim(testData_US)
head(trainData_US)
head(under_Sample)
library(class)


# intrain <- createDataPartition(y = risk$cancerStatus, p= 0.7, list = FALSE)
# training <- risk[intrain,]
# testing <- risk[-intrain,]
# trainData_US=chopFn(under_Sample,0.75)
# testData_US=chopFn(under_Sample,0.25)
# library(caret)
# head(training)
# trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 3)
# set.seed(3333)
# knn_fit <- train(cancerStatus ~., data =  training, method = "knn",
#                  trControl=trctrl,
#                  preProcess = c("center", "scale"),
#                  tuneLength = 10)
# knn_fit
# pred<- predict(knn_fit,testing)
# knn_cm<- confusionMatrix(pred,testing$cancerStatus)



# normalize<- function(x){
#   return ((x-min(x))/(max(x)-min(x)))
# }
# head(trainData_US)
# trainData_US<-as.data.frame(lapply(trainData_US[1:15],normalize))
# testData_US<-as.data.frame(lapply(testData_US[1:15],normalize))
# head(trainData_US)
# trainData_US<-trainData_US[c(1:2,4:5,6:10,13:16)]
# testData_US<-testData_US[c(1:2,4:5,6:10,13:16)]
# head(trainData_US)
# knn.US<- knn(trainData_US,testData_US,trainData_US[,11],k=5)
# 
# data.set = data.frame(cancerStatus = knn.US) 
# 
# confusionMatrix(knn.US,testData_US$cancerStatus)
# #table(knn.US,testData_US$cancerStatus)
# ##########KNN under Sampling############



########Over Sampling##############
set.seed(1234)
over_Sample<-ovun.sample(cancerStatus~.,data=risk,method = "over")$data
table(over_Sample$cancerStatus)     
head(over_Sample)
write.csv(over_Sample, file = "over_Sample.csv")

trainData_OS=chopFn(over_Sample,0.75)
testData_OS=chopFn(over_Sample,0.25)

head(trainData_OS)
#######Logistic Over Sampling############
lg.fit_OS<- glm(cancerStatus ~  menopaus+agegrp + race + Hispanic + 
                  bmi + agefirst + noOfFirstDegRelativesCan +invasive+
                  prevcanProc+lastMamm,data=trainData_OS)
#summary(lg.fit_OS)
test.p_OS <- predict(lg.fit_OS,testData_OS)
pred_OS<-rep(0,length(test.p_OS))
pred_OS[test.p_OS >=0.5]<-1 

CM_LR_OS<-confusionMatrix(pred_OS,testData_OS$cancerStatus)
CM_LR_OS
body(predict.lm)
#################


##########Naive Bayes Over Sampling############

library(e1071)
trainData_OS$cancerStatus<-as.factor(trainData_OS$cancerStatus)
nb.OS <- naiveBayes(cancerStatus ~  menopaus+agegrp + race + Hispanic + 
                      bmi + agefirst + noOfFirstDegRelativesCan +invasive+
                      prevcanProc+lastMamm,data=trainData_OS)


table(trainData_OS$cancerStatus)
pred.nb.OS<- predict(nb.OS,testData_OS)
#help("naive")

CM_nb.OS<-confusionMatrix(pred.nb.OS,testData_OS$cancerStatus)
CM_nb.OS

##########Naive Bayes Over Sampling############


# ##########KNN Over Sampling############
# dim(over_Sample)
# dim(trainData_OS)
# dim(testData_OS)
# head(trainData_OS)
# library(class)
# head(trainData_OS)
# knn.OS<- knn(trainData_OS,testData_OS,trainData_OS[,14],k=4)
# confusionMatrix(knn.OS,testData_OS$cancerStatus)
# 
# table(knn.OS,testData_OS$cancerStatus)
# ##########KNN Over Sampling############
# 
# help(knn)


###########Random Forest Over Sampling###############

rf_OS <-randomForest(cancerStatus ~ menopaus+agegrp + race + Hispanic + 
                       bmi + agefirst + noOfFirstDegRelativesCan +invasive+
                       prevcanProc+lastMamm,ntree=5,
                  data=trainData_OS,importance=T)
#typeof(trainData$cancerStatus)
trainData_OS$cancerStatus<-as.factor(trainData_OS$cancerStatus)


test.RF_OS <- predict(rf_OS,testData_OS)
head(test.RF_OS)
predRF_OS<-rep(0,length(test.RF_OS))
predRF_OS[test.RF_OS!= 0]<- 1 

CM_RF_OS<-confusionMatrix(testData_OS$cancerStatus,predRF_OS)
CM_RF_OS
predictionRF<- prediction(as.numeric(test.RF_OS),as.numeric(testData$cancerStatus))
performanceRF<-performance(predictionRF,measure="tpr",x.measure = "fpr")
plot(performanceRF,main="ROC Curve",xlab="1-Specificity",ylab="Senssitivity")



###########Random Forest Over Sampling############### 

maxs <- apply(over_Sample[,c(1:16)],2,max)
mins <- apply(over_Sample[,c(1:16)],2,min)

# typeof(Q1LESSDATA$OriginalLoanTerm)

scaled.data_OS <- as.data.frame(scale(over_Sample[,c(1:16)],center = mins,scale = maxs-mins))
cancerStatus<- as.numeric(over_Sample$cancerStatus)
scaled.data<-cbind(cancerStatus,scaled.data)

require(reshape2)
melt(scaled.data)

head(scaled.data)
write.csv(scaled.data_OS, file = "scaleddata_OS.csv")



model_list <- list(RF_os=rf_OS,RF_US=rf.US, 
                  NB_OS=nb.OS,NB_US=nb.US, Logistic_US=lg.US,LG_OS=lg.fit_OS)





cm_list <- list(RF_US=CM_rf.US, RF_OS=CM_RF_OS, LR_US=CM_lg.US,LR_OS=CM_LR_OS,NB_US=CM_nb.US,NB_OS=CM_nb.OS) 
cm_list_results <- sapply(cm_list, function(x) x$byClass)


cm_results_max <- apply(cm_list_results, 1, which.is.max)
output_report <- data.frame(metric=names(cm_results_max), 
                            best_model=colnames(cm_list_results)[cm_results_max],
                            value=mapply(function(x,y) {cm_list_results[x,y]}, 
                                         names(cm_results_max), 
                                         cm_results_max))
rownames(output_report) <- NULL
output_report
