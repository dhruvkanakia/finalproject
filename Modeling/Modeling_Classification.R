
library(leaps)
library(ISLR)
library(forecast)
library(randomForest)
library(caret)
library(ROCR) 
library(e1071)
library(neuralnet)
library(ROSE)
setwd("D:/Final")

data <- read.csv('Clean_Data1.csv')
data <- data[,2:32]

head(data)

data1 <- read.csv('Clean_Data.csv')
data1 <- data1[,2:32]

dataDesired<- data1[1:11]
head(dataDesired)
head(data1)

set.seed(1234)
data_index <- createDataPartition(data1$diagnosis, p=0.7, list = FALSE)
train_data1 <- data1[data_index,]
test_data1 <- data1[-data_index,]



set.seed(123)
data_index1 <- createDataPartition(data$diagnosis, p=0.7, list = FALSE)
train_data <- data[data_index1,]
test_data <- data[-data_index1,]

train_data


lm.fit <-lm(diagnosis~.,data=data1)
summary(lm.fit)
step=step(lm.fit,direction = "backward")

names<- c('diagnosis','radius_mean','compactness_mean','concavity_mean','concave.points_mean','radius_se','smoothness_se',
               'concavity_se','concave.points_se','radius_worst','texture_worst','area_worst','concavity_worst','symmetry_worst','fractal_dimension_worst')

names_test<- c('radius_mean','compactness_mean','concavity_mean','concave.points_mean','radius_se','smoothness_se',
          'concavity_se','concave.points_se','radius_worst','texture_worst','area_worst','concavity_worst','symmetry_worst','fractal_dimension_worst')



wss.type<- nrow((dataDesired)-1)* sum(apply(dataDesired,2,var))
for (i in 2:15)
{
  wss.type[i]<- sum(kmeans(dataDesired,centers = i)$withinss)
}
plot (1:15,wss.type,type = "b",xlab="No of Clusters for type",ylab = "SOS")


km.fit<- kmeans(dataDesired,2,)

#############   randomforest ###############################33
 
randomForestAlgo<- function(x){
  rf <-randomForest(diagnosis ~  radius_mean + compactness_mean + concavity_mean + 
                      concave.points_mean + radius_se + smoothness_se + concavity_se + 
                      concave.points_se + radius_worst + texture_worst + area_worst + 
                      concavity_worst + symmetry_worst + fractal_dimension_worst,
                    ntree=25,na.action = na.exclude,family=binomial(link = "logit"),data=x,importance=T)
  return(rf)  
}


rfModel=randomForestAlgo(train_data1)
print(rfModel)



test.RF <- predict(rfModel,test_data1,type='response')
head(test.RF)
predRF<-rep(0,length(test.RF))
predRF[test.RF!= 0]<- 1 

rf=confusionMatrix(test_data1$diagnosis,predRF, positive="1")

rf

########################  logistic regression############################################

lg.fit<- glm(diagnosis ~.,data=data,family = binomial(link = "logit")  )

lg.fit<- glm(diagnosis ~.,data=data1)

summary(lg.fit)

data$diagnosis<-factor(data$diagnosis)

lg.fit<- glm(diagnosis ~  radius_mean + compactness_mean + concavity_mean + 
               concave.points_mean + radius_se + smoothness_se + concavity_se + 
               concave.points_se + radius_worst + texture_worst + area_worst + 
               concavity_worst + symmetry_worst + fractal_dimension_worst,data=train_data1,family = binomial(link = "logit"))
#step=step(lg.fit,direction = "both")
summary(lg.fit)

test.p <- predict(lg.fit,test_data1,type='response')
head(test.p)
pred<-rep(0,length(test.p))
pred[test.p >=0.5]<-1 

lr<-confusionMatrix(test_data1$diagnosis,pred)

lr

##############  NEURAL NETWORK WITHOUT UNDER SAMPLING ################

maxs <- apply(train_data1,2,max)
mins <- apply(train_data1,2,min)

#unique(scaled.data$radius_mean)

scaled.data <- as.data.frame(scale(train_data1,center = mins,scale = maxs-mins))
summary(scaled.data)
nn <- neuralnet(diagnosis~radius_mean + compactness_mean + concavity_mean + 
                  concave.points_mean + radius_se + smoothness_se + concavity_se + 
                  concave.points_se + radius_worst + texture_worst + area_worst + 
                  concavity_worst + symmetry_worst + fractal_dimension_worst,scaled.data,
                hidden=c(3,3),stepmax = 1e6,threshold = 0.01,linear.output = F)


print(nn)

maxs <- apply(test_data1,2,max)
mins <- apply(test_data1,2,min)
#head(scaled.dat.test)


scaled.dat.test <- as.data.frame(scale(test_data1,center = mins,scale = maxs-mins))
print(head(scaled.dat.test,10))

nn.pred<-compute(nn,scaled.dat.test[,names_test])
#help("neuralnet")
nn.pred$net.result
plot(nn)
nn.test<-rep(0,length(nn.pred$net.result))
nn.test[nn.pred$net.result>=0.5]<-1
nn_cla <- confusionMatrix(nn.test,scaled.dat.test$diagnosis)

nn_cla

###################   KNN #################



trctrl <- trainControl(method = "repeatedcv", number = 5,classProbs = TRUE,
                       summaryFunction = twoClassSummary)


knn_fit <- train(diagnosis ~., data = train_data, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),metric = "ROC",
                 tuneLength = 10)



pred_knn <- predict(knn_fit, test_data)
cm_knn <- confusionMatrix(pred_knn, test_data$diagnosis, positive = "M")


cm_knn



############## NAIVE BAYES ###################

trctrl <- trainControl(method = "cv", number = 5,classProbs = TRUE,
                       summaryFunction = twoClassSummary)


nb_model <- train(diagnosis ~., data = train_data, method = "nb",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),metric = "ROC",
                 tuneLength = 10)



pred_nb <- predict(nb_model, test_data)
cm_nb <- confusionMatrix(pred_nb, test_data$diagnosis, positive = "M")











###UNDER SAMPLING#############

set.seed(1234)
under_Sample<-ovun.sample(diagnosis~.,data=data,method = "under")$data
table(under_Sample$diagnosis)    

   


under_sample_backward<- under_Sample

summary(under_sample_backward)

data_index_undersampled <- createDataPartition(under_sample_backward$diagnosis, p=0.7, list = FALSE)
train_data_undersampled <- under_sample_backward[data_index_undersampled,]
test_data_undersampled <- under_sample_backward[-data_index_undersampled,]

test_data_undersampled


#######################  NEURAL NETWORK   with UNDER SAMPLING##################################





trctrl <- trainControl(method = "cv", number = 5,classProbs = TRUE,
                       summaryFunction = twoClassSummary)


nn_model_under <- train(diagnosis ~., data = train_data_undersampled, method = "nnet",
                        trControl=trctrl,
                        preProcess = c("center", "scale"),metric = "ROC",
                        tuneLength = 10)



pred_nn_model_under <- predict(nn_model_under, test_data_undersampled)
cm_nn_under <- confusionMatrix(pred_nn_model_under, test_data_undersampled$diagnosis, positive = "M")


cm_nn_under






############# RandomForest Under sampling ##################


trctrl <- trainControl(method = "cv", number = 5,classProbs = TRUE,
                       summaryFunction = twoClassSummary)


rf_model_under <- train(diagnosis ~., data = train_data_undersampled[,names], method = "ranger",
                  trControl=trctrl,
                  preProcess = c("center", "scale"),metric = "ROC",
                  tuneLength = 10)



pred_rf_model_under <- predict(rf_model_under, test_data_undersampled)
cm_rf_under <- confusionMatrix(pred_rf_model_under, test_data_undersampled$diagnosis, positive = "M")


cm_rf_under

################### Logistic Regression Under Sampling ###############3

trctrl <- trainControl(method = "cv", number = 5,classProbs = TRUE,
                       summaryFunction = twoClassSummary)


lr_model_under <- train(diagnosis ~., data = train_data_undersampled, method = "glm",
                        trControl=trctrl,
                        preProcess = c("center", "scale"),metric = "ROC",
                        tuneLength = 10)



pred_lf_model_under <- predict(lr_model_under, test_data_undersampled)
cm_lr_under <- confusionMatrix(pred_lf_model_under, test_data_undersampled$diagnosis, positive = "M")


cm_lr_under



###################### KNN ALGORITHM #########################


trctrl <- trainControl(method = "cv", number = 5,classProbs = TRUE,
                       summaryFunction = twoClassSummary)


knn_fit_under <- train(diagnosis ~., data = train_data_undersampled, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),metric = "ROC",
                 tuneLength = 10)



pred_knn_under <- predict(knn_fit_under, test_data_undersampled)
cm_knn_under <- confusionMatrix(pred_knn_under, test_data_undersampled$diagnosis, positive = "M")


cm_knn_under




####################### NAIVE BAYES UNDER SAMPLING  #############################


trctrl <- trainControl(method = "cv", number = 5,classProbs = TRUE,
                       summaryFunction = twoClassSummary)


nb_model_under <- train(diagnosis ~., data = train_data_undersampled, method = "nb",
                        trControl=trctrl,
                        preProcess = c("center", "scale"),metric = "ROC",
                        tuneLength = 10)



pred_nb_model_under <- predict(nb_model_under, test_data_undersampled)
cm_nb_under <- confusionMatrix(pred_nb_model_under, test_data_undersampled$diagnosis, positive = "M")


cm_nb_under


############## PCA ###################

pca_df <- prcomp(data[,(2:31)], center = TRUE, scale = TRUE)
pca_df
pca_df<- data.frame(pca_df$x[,1:17])
pca_df
diag <- data[.1:2]
diag

final_pca<-cbind(diag,pca_df)

final_pca
summary(pca_df)

########### RANDOM FOREST WITH PCA #################

trctrl <- trainControl(method = "cv", number = 5,classProbs = TRUE,
                       summaryFunction = twoClassSummary,preProcOptions = list(thresh = 0.99))



rf_model_pca <- train(diagnosis ~., data = train_data, method = "ranger",
                        trControl=trctrl,
                        preProcess = c("center", "scale","pca"),metric = "ROC",
                        tuneLength = 10)



pred_rf_model_pca <- predict(rf_model_pca, test_data)
cm_rf_pca1 <- confusionMatrix(pred_rf_model_pca, test_data$diagnosis, positive = "M")


cm_rf_pca1
  
#############################################################################
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)
set.seed(825)
gbmFit1 <- train(diagnosis ~ ., train_data, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1

############# LOGISTIC REGRESSION WITH PCA ##################



trctrl <- trainControl(method = "cv", number = 5,classProbs = TRUE,
                       summaryFunction = twoClassSummary,preProcOptions = list(thresh = 0.99))



lr_model_pca <- train(diagnosis ~., data = train_data, method = "glm",
                      trControl=trctrl,
                      preProcess = c("center", "scale","pca"),metric = "ROC",
                      tuneLength = 10)



pred_lr_model_pca <- predict(lr_model_pca, test_data)
cm_lr_pca1 <- confusionMatrix(pred_lr_model_pca, test_data$diagnosis, positive = "M")


cm_lr_pca1


########### NEURAL NETWORK USING PCA #########

trctrl <- trainControl(method = "cv", number = 10,classProbs = TRUE,repeats = 2,
                       summaryFunction = twoClassSummary,preProcOptions = list(thresh = 0.99))


set.seed(3216)
nn_model_pca <- train(diagnosis ~., data = train_data, method = "nnet",
                      trControl=trctrl,
                      preProcess = c("center", "scale","pca"),metric = "ROC",
                      tuneLength = 10)


nn_model_pca
pred_nn_model_pca <- predict(nn_model_pca, test_data)

#pred_nn_model_pca
cm_nn_pca1 <- confusionMatrix(pred_nn_model_pca, test_data$diagnosis, positive = "M")


cm_nn_pca1




################ NAIVE BAYES USING PCA ############

trctrl <- trainControl(method = "cv", number = 5,classProbs = TRUE,
                       summaryFunction = twoClassSummary,preProcOptions = list(thresh = 0.99))



nb_model_pca <- train(diagnosis ~., data = train_data, method = "nb",
                      trControl=trctrl,
                      preProcess = c("center", "scale","pca"),metric = "ROC",
                      tuneLength = 10)



pred_nb_model_pca <- predict(nb_model_pca, test_data)
cm_nb_pca1 <- confusionMatrix(pred_nb_model_pca, test_data$diagnosis, positive = "M")


cm_nb_pca1


################ K  USING PCA ##################


trctrl <- trainControl(method = "cv", number = 5,classProbs = TRUE,
                       summaryFunction = twoClassSummary,preProcOptions = list(thresh = 0.99))



knn_model_pca <- train(diagnosis ~., data = train_data, method = "knn",
                      trControl=trctrl,
                      preProcess = c("center", "scale","pca"),metric = "ROC",
                      tuneLength = 10)





pred_knn_model_pca <- predict(knn_model_pca, test_data)
cm_knn_pca1 <- confusionMatrix(pred_knn_model_pca, test_data$diagnosis, positive = "M")


cm_knn_pca1


f <- function(x,y) {
  ab <- train(diagnosis ~., data = train_data, method = x,
                         trControl=y,
                         preProcess = c("center", "scale","pca"),metric = "ROC",
                         tuneLength = 10)
  return(ab)
}

ac<- f("knn",trctrl)
pred_knn_model_pca1 <- predict(ac, test_data)
cm_knn_pca2 <- confusionMatrix(pred_knn_model_pca1, test_data$diagnosis, positive = "M")


cm_knn_pca2





