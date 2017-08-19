setwd("D:/Final")
library(caret)
library(forecast)
#install.packages("factoextra")

library(factoextra)
library(FactoMineR)

#cancerTypeData <- read.table("brca_metabric_clinical_data.tsv",sep = "\t",header = TRUE)
cancerTypeData <- read.table("brca_metabric_clinical_data.tsv",sep = "\t",na.strings = c("","NA"))
head(cancerTypeData)
names(cancerTypeData)<- c("PatientID","SampleID","NottinghamPrognosticIndex","CancerType","CancerTypeDetailed","Cellularity",
          "Chemotherapy","Cohort","ERStatus","ERStatusMeasuredbyIHC","HER2Status","HER2StatusMeasuredBySNP6"
          ,"HormoneTherapy","InferredMenopausalState","IntegrativeCluster","NeoplasmHistologicGrade"
          ,"AgeAtDiagnosis","OncotreeCode","OverallSurvival(Months)","OverallSurvivalStatus","PRStatus",
          "Pam50 + Claudin-lowSubtype","3-GeneClassifierSubtype","PatientVitalStatus","PrimaryTumorLaterality",
          "RadioTherapy","CancerStudies","SampleType","TumorOtherHistologicSubtype","TumorSize","TumorStage"
          ,"TypeOfBreastSurgery")
# head(cancerTypeData)
# str(cancerTypeData)
# 
# cancerTypeData[,22]
# cancerTypeData$NottinghamPrognosticIndex<-as.integer(cancerTypeData$NottinghamPrognosticIndex)
# unique(cancerTypeData$PRStatus)
# typeof(cancerTypeData$TumorStage)
# table(cancerTypeData$RadioTherapy)
# table(desiredData$RadioTherapy)
# is.na(cancerTypeData$AgeAtDiagnosis)

levels(cancerTypeData$Cellularity)<-c(2,0,1)
levels(cancerTypeData$Chemotherapy)<-c(0,1)
levels(cancerTypeData$ERStatusMeasuredbyIHC)<-c(0,1)
levels(cancerTypeData$HER2Status)<-c(0,1)
levels(cancerTypeData$HormoneTherapy)<-c(0,1)
levels(cancerTypeData$InferredMenopausalState)<-c(1,0)
levels(cancerTypeData$NeoplasmHistologicGrade)<-c(1,2,3)
levels(cancerTypeData$OverallSurvivalStatus)<-c(0,1)
levels(cancerTypeData$PRStatus)<-c(0,1)
levels(cancerTypeData$PrimaryTumorLaterality)<-c(1,2)
levels(cancerTypeData$RadioTherapy)<-c(1,2)

#####Selecting Deisred Columns from dataset #####
desiredData <- cancerTypeData[,c(3,5,6,7,10,11,13,14,16,17,20,21,22,25,26,30,31)]
#####Selecting Deisred Columns from dataset #####
# unique(cancerTypeData$HER2Status)
# factor(cancerTypeData$HER2Status)
head(desiredData)
head(cancerTypeData)
typeof(desiredData)
# desiredData<-na.omit(desiredData)
# head(desiredData)

#desiredData<-desiredData[-desiredData$Cellularity=='',]
#desiredData<-desiredData[!(is.na(desiredData$Cellularity) | desiredData$Cellularity==" "), ]
#head(desiredData)
#desiredData<-data.frame(desiredData,stringsAsFactors=FALSE)
#########Clean Dataframe############

levels(desiredData$Cellularity)<-c(2,0,1)
levels(desiredData$Chemotherapy)<-c(0,1)
levels(desiredData$ERStatusMeasuredbyIHC)<-c(0,1)
levels(desiredData$HER2Status)<-c(0,1)
levels(desiredData$HormoneTherapy)<-c(0,1)
levels(desiredData$InferredMenopausalState)<-c(1,0)
levels(desiredData$NeoplasmHistologicGrade)<-c(1,2,3)
levels(desiredData$OverallSurvivalStatus)<-c(0,1)
levels(desiredData$PRStatus)<-c(0,1)
levels(desiredData$PrimaryTumorLaterality)<-c(1,2)
levels(desiredData$RadioTherapy)<-c(1,2)
#levels(desiredData$TumorStage)
desiredDataCleanFile<-(desiredData[complete.cases(desiredData), ])
#########Clean Dataframe############

##############Clustering######################

hist(desiredDataCleanFile$AgeAtDiagnosis)
d<-density(desiredDataCleanFile$AgeAtDiagnosis)
plot(d)
polygon(d, col="blue", border="black")

dataLessthan60<-subset(desiredDataCleanFile, AgeAtDiagnosis <=62)
dataGreaterthan60<-subset(desiredDataCleanFile, AgeAtDiagnosis > 62)

nrow(dataLessthan60)
nrow(dataGreaterthan60)

write.csv(dataLessthan60,"dataLessthan60.csv")
write.csv(dataGreaterthan60,"dataGreaterthan60.csv")


##############Clustering######################


#is.na(desiredDataCleanFile)


str(desiredDataCleanFile)
unique(desiredData$CancerTypeDetailed)
write.csv(desiredData,"desiredData.csv")
sum(is.na(desiredData$Cellularity))
sum(is.in(desiredDataCleanFile))
desiredDataCleanFile<-as.matrix(desiredDataCleanFile)
nrow(desiredDataCleanFile)
nrow(desiredDataCleanFilea.features)
data("iris")
iris

desiredDataCleanFilea.features<-desiredDataCleanFile
desiredDataCleanFile<-desiredDataCleanFile[,-2]
help("kmeans")
typeof(desiredDataCleanFile[,11])
km.out<- kmeans(desiredDataCleanFile[,c(4,5,11)],6,nstart = 50)
sum(is.na(desiredDataCleanFile[,c(4,5,11)]))

res.mca <- MCA(desiredDataCleanFile[,c(4,5,11)], graph=FALSE)
res.hcpc <- HCPC(res.mca, graph = FALSE)
plot(res.hcpc, choice = "map")

fviz_cluster(res.hcpc, repel = TRUE, cluster = km.out$cluster,
geom = c("point"), show.clust.cent = FALSE, ellipse = TRUE,
ellipse.type = "cocave", ellipse.level = 3.75, ellipse.alpha = 0.25, pointsize = 10,
main = "K-Means")

nrow(as.data.frame(km.out$cluster))
unique(desiredDataCleanFile$Cluster)
head(desiredDataCleanFile)
#desiredDataCleanFile[,17] <- as.data.frame(km.out$cluster)
colnames(desiredDataCleanFile)[which(names(desiredDataCleanFile)=="km.out$cluster")]<-"Cluster"
desiredDataCleanFile[km.out$cluster==1,]

library(ggplot2)
ggplot(desiredDataCleanFile, aes(ERStatusMeasuredbyIHC, PRStatus, color = Cluster)) + geom_point()

irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(desiredDataCleanFilea.features, aes(ERStatusMeasuredbyIHC, PRStatus, color = desiredDataCleanFilea.features$`Pam50 + Claudin-lowSubtype`)) + geom_point()

typeof(desiredDataCleanFile$HER2Status)

fviz_cluster( # plot K-means clustering
  list(data = desiredDataCleanFile[,c(4,5,11)], cluster = km.out$cluster),
  geom = c("point"),  repel = TRUE, show.clust.cent = FALSE, ellipse = TRUE,
  ellipse.type = "convex", ellipse.level = 1.95, ellipse.alpha = 0.25, pointsize = 2,
  main = "K-Means")



names(km.out)
km.out$cluster
km.out$size
km.out$

order(km.out$cluster)  

head(desiredDataCleanFilea.features)



  

desiredDataCleanFilea.features$CancerTypeDetailed
desiredDataCleanFile<-as.data.frame(desiredDataCleanFile)
desiredDataCleanFile<-as.data.frame(desiredDataCleanFile)
unique(desiredDataCleanFilea.features$CancerTypeDetailed)
table(desiredDataCleanFilea.features$`Pam50 + Claudin-lowSubtype`,km.out$cluster)
table(km.out$cluster,desiredDataCleanFilea.features$CancerTypeDetailed)
#confusionMatrix(desiredDataCleanFilea.features$CancerTypeDetailed,km.out$cluster)


plot()
table(desiredDataCleanFilea.features$CancerTypeDetailed)
table(km.out$cluster)

levels(desiredDataCleanFilea.features$CancerTypeDetailed)
levels(km.out$cluster)<-c("Breast","Breast Ductal Carcinoma In Situ",
"Breast Invasive Ductal Carcinoma","Breast Invasive Lobular Carcinoma",
"Breast Mixed Ductal and Lobular Carcinoma",
"Invasive Breast Carcinoma",
"Phyllodes Tumor of the Breast")

length(desiredDataCleanFilea.features$CancerTypeDetailed)
length(km.out$cluster)
plot(desiredDataCleanFile,col=(km.out$cluster))

sum(is.na(desiredDataCleanFilea.features$CancerTypeDetailed))
na.omit(desiredDataCleanFilea.features$CancerTypeDetailed)


desiredDataCleanFile<-desiredDataCleanFile[,-13]

head(desiredDataCleanFile)
dim(desiredDataCleanFile)
head(desiredDataCleanFilea.features)
unique(desiredData$TumorStage)

head(ageCount1)
ageCount1<-round(ageCount1,2)
head(ageCount)
typeof(ageCount$Var1)
ageCount1<-data.frame(table(desiredDataCleanFile$AgeAtDiagnosis))
ageCount <- data.frame(var1 = 21:100, value = runif(80))
ageCount <- transform(ageCount, bin = cut(var1, 20))
library(plyr)
ddply(ageCount, "bin", summarize, totVal = sum(value))

hist(desiredDataCleanFile$AgeAtDiagnosis)
d<-density(desiredDataCleanFile$AgeAtDiagnosis)
plot(d)
polygon(d, col="blue", border="black")

head()
dataLessthan60<-subset(desiredDataCleanFile, AgeAtDiagnosis <=62)
dataGreaterthan60<-subset(desiredDataCleanFile, AgeAtDiagnosis > 62)

nrow(dataLessthan60)
nrow(dataGreaterthan60)

write.csv(dataLessthan60,"dataLessthan60.csv")
write.csv(dataGreaterthan60,"dataGreaterthan60.csv")

#split(ageCount,cut(ageCount$Var1,seq(21,100,by=10)))

plot(desiredDataCleanFile$AgeAtDiagnosis,nrow(desiredDataCleanFile$AgeAtDiagnosis))

plot(desiredDataCleanFilea.features$CancerTypeDetailed,desiredDataCleanFilea.features$TumorStage)
plot(desiredDataCleanFilea.features$CancerTypeDetailed,desiredDataCleanFilea.features$AgeAtDiagnosis)

wss<- nrow((desiredDataCleanFile[,c(4,5,11)])-1)* sum(apply(desiredDataCleanFile[,c(4,5,11)],2,var))
for (i in 2:4)
{
  wss[i]<- sum(kmeans(desiredDataCleanFile[,c(4,5,11)],centers = i)$withinss)
}
plot (1:8,wss,type = "b",xlab="No of Clusters",ylab = "SOS")
wss