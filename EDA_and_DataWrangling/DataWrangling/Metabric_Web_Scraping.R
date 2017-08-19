
library(cgdsr)
# Create CGDS object
mycgds = CGDS("http://www.cbioportal.org/")
if ((dir.exists("Data"))==TRUE){
  print('It exists')
}else{
  dir.create("Data")
  print('Created')
}
a<-getClinicalData(mycgds, "brca_metabric_all")
write.csv(a,'Data/brca_metabric.csv')


cancerTypeData <- read.table("Data/brca_metabric.csv",sep = ",",na.strings = c("","NA"),header = TRUE)
head(cancerTypeData)
#str(cancerTypeData)
cancerTypeData<- cancerTypeData[,c(2:30)]
#head(a)
desiredData<- (cancerTypeData)

head(desiredData)
#desiredData<- (cancerTypeData[,c(19,4,5,6,9,10,13,15,16,14,1,22,23,7,18,24,27,28)])
head(desiredData)
#####Selecting Deisred Columns from dataset #####
#########Clean Dataframe############
levels(desiredData$CELLULARITY) <- c(2,0,1)
levels(desiredData$CHEMOTHERAPY)<-c(0,1)
levels(desiredData$ER_IHC)<-c(0,1)
levels(desiredData$HER2_STATUS)<-c(0,1)
levels(desiredData$HORMONE_THERAPY)<-c(0,1)
levels(desiredData$INFERRED_MENOPAUSAL_STATE)<-c(1,0)
levels(desiredData$OS_STATUS)<-c(0,1)
levels(desiredData$ER_STATUS)<-c(0,1)
levels(desiredData$PR_STATUS)<-c(0,1)
levels(desiredData$LATERALITY)<-c(1,2)
levels(desiredData$RADIO_THERAPY)<-c(1,2)
#levels(desiredData$TumorStage)
desiredDataCleanFile<-(desiredData[complete.cases(desiredData), ])
head(desiredData)
#########Clean Dataframe############
##############Clustering######################
# hist(desiredDataCleanFile$AGE_AT_DIAGNOSIS)
# d<-density(desiredDataCleanFile$AGE_AT_DIAGNOSIS)
# plot(d)
# polygon(d, col="blue", border="black")
dataLessthan60<-subset(desiredDataCleanFile, AGE_AT_DIAGNOSIS <=62)
dataGreaterthan60<-subset(desiredDataCleanFile, AGE_AT_DIAGNOSIS > 62)
# head(dataLessthan60)
# nrow(dataGreaterthan60)
write.csv(desiredDataCleanFile,"DataWrangling/DataWrangling/Data/DesiredData.csv")
write.csv(dataLessthan60,"DataWrangling/DataWrangling/Data/dataLessthan60.csv")
write.csv(dataGreaterthan60,"DataWrangling/DataWrangling/Data/dataGreaterthan60.csv")
head(desiredData)
