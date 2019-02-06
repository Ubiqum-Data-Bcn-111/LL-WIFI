pacman:: p_load(readr,lubridate,dplyr,anytime,ggplot2,esquisse,caret,randomForest,e1071,class,C50,cowplot)
trainingData <- read_csv('UJIndoorLoc/trainingData.csv')
validData <- read_csv('UJIndoorLoc/validationData.csv')

#Deleting duplicates
trainingData <- unique(trainingData) # Getting rid of 637 rows
validData <- unique(validData)

#Creating DateTime variable
trainingData$TIMESTAMP <- anytime(trainingData$TIMESTAMP)
validData$TIMESTAMP <- anytime(validData$TIMESTAMP)

# Factoring necessary variables
trainingData$PHONEID <- as.factor(trainingData$PHONEID)
trainingData$USERID <- as.factor(trainingData$USERID)
trainingData$SPACEID <- as.factor(trainingData$SPACEID)
trainingData$RELATIVEPOSITION <- as.factor(trainingData$RELATIVEPOSITION)

validData$PHONEID <- as.factor(validData$PHONEID)
validData$USERID <- as.factor(validData$USERID)
validData$SPACEID <- as.factor(validData$SPACEID)
validData$RELATIVEPOSITION <- as.factor(validData$RELATIVEPOSITION)

#Re-factoring Floor n Building ID to start at 1 and not 0, and then combining them to create one Building and floor identifier
if (0 %in% trainingData$FLOOR == TRUE){
  trainingData$FLOOR <- as.numeric(recode(trainingData$FLOOR, '0' = '1', '1'= '2', '2'='3', '3'='4', '4'='5'))
}
if (0 %in% trainingData$BUILDINGID){
  trainingData$BUILDINGID <- as.numeric(recode(trainingData$BUILDINGID, '0'= '1', '1'= '2','2'='3'))
}
trainingData <- trainingData %>% 
  mutate(BUILD_FLOOR_ID = 10 * BUILDINGID + FLOOR)
trainingData$BUILDINGID <-as.factor(trainingData$BUILDINGID)
trainingData$FLOOR <-as.factor(trainingData$FLOOR)
trainingData$BUILD_FLOOR_ID <-as.factor(trainingData$BUILD_FLOOR_ID)


if (0 %in% validData$FLOOR == TRUE){
  validData$FLOOR <- as.numeric(recode(validData$FLOOR, '0' = '1', '1'= '2', '2'='3', '3'='4', '4'='5'))
}
if (0 %in% validData$BUILDINGID){
  validData$BUILDINGID <- as.numeric(recode(validData$BUILDINGID, '0'= '1', '1'= '2','2'='3'))
}
validData <- validData %>% 
  mutate(BUILD_FLOOR_ID = 10 * BUILDINGID + FLOOR)
validData$BUILDINGID <- as.factor(validData$BUILDINGID)
validData$FLOOR <- as.factor(validData$FLOOR)
validData$BUILD_FLOOR_ID <- as.factor(validData$BUILD_FLOOR_ID)

#GRAPH TO EXPLORE DATA----
#Graph frequency of each User
trainingData %>%
  group_by(USERID) %>%
  count() %>%
  ggplot(aes(x=USERID,y=n)) + geom_histogram(binwidth=5, stat = 'identity') + ggtitle('Frequency of USERID') + xlab('User ID') + ylab('Frequency')

#Graph frequency of each phone used
trainingData %>%
  group_by(PHONEID) %>%
  count() %>%
  ggplot(aes(x=PHONEID,y=n)) + geom_histogram(binwidth=5, stat = 'identity') + ggtitle('Frequency of PHONEID') + xlab('Phone ID') + ylab('Frequency')


#Graph by FLOOR & BUILDING
trainingData %>%
  group_by(BUILD_FLOOR_ID) %>%
  count()%>%
  arrange(n)%>%
  ggplot(aes(x=BUILD_FLOOR_ID,y=n)) + geom_bar(stat='identity') + ggtitle('Frequency of signals by Building & Floor') + ylab('#Signals')

#Comparing # of observations in BUILD 3 and Mean distance
Build3 <-trainingData %>%
  group_by(BUILD_FLOOR_ID) %>%
  count()%>%
  filter(BUILD_FLOOR_ID == 31 | BUILD_FLOOR_ID == 32 |BUILD_FLOOR_ID == 33 |BUILD_FLOOR_ID == 34 |BUILD_FLOOR_ID == 35)

ggplot(data = Build3) +
  aes(x = BUILD_FLOOR_ID, weight = n) +
  geom_bar(fill = "#0c4c8a") +
  labs(title = "# Observations per Floor in training set",
    y = "# obersevations",
    subtitle = "Building 3") +
  theme_minimal()


#Graph by longitudes/Latitude
#trainingData
ggplot(trainingData,aes(LATITUDE,LONGITUDE, fill=BUILDINGID, color=BUILDINGID)) + geom_point() + ggtitle('Mapping of Longitudinal & Latitudinal points') + xlab('Latitude') + ylab('Longitude') + scale_fill_manual(values=c('Building 1'= '1','Building 2' = '2','Building 3'='3'))
ggplot(trainingData,aes(LATITUDE,LONGITUDE, fill=FLOOR,color=FLOOR)) + geom_point() + ggtitle('Mapping of Longitudinal & Latitudinal points') + xlab('Latitude') + ylab('Longitude')
ggplot(trainingData,aes(LATITUDE,LONGITUDE, fill=BUILD_FLOOR_ID)) + geom_point() + ggtitle('Mapping of Longitudinal & Latitudinal points') + xlab('Latitude') + ylab('Longitude')
ggplot(data = NonWAPtrain) +
  aes(x = LONGITUDE, y = LATITUDE, color = BUILD_FLOOR_ID) +
  geom_point() +
  theme_minimal()

#validData
ggplot(validData,aes(LATITUDE,LONGITUDE, fill=BUILDINGID, color=BUILDINGID)) + geom_point() + ggtitle('Mapping of Longitudinal & Latitudinal points') + xlab('Latitude') + ylab('Longitude') + scale_fill_manual(values=c('Building 1'= '1','Building 2' = '2','Building 3'='3'))
ggplot(validData,aes(LATITUDE,LONGITUDE, fill=FLOOR,color=FLOOR)) + geom_point() + ggtitle('Mapping of Longitudinal & Latitudinal points') + xlab('Latitude') + ylab('Longitude')
ggplot(validData,aes(LATITUDE,LONGITUDE, fill=BUILD_FLOOR_ID)) + geom_point() + ggtitle('Mapping of Longitudinal & Latitudinal points') + xlab('Latitude') + ylab('Longitude')

#Check signal strength per phones
User_19 <- trainingData %>%
  filter(USERID == 19) #%>%
#Analysing Non WAP signals

trainingDataNONWAP <- trainingData[521:531]
validDataNONWAP <- validData[521:531]
NonWAP <- rbind(trainingDataNONWAP,validDataNONWAP)

tablephonebyBuild <- NonWAP %>%
  group_by(BUILDINGID, PHONEID, TrainorValid) %>%
  count()# %>%
  #group_by(BUILDINGID, TrainorValid) %>%
  #count
esquisser(tablephonebyBuild)
esquisser(trainingDataNONWAP)

ggplot(data = tablephonebyBuild) +
  aes(x = BUILDINGID, fill = TrainorValid, weight = nn) +
  geom_bar() +
  labs(title = "#Phones used by Building",
    y = "# Phones",
    subtitle = "Training set (0) & Validation Set (1)") +
  theme_minimal() +
  facet_wrap(vars(TrainorValid))

ggplot(data = tablephonebyBUIDFLOORID) +
  aes(x = BUILD_FLOOR_ID, fill = TrainorValid, weight = nn) +
  geom_bar() +
  labs(title = "#Phones used by Building/Floor",
       y = "# Phones",
       subtitle = "Training set (0) & Validation Set (1)") +
  theme_minimal() +
  facet_wrap(vars(TrainorValid))


#MANIPULATING VALUES WHICH ARE NOT THAT USEFUL----
# Changing 100 values to -105
trainingData[trainingData == 100] <- -105
validData[validData == 100] <- -105

#Changing all -90:-104 to -105 & Changing all 0:-45 to -45
#for training
subsetWAP <- trainingData[,1:520]
subsetWAP[subsetWAP <= -95] <- -105
trainingData[,1:520] <- subsetWAP

subsetWAP <- trainingData[,1:520]
subsetWAP[subsetWAP >= -45] <- -45
trainingData[,1:520] <- subsetWAP

#for validation set
subsetWAP <- validData[,1:520]
subsetWAP[subsetWAP <= -95] <- -105
validData[,1:520] <- subsetWAP

subsetWAP <- trainingData[,1:520]
subsetWAP[subsetWAP >= -45] <- -45
trainingData[,1:520] <- subsetWAP

#DELETING VARIABLES AND OBSERVATIONS WITH NO USE ----
#see how many observations have no interesting WAP signals whatsoever
trainingData %>%
  mutate(NoSignal=rowSums(trainingData[1:520])) %>%
  group_by(NoSignal) %>%
  count(NoSignal) %>%
  arrange(desc(NoSignal)) #--> 73 rows with no values --> DELETE or som

#Delete those rows in training 
trainingData <- trainingData %>%
  mutate(NoSignal=rowSums(trainingData[1:520])) %>%
  filter(NoSignal != -54600)
trainingData$NoSignal <- NULL

#Check which WAP signals don't turn on even once
NoSignalWAPtrain <- c()
for (i in 1:520){
  if(sum(trainingData[,i]) == (nrow(trainingData)*-105)){
    NoSignalWAPtrain <- c(NoSignalWAPtrain,i)
  }
}

NoSignalWAPValid <- c()
for (i in 1:520){
  if(sum(validData[,i]) == (nrow(validData)*-105)){
    NoSignalWAPValid <- c(NoSignalWAPValid,i)
  }
} 

Total_non_used_WAPS <- unique(c(NoSignalWAPtrain,NoSignalWAPValid))
trainingData <- trainingData[,-Total_non_used_WAPS]
validData <- validData[,-Total_non_used_WAPS]

#EXPLORING LINKS BETWEEN WAPS AND BUIL_FLOO_ID----
#in trainingData
Max_WAP_index <- as.data.frame(matrix(0, ncol = 5, nrow = 19937, dimnames=list(NULL,c('Max Value','WAP #','Longitude','Latitude','Buil_Floor_ID'))))
for (i in 1:nrow(trainingData)){
  Max_WAP_index[i,1] = max(trainingData[i,1:520])
  Max_WAP_index[i,2] = which.max(trainingData[i,1:520])
  Max_WAP_index[i,3] = trainingData$LONGITUDE[i]
  Max_WAP_index[i,4] = trainingData$LATITUDE[i]
  Max_WAP_index[i,5] = trainingData$BUILD_FLOOR_ID[i]
}

#in validData
Max_WAP_index_Val <- as.data.frame(matrix(0, ncol = 5, nrow = 19937, dimnames=list(NULL,c('Max Value','WAP #','Longitude','Latitude','Buil_Floor_ID'))))
for (i in 1:nrow(validData)){
  Max_WAP_index[i,1] = max(validData[i,1:520])
  Max_WAP_index[i,2] = which.max(validData[i,1:520])
  Max_WAP_index[i,3] = validData$LONGITUDE[i]
  Max_WAP_index[i,4] = validData$LATITUDE[i]
  Max_WAP_index[i,5] = validData$BUILD_FLOOR_ID[i]
}



#FINALISING DATASETS FOR RUNNING MODELS ----

ModData <-cbind(trainingData[,1:307],trainingData[,313]) # Getting rid of Variables I won't be needing for preditcions
ModDataOUT <- cbind(trainingData[,1:79],trainingData[,81:307],trainingData[,313])
validDataOUT <- cbind(validData[,1:79],validData[,81:313])

#Creating a datafram to store resutls for comparison
Accuracy_metrics_class <- data.frame(Model= character(),Accuracy=double(), Kappa=double()) #Creating a datafram to store resutls for comparison
Accuracy_metrics_reg <- data.frame(Model= character(),RMSE=numeric(), Rsquared=numeric(), MAE=numeric()) #Creating a datafram to store resutls for comparison

#Models to predict BUILD_FLOOR_ID----
set.seed(123)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = TRUE)
C_KNN_BF <- train(BUILD_FLOOR_ID ~ . - LONGITUDE - LATITUDE - BUILDINGID - FLOOR,
                data = ModData,
                trControl = trctrl,
                method = 'knn')

NC_KNN_BF <- knn(train=ModData,test=cbind(validData[1:307],validData[313]), cl=ModData$BUILD_FLOOR_ID,k=3)
  
NC_RF_BF <- randomForest(BUILD_FLOOR_ID ~ . -LONGITUDE -LATITUDE -BUILDINGID -FLOOR,
                         data = ModData, ntree=50, importance=TRUE, do.trace = TRUE)

NC_SVM_BF <- svm(BUILD_FLOOR_ID ~. -LONGITUDE -LATITUDE -BUILDINGID -FLOOR, data = ModData, do.trace =TRUE)

C50_BF <- C5.0(BUILD_FLOOR_ID ~ . -LONGITUDE -LATITUDE -BUILDINGID -FLOOR,
               data = ModData, ntree=50, importance=TRUE, do.trace = TRUE) 


#Predictions & Accuracy metrics for BUILD_FLOOR

postResample(NC_KNN_BF, validData$BUILD_FLOOR_ID)
Accuracy_metrics_class <- c('NC_KNN_BF',postResample(NC_KNN_BF,validData$BUILD_FLOOR_ID))

pred_NC_RF_BF <- predict(NC_RF_BF, validData)
postResample(pred_NC_RF_BF,validData$BUILD_FLOOR_ID)
Accuracy_metrics_class <- rbind(Accuracy_metrics_class,c('NC_RF_BF',postResample(pred_NC_RF_BF,validData$BUILD_FLOOR_ID)))

pred_SVM_BF <- predict(NC_SVM_BF, validData)
postResample(pred_SVM_BF, validData$BUILD_FLOOR_ID)
Accuracy_metrics_class <- rbind(Accuracy_metrics_class,c('SVM_BF',postResample(pred_SVM_BF,validData$BUILD_FLOOR_ID)))

pred_C50_BF <- predict(C50_BF, validData)
postResample(pred_C50_BF,validData$BUILD_FLOOR_ID)
Accuracy_metrics_class <- rbind(Accuracy_metrics_class,c('C50_BF',postResample(pred_C50_BF,validData$BUILD_FLOOR_ID)))

#CONFUSION MATRICES of best models for BUILD_FLOOR_ID
confusionMatrix(NC_KNN_BF, validData$BUILD_FLOOR_ID)
confusionMatrix(pred_NC_RF_BF, validData$BUILD_FLOOR_ID)
confusionMatrix(pred_SVM_BF, validData$BUILD_FLOOR_ID)
confusionMatrix(pred_C50_BF, validData$BUILD_FLOOR_ID)


#MODELS FOR LONGITUTE ----
#WITH B&F
#Model creation
set.seed(123)
KNN_LG_R <- knnreg(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData, k=5)

RF_LG <- randomForest(LONGITUDE ~ . -LATITUDE -BUILDINGID -FLOOR,
                      data = ModData, ntree=100, importance=TRUE, do.trace = TRUE)

SVM_LG <- svm(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData, do.trace =TRUE)

fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 4)
GBM_LG <- train(LONGITUDE ~ .-FLOOR -LATITUDE -BUILDINGID, data = ModData, method = "gbm", trControl = fitControl,verbose = FALSE)

LM_LG <- lm(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData)

RT_LG_OUT <- randomForest(LONGITUDE ~ . -LATITUDE -BUILDINGID -FLOOR,
                          data = ModDataOUT, ntree=100, importance=TRUE, do.trace = TRUE)

KNN_LG_OUT <- knnreg(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModDataOUT, k=5)

#Predictions
pre_KNN_LG_R <- predict(KNN_LG_R, validData)
postResample(pre_KNN_LG_R, validData$LONGITUDE)
Accuracy_metrics_reg <- c('KNN_LG_R',postResample(pre_KNN_LG_R,validData$LONGITUDE))

pred_RF_LG <- predict(RF_LG,validData)
postResample(pred_RF_LG, validData$LONGITUDE)
Accuracy_metrics_reg <- rbind(Accuracy_metrics_reg,c('RF_LG',postResample(pred_RF_LG,validData$LONGITUDE)))

pred_SVM_LG <- predict(SVM_LG, validData)
postResample(pred_SVM_LG,validData$LONGITUDE)
Accuracy_metrics_reg <- rbind(Accuracy_metrics_reg,c('SVM_LG',postResample(pred_SVM_LG,validData$LONGITUDE)))

pred_GBM_LG <- predict(GBM_LG, validData)
postResample(pred_GBM_LG, validData$LONGITUDE)
Accuracy_metrics_reg <- rbind(Accuracy_metrics_reg,c('GBM_LG',postResample(pred_GBM_LG,validData$LONGITUDE)))

pred_LM_LG <- predict(LM_LG, validData)
postResample(pred_LM_LG, validData$LONGITUDE)
Accuracy_metrics_reg <- rbind(Accuracy_metrics_reg,c('LM_LG',postResample(pred_LM_LG,validData$LONGITUDE)))

pred_RF_LG_OUT <- predict(RT_LG_OUT,validDataOUT)
postResample(pred_RF_LG_OUT, validDataOUT$LONGITUDE)
Accuracy_metrics_reg <- rbind(Accuracy_metrics_reg,c('RF_LG_OUT',postResample(pred_RF_LG_OUT,validData$LONGITUDE)))

pred_KNN_LG_OUT <- predict(KNN_LG_OUT, validDataOUT)
postResample(pred_KNN_LG_OUT, validDataOUT$LONGITUDE)
Accuracy_metrics_reg <- rbind(Accuracy_metrics_reg,c('KNN_LG_OUT',postResample(pred_KNN_LG_OUT,validDataOUT$LONGITUDE)))

#MODELS FOR LATTITUDE ----
#With B&F
RF_LT <- randomForest(LATITUDE ~ . -LONGITUDE -BUILDINGID -FLOOR,
                      data = ModData, ntree=100, importance=TRUE, do.trace = TRUE)


SVM_LT <- svm(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData, do.trace =TRUE)


KNN_LT <- knnreg(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData, k=5)

LM_LT <- lm(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData)

RF_LT_OUT <- randomForest(LATITUDE ~ . -LONGITUDE -BUILDINGID -FLOOR,
                          data = ModDataOUT, ntree=100, importance=TRUE, do.trace = TRUE)

KNN_LT_OUT <- knnreg(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModDataOUT, k=5)

#Predictions
pred_RF_LT <- predict(RF_LT, validData)
postResample(pred_RF_LT, validData$LATITUDE)
Accuracy_metrics_reg <- rbind(Accuracy_metrics_reg,c('RF_LT',postResample(pred_RF_LT,validData$LATITUDE)))

pred_KNN_LT <- predict(KNN_LT, validData)
postResample(pred_KNN_LT, validData$LATITUDE)
Accuracy_metrics_reg <- rbind(Accuracy_metrics_reg,c('KNN_LT',postResample(pred_KNN_LT,validData$LATITUDE)))

pred_SVM_LT <- predict(SVM_LT, validData)
postResample(pred_SVM_LT, validData$LATITUDE)
Accuracy_metrics_reg <- rbind(Accuracy_metrics_reg,c('SVM_LT',postResample(pred_SVM_LT,validData$LATITUDE)))

pred_LM_LT <- predict(LM_LT, validData)
postResample(pred_LM_LT, validData$LATITUDE)
Accuracy_metrics_reg <- rbind(Accuracy_metrics_reg,c('LM_LT',postResample(pred_LM_LT,validData$LONGITUDE)))

pred_RF_LT_OUT <- predict(RF_LT_OUT, validData)
postResample(pred_RF_LT_OUT, validData$LATITUDE)
Accuracy_metrics_reg <- rbind(Accuracy_metrics_reg,c('RF_LT_OUT',postResample(pred_RF_LT_OUT,validData$LATITUDE)))

pred_KNN_LT_OUT <- predict(KNN_LT_OUT, validDataOUT)
postResample(pred_KNN_LT_OUT, validDataOUT$LATITUDE)
Accuracy_metrics_reg <- rbind(Accuracy_metrics_reg,c('KNN_LT_OUT',postResample(pred_KNN_LT_OUT,validDataOUT$LATITUDE)))

#PLOT RESULS vs REAL VALUES -----
pred_vs_real <- data.frame(LT_pred = pred_RF_LT, LG_pred = pred_RF_LG, LT_real = validData$LATITUDE, LG_real = validData$LONGITUDE, USERID = validData$USERID, PHONEID=validData$PHONEID, BUILD_FLOOR_ID = validData$BUILD_FLOOR_ID, BUILDINGID = validData$BUILDINGID, FLOOR = validData$FLOOR)
pred_LG_LT <- data.frame(LT = pred_RF_LT, LG = pred_RF_LG, pred_or_real  = 1,USERID = validData$USERID, PHONEID=validData$PHONEID, BUILD_FLOOR_ID = validData$BUILD_FLOOR_ID, BUILDINGID = validData$BUILDINGID, FLOOR = validData$FLOOR)
real_LG_LT <- data.frame(LT = validData$LATITUDE, LG = validData$LONGITUDE, pred_or_real = 0 ,USERID = validData$USERID, PHONEID=validData$PHONEID, BUILD_FLOOR_ID = validData$BUILD_FLOOR_ID, BUILDINGID = validData$BUILDINGID, FLOOR = validData$FLOOR)

pred_vs_realB1 <- pred_vs_real2 %>%
  filter(BUILDINGID == 1)

pred_vs_realB1 <- pred_vs_real2 %>%
  filter(BUILDINGID == 2)

pred_vs_realB1 <- pred_vs_real2 %>%
  filter(BUILDINGID == 3)

esquisser(pred_vs_real2)
ggplot(data = pred_vs_real2) +
  aes(x = LT, y = LG, color = pred_or_real) +
  geom_point(size=0.5) +
  labs(title = "Predicted & Actual values for Latitude & Longitude",
    x = "Latitude",
    y = "Longitude") +
  theme_minimal() +
  facet_wrap(vars(pred_or_real))

ggplot(data = pred_vs_real2) +
  aes(x = LT, y = LG, color = pred_or_real) +
  geom_point(size=0.5) +
  labs(title = "Longitude and Latitude Predictions vs Actual values",
       x = "Latitude",
       y = "Longitude") +
  theme_minimal() +
  theme(legend.position = 'none')


pred_vs_realB1 <- pred_vs_real2 %>%
  filter(BUILDINGID == 1)
esquisser(pred_vs_realB1)

ggplot(data = pred_vs_realB1) +
  aes(x = LT, y = LG, color = pred_or_real) +
  geom_point(size=0.5) +
  labs(title = "Longitude & Latitude Predictions vs Actual values in Building 1",
    x = "Latitude",
    y = "Longitude") +
  theme_minimal() +
  facet_wrap(vars(FLOOR))


pred_vs_realB2 <- pred_vs_real2 %>%
  filter(BUILDINGID == 2)
esquisser(pred_vs_realB2)

ggplot(data = pred_vs_realB2) +
  aes(x = LT, y = LG, color = pred_or_real) +
  geom_point(size=0.5) +
  labs(title = "Predictions vs Actual Values for Latitude & Longitude for Building 2",
    x = "Latitude",
    y = "Longitude") +
  theme_minimal() +
  facet_wrap(vars(FLOOR))


pred_vs_realB3 <- pred_vs_real2 %>%
  filter(BUILDINGID == 3)
esquisser(pred_vs_realB3)


ggplot(data = pred_vs_realB3) +
  aes(x = LT, y = LG, color = pred_or_real) +
  geom_point(size=0.5) +
  labs(title = "Predictions vs Actual Values for Latitude & Longitude for Building 3",
       x = "Latitude",
       y = "Longitude") +
  theme_minimal() +
  facet_wrap(vars(FLOOR))



#ERROR ANALYSIS----
# CReating errors dataframe
errordf <- data.frame(LT_errors = as.data.frame(pred_RF_LT - validData$LATITUDE), LG_errors = as.data.frame(pred_RF_LG - validData$LONGITUDE), USERID = validData$USERID, PHONEID=validData$PHONEID, BUILD_FLOOR_ID = validData$BUILD_FLOOR_ID, BUILDINGID = validData$BUILDINGID, FLOOR = validData$FLOOR)
colnames(errordf) <- c('LT_errors','LG_errors','USRID', 'PHONEID', 'BUILD_FLOOR_ID','BUILDINGID','FLOOR')
#Shows density plot of Longitude errors

ggplot(data = errordf) +
  aes(x = LG_errors) +
  geom_density(adjust = 1, fill = "#0c4c8a") +
  labs(title='Density plot of Longitudinal errors',
       x = 'Longitude',
       y ='density') +
  theme_minimal()

#Shows density plot of Latitude erros
ggplot(data = errordf) +
  aes(x = LT_errors) +
  geom_density(adjust = 1, fill = "#0c4c8a") +
  labs(title='Density plot of Latitudinal errors',
       x = 'Latitude',
       y ='density') +
  theme_minimal()

esquisser(errordf)
#shows frequency of errors by PHONE

errordf_20_20 <- errordf %>%
  filter((LT_errors > 20 | LT_errors < -20) & (LG_errors >2 | LG_errors < -20))

errordf_20 <- errordf %>%
  filter(LT_errors > 20 | LT_errors < -20 | LG_errors >20 | LG_errors < -20)

errordf_10 <- errordf %>%
  filter(LT_errors > 10 | LT_errors < -10 | LG_errors >10 | LG_errors < -10)

errordf_8 <- errordf %>%
  filter(LT_errors > 8 | LT_errors < -8 | LG_errors >8 | LG_errors < -8)

esquisser(errordf_20)
esquisser(errordf_10)
esquisser(errordf_8)

#Following grpahs show where the LAtitude and Longitue errors come from in respect
str(errordf[1:2])
errordf <- errordf %>%
  mutate(distance = sqrt(abs(LT_errors * LT_errors)+abs(LG_errors * LG_errors))) %>%
  group_by(BUILD_FLOOR_ID) %>%
  summarise(avg=mean(distance))

errordfB3 <- errordf %>%
  filter(BUILD_FLOOR_ID == 31 | BUILD_FLOOR_ID == 32 |BUILD_FLOOR_ID == 33 |BUILD_FLOOR_ID == 34 |BUILD_FLOOR_ID == 35)


errordf <- cbind(errordf, Weights$n)

ditaance <- errordf %>%
  mutate(Wdist = avg * Weights$n) 


esquisser(errordf)

#AVERAGE DISTANCE PER BUILDING/FLOOR

ggplot(data = errordf) +
  aes(x = BUILD_FLOOR_ID, weight = avg) +
  geom_bar(fill = "#0c4c8a") +
  labs(title = "Average distance between Predictions and Actual positions per Building/Floor ID",
    y = "Average distance") +
  theme_minimal()

ggplot(data = errordfB3) +
  aes(x = BUILD_FLOOR_ID, weight = avg) +
  geom_bar(fill = "#0c4c8a") +
  labs(title = "Average distance between Predictions and Actual positions per Floor",
       subtitle = 'Building 3',
       y = "Average distance") +
  theme_minimal()

error_dist_8 <- errordf %>%
  filter(distance > 10 & distance < 60) 

error_dist_8_B1 <- error_dist_8 %>%
  filter(BUILDINGID ==1 )

error_dist_8_B2 <- error_dist_8 %>%
  filter(BUILDINGID ==2 )

error_dist_8_B3 <- error_dist_8 %>%
  filter(BUILDINGID ==3 )

esquisser(error_dist_8)
ggplot(data = error_dist_8_B1) +
  aes(x = distance) +
  geom_histogram(bins = 30, fill = "#0c4c8a") +
  theme_minimal() +
  facet_wrap(vars(FLOOR))

esquisser(error_dist_8_B2)
ggplot(data = error_dist_8_B2) +
  aes(x = distance) +
  geom_histogram(bins = 30, fill = "#0c4c8a") +
  theme_minimal() +
  facet_wrap(vars(FLOOR))

ggplot(data = error_dist_8_B3) +
  aes(x = distance) +
  geom_histogram(bins = 30, fill = "#0c4c8a") +
  theme_minimal() +
  facet_wrap(vars(FLOOR))

error_dist_11 <- errordf %>%
  filter(distance > 11)

pred_vs_real2 <- rbind(pred_LG_LT,real_LG_LT)
#Creating a model for each BUIL_FLOOR_ID----
Weights <- validData %>%
  group_by(BUILD_FLOOR_ID) %>%
  summarise(n=n())

Weights$n <- Weights$n / 1111

#Creating different datasets for training ----
ModData11 <- ModData %>%
  filter(BUILD_FLOOR_ID == '11')

ModData12 <- ModData %>%
  filter(BUILD_FLOOR_ID == '12')

ModData13 <- ModData %>%
  filter(BUILD_FLOOR_ID == '13')

ModData14 <- ModData %>%
  filter(BUILD_FLOOR_ID == '14')

ModData21 <- ModData %>%
  filter(BUILD_FLOOR_ID == '21')

ModData22 <- ModData %>%
  filter(BUILD_FLOOR_ID == '22')

ModData23 <- ModData %>%
  filter(BUILD_FLOOR_ID == '23')

ModData24 <- ModData %>%
  filter(BUILD_FLOOR_ID == '24')

ModData31 <- ModData %>%
  filter(BUILD_FLOOR_ID == '31')

ModData32 <- ModData %>%
  filter(BUILD_FLOOR_ID == '32')

ModData33 <- ModData %>%
  filter(BUILD_FLOOR_ID == '33')

ModData34 <- ModData %>%
  filter(BUILD_FLOOR_ID == '34')

ModData35 <- ModData %>%
  filter(BUILD_FLOOR_ID == '35')

#Creating different datasets for validData
validData11 <- validData %>%
  filter(BUILD_FLOOR_ID == '11')

validData12 <- validData %>%
  filter(BUILD_FLOOR_ID == '12')

validData13 <- validData %>%
  filter(BUILD_FLOOR_ID == '13')

validData14 <- validData %>%
  filter(BUILD_FLOOR_ID == '14')

validData21 <- validData  %>%
  filter(BUILD_FLOOR_ID == '21')

validData22 <- validData %>%
  filter(BUILD_FLOOR_ID == '22')

validData23 <- validData %>%
  filter(BUILD_FLOOR_ID == '23')

validData24 <- validData %>%
  filter(BUILD_FLOOR_ID == '24')

validData31 <- validData %>%
  filter(BUILD_FLOOR_ID == '31')

validData32 <- validData %>%
  filter(BUILD_FLOOR_ID == '32')

validData33 <- validData %>%
  filter(BUILD_FLOOR_ID == '33')

validData34 <- validData %>%
  filter(BUILD_FLOOR_ID == '34')

validData35 <- validData %>%
  filter(BUILD_FLOOR_ID == '35')

#Modeling LATITUDE with KNN ----
KNN_LT_11 <- knnreg(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData11, k=5)
KNN_LT_12 <- knnreg(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData12, k=5)
KNN_LT_13 <- knnreg(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData13, k=5)
KNN_LT_14 <- knnreg(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData14, k=5)

KNN_LT_21 <- knnreg(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData21, k=5)
KNN_LT_22 <- knnreg(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData22, k=5)
KNN_LT_23 <- knnreg(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData23, k=5)
KNN_LT_24 <- knnreg(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData24, k=5)

KNN_LT_31 <- knnreg(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData31, k=5)
KNN_LT_32 <- knnreg(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData32, k=5)
KNN_LT_33 <- knnreg(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData33, k=5)
KNN_LT_34 <- knnreg(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData34, k=5)
KNN_LT_35 <- knnreg(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData35, k=5)

pred_KNN_LT_11 <- predict(KNN_LT_11, validData11)
pred_KNN_LT_12 <- predict(KNN_LT_12, validData12)
pred_KNN_LT_13 <- predict(KNN_LT_13, validData13)
pred_KNN_LT_14 <- predict(KNN_LT_14, validData14)

pred_KNN_LT_21 <- predict(KNN_LT_21, validData21)
pred_KNN_LT_22 <- predict(KNN_LT_22, validData22)
pred_KNN_LT_23 <- predict(KNN_LT_23, validData23)
pred_KNN_LT_24 <- predict(KNN_LT_24, validData24)

pred_KNN_LT_31 <- predict(KNN_LT_31, validData31)
pred_KNN_LT_32 <- predict(KNN_LT_32, validData32)
pred_KNN_LT_33 <- predict(KNN_LT_33, validData33)
pred_KNN_LT_34 <- predict(KNN_LT_34, validData34)
pred_KNN_LT_35 <- predict(KNN_LT_35, validData35)
                          
postResample(pred_KNN_LT_11,validData11$LATITUDE)
postResample(pred_KNN_LT_12,validData12$LATITUDE)
postResample(pred_KNN_LT_13,validData13$LATITUDE)
postResample(pred_KNN_LT_14,validData14$LATITUDE)

postResample(pred_KNN_LT_21,validData21$LATITUDE)
postResample(pred_KNN_LT_22,validData22$LATITUDE)
postResample(pred_KNN_LT_23,validData23$LATITUDE)
postResample(pred_KNN_LT_24,validData24$LATITUDE)

postResample(pred_KNN_LT_31,validData31$LATITUDE)
postResample(pred_KNN_LT_32,validData32$LATITUDE)
postResample(pred_KNN_LT_33,validData33$LATITUDE)
postResample(pred_KNN_LT_34,validData34$LATITUDE)
postResample(pred_KNN_LT_35,validData35$LATITUDE)

Accuracy_metrics_reg_BF <- data.frame(RMSE=as.double(), Rsquared=as.double(), MAE=as.double())

Accuracy_metrics_reg_BF <- c(as.double(postResample(pred_KNN_LT_11,validData11$LATITUDE)))
Accuracy_metrics_reg_BF <- rbind(Accuracy_metrics_reg_BF,c(as.double(postResample(pred_KNN_LT_12,validData12$LATITUDE))))
Accuracy_metrics_reg_BF <- rbind(Accuracy_metrics_reg_BF,c(as.double(postResample(pred_KNN_LT_13,validData13$LATITUDE))))
Accuracy_metrics_reg_BF <- rbind(Accuracy_metrics_reg_BF,c(as.double(postResample(pred_KNN_LT_14,validData14$LATITUDE))))

Accuracy_metrics_reg_BF <- rbind(Accuracy_metrics_reg_BF,c(as.double(postResample(pred_KNN_LT_21,validData21$LATITUDE))))
Accuracy_metrics_reg_BF <- rbind(Accuracy_metrics_reg_BF,c(as.double(postResample(pred_KNN_LT_22,validData22$LATITUDE))))
Accuracy_metrics_reg_BF <- rbind(Accuracy_metrics_reg_BF,c(as.double(postResample(pred_KNN_LT_23,validData23$LATITUDE))))
Accuracy_metrics_reg_BF <- rbind(Accuracy_metrics_reg_BF,c(as.double(postResample(pred_KNN_LT_24,validData24$LATITUDE))))

Accuracy_metrics_reg_BF <- rbind(Accuracy_metrics_reg_BF,c(as.double(postResample(pred_KNN_LT_31,validData31$LATITUDE))))
Accuracy_metrics_reg_BF <- rbind(Accuracy_metrics_reg_BF,c(as.double(postResample(pred_KNN_LT_32,validData32$LATITUDE))))
Accuracy_metrics_reg_BF <- rbind(Accuracy_metrics_reg_BF,c(as.double(postResample(pred_KNN_LT_33,validData33$LATITUDE))))
Accuracy_metrics_reg_BF <- rbind(Accuracy_metrics_reg_BF,c(as.double(postResample(pred_KNN_LT_34,validData34$LATITUDE))))
Accuracy_metrics_reg_BF <- rbind(Accuracy_metrics_reg_BF,c(as.double(postResample(pred_KNN_LT_35,validData35$LATITUDE))))

Accuracy_metrics_reg_BF <- as.data.frame(Accuracy_metrics_reg_BF)

Accuracy_metrics_reg_BF[,2] <- as.double(Accuracy_metrics_reg_BF[,2])
Accuracy_metrics_reg_BF[,3] <- as.double(Accuracy_metrics_reg_BF[,3])
Accuracy_metrics_reg_BF[,4] <- as.double(Accuracy_metrics_reg_BF[,4])
Accuracy_metrics_reg_BF$WRMSE <- 1
Accuracy_metrics_reg_BF$WSQUARED <- 1
Accuracy_metrics_reg_BF$WMAE <- 1



for (i in 1:nrow(Accuracy_metrics_reg_BF)){
  Accuracy_metrics_reg_BF[i,4] <- Accuracy_metrics_reg_BF[i,1] * Weights[i,2]
}


for (i in 1:nrow(Accuracy_metrics_reg_BF)){
  Accuracy_metrics_reg_BF[i,5] <- Accuracy_metrics_reg_BF[i,2] * Weights[i,2]
}


for (i in 1:nrow(Accuracy_metrics_reg_BF)){
  Accuracy_metrics_reg_BF[i,6] <- Accuracy_metrics_reg_BF[i,3] * Weights[i,2]
}

RMSEfin <- sum(Accuracy_metrics_reg_BF[,4])
Rsquaredfin <- sum(Accuracy_metrics_reg_BF[,5])
MAEfin <- sum(Accuracy_metrics_reg_BF[,6])
KNN_LT_byBF <- c(RMSEfin,Rsquaredfin,MAEfin)

Accuracy_metrics_reg <- rbind(Accuracy_metrics_reg, c('KNN_LT_byBF', KNN_LT_byBF))

#MODELIING LONGITUDE WITH KNN----
KNN_LG_11 <- knnreg(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData11, k=5)
KNN_LG_12 <- knnreg(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData12, k=5)
KNN_LG_13 <- knnreg(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData13, k=5)
KNN_LG_14 <- knnreg(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData14, k=5)

KNN_LG_21 <- knnreg(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData21, k=5)
KNN_LG_22 <- knnreg(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData22, k=5)
KNN_LG_23 <- knnreg(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData23, k=5)
KNN_LG_24 <- knnreg(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData24, k=5)

KNN_LG_31 <- knnreg(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData31, k=5)
KNN_LG_32 <- knnreg(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData32, k=5)
KNN_LG_33 <- knnreg(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData33, k=5)
KNN_LG_34 <- knnreg(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData34, k=5)
KNN_LG_35 <- knnreg(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData35, k=5)

pred_KNN_LG_11 <- predict(KNN_LG_11, validData11)
pred_KNN_LG_12 <- predict(KNN_LG_12, validData12)
pred_KNN_LG_13 <- predict(KNN_LG_13, validData13)
pred_KNN_LG_14 <- predict(KNN_LG_14, validData14)

pred_KNN_LG_21 <- predict(KNN_LG_21, validData21)
pred_KNN_LG_22 <- predict(KNN_LG_22, validData22)
pred_KNN_LG_23 <- predict(KNN_LG_23, validData23)
pred_KNN_LG_24 <- predict(KNN_LG_24, validData24)

pred_KNN_LG_31 <- predict(KNN_LG_31, validData31)
pred_KNN_LG_32 <- predict(KNN_LG_32, validData32)
pred_KNN_LG_33 <- predict(KNN_LG_33, validData33)
pred_KNN_LG_34 <- predict(KNN_LG_34, validData34)
pred_KNN_LG_35 <- predict(KNN_LG_35, validData35)

postResample(pred_KNN_LG_11,validData11$LONGITUDE)
postResample(pred_KNN_LG_12,validData12$LONGITUDE)
postResample(pred_KNN_LG_13,validData13$LONGITUDE)
postResample(pred_KNN_LG_14,validData14$LONGITUDE)

postResample(pred_KNN_LG_21,validData21$LONGITUDE)
postResample(pred_KNN_LG_22,validData22$LONGITUDE)
postResample(pred_KNN_LG_23,validData23$LONGITUDE)
postResample(pred_KNN_LG_24,validData24$LONGITUDE)

postResample(pred_KNN_LG_31,validData31$LONGITUDE)
postResample(pred_KNN_LG_32,validData32$LONGITUDE)
postResample(pred_KNN_LG_33,validData33$LONGITUDE)
postResample(pred_KNN_LG_34,validData34$LONGITUDE)
postResample(pred_KNN_LG_35,validData35$LONGITUDE)

Accuracy_metrics_reg_BF22 <- data.frame(RMSE=as.double(), Rsquared=as.double(), MAE=as.double())

Accuracy_metrics_reg_BF2 <- c(as.double(postResample(pred_KNN_LG_11,validData11$LONGITUDE)))
Accuracy_metrics_reg_BF2 <- rbind(Accuracy_metrics_reg_BF2,c(as.double(postResample(pred_KNN_LG_12,validData12$LONGITUDE))))
Accuracy_metrics_reg_BF2 <- rbind(Accuracy_metrics_reg_BF2,c(as.double(postResample(pred_KNN_LG_13,validData13$LONGITUDE))))
Accuracy_metrics_reg_BF2 <- rbind(Accuracy_metrics_reg_BF2,c(as.double(postResample(pred_KNN_LG_14,validData14$LONGITUDE))))

Accuracy_metrics_reg_BF2 <- rbind(Accuracy_metrics_reg_BF2,c(as.double(postResample(pred_KNN_LG_21,validData21$LONGITUDE))))
Accuracy_metrics_reg_BF2 <- rbind(Accuracy_metrics_reg_BF2,c(as.double(postResample(pred_KNN_LG_22,validData22$LONGITUDE))))
Accuracy_metrics_reg_BF2 <- rbind(Accuracy_metrics_reg_BF2,c(as.double(postResample(pred_KNN_LG_23,validData23$LONGITUDE))))
Accuracy_metrics_reg_BF2 <- rbind(Accuracy_metrics_reg_BF2,c(as.double(postResample(pred_KNN_LG_24,validData24$LONGITUDE))))

Accuracy_metrics_reg_BF2 <- rbind(Accuracy_metrics_reg_BF2,c(as.double(postResample(pred_KNN_LG_31,validData31$LONGITUDE))))
Accuracy_metrics_reg_BF2 <- rbind(Accuracy_metrics_reg_BF2,c(as.double(postResample(pred_KNN_LG_32,validData32$LONGITUDE))))
Accuracy_metrics_reg_BF2 <- rbind(Accuracy_metrics_reg_BF2,c(as.double(postResample(pred_KNN_LG_33,validData33$LONGITUDE))))
Accuracy_metrics_reg_BF2 <- rbind(Accuracy_metrics_reg_BF2,c(as.double(postResample(pred_KNN_LG_34,validData34$LONGITUDE))))
Accuracy_metrics_reg_BF2 <- rbind(Accuracy_metrics_reg_BF2,c(as.double(postResample(pred_KNN_LG_35,validData35$LONGITUDE))))

Accuracy_metrics_reg_BF <- as.data.frame(Accuracy_metrics_reg_BF)

Accuracy_metrics_reg_BF[,1] <- as.double(Accuracy_metrics_reg_BF[,1])
Accuracy_metrics_reg_BF[,2] <- as.double(Accuracy_metrics_reg_BF[,2])
Accuracy_metrics_reg_BF[,3] <- as.double(Accuracy_metrics_reg_BF[,3])
Accuracy_metrics_reg_BF$WRMSE <- 1
Accuracy_metrics_reg_BF$WSQUARED <- 1
Accuracy_metrics_reg_BF$WMAE <- 1



for (i in 1:nrow(Accuracy_metrics_reg_BF)){
  Accuracy_metrics_reg_BF[i,4] <- Accuracy_metrics_reg_BF[i,1] * Weights[i,2]
}


for (i in 1:nrow(Accuracy_metrics_reg_BF)){
  Accuracy_metrics_reg_BF[i,5] <- Accuracy_metrics_reg_BF[i,2] * Weights[i,2]
}


for (i in 1:nrow(Accuracy_metrics_reg_BF)){
  Accuracy_metrics_reg_BF[i,6] <- Accuracy_metrics_reg_BF[i,3] * Weights[i,2]
}

RMSEfin <- sum(Accuracy_metrics_reg_BF[,4])
Rsquaredfin <- sum(Accuracy_metrics_reg_BF[,5])
MAEfin <- sum(Accuracy_metrics_reg_BF[,6])
KNN_LG_byBF <- c(RMSEfin,Rsquaredfin,MAEfin)

Accuracy_metrics_reg <- rbind(Accuracy_metrics_reg, c('KNN_LG_byBF', KNN_LG_byBF))

#PREDICTING LG BY RANDOM FOREST----

KNN_LG_11 <- randomForest(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData11, ntree=100)
KNN_LG_12 <- randomForest(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData12, ntree=100)
KNN_LG_13 <- randomForest(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData13, ntree=100)
KNN_LG_14 <- randomForest(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData14, ntree=100)

KNN_LG_21 <- randomForest(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData21, ntree=100)
KNN_LG_22 <- randomForest(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData22, ntree=100)
KNN_LG_23 <- randomForest(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData23, ntree=100)
KNN_LG_24 <- randomForest(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData24, ntree=100)

KNN_LG_31 <- randomForest(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData31, ntree=100)
KNN_LG_32 <- randomForest(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData32, ntree=100)
KNN_LG_33 <- randomForest(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData33, ntree=100)
KNN_LG_34 <- randomForest(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData34, ntree=100)
KNN_LG_35 <- randomForest(LONGITUDE ~. -FLOOR -LATITUDE -BUILDINGID, data = ModData35, ntree=100)

pred_KNN_LG_11 <- predict(KNN_LG_11, validData11)
pred_KNN_LG_12 <- predict(KNN_LG_12, validData12)
pred_KNN_LG_13 <- predict(KNN_LG_13, validData13)
pred_KNN_LG_14 <- predict(KNN_LG_14, validData14)

pred_KNN_LG_21 <- predict(KNN_LG_21, validData21)
pred_KNN_LG_22 <- predict(KNN_LG_22, validData22)
pred_KNN_LG_23 <- predict(KNN_LG_23, validData23)
pred_KNN_LG_24 <- predict(KNN_LG_24, validData24)

pred_KNN_LG_31 <- predict(KNN_LG_31, validData31)
pred_KNN_LG_32 <- predict(KNN_LG_32, validData32)
pred_KNN_LG_33 <- predict(KNN_LG_33, validData33)
pred_KNN_LG_34 <- predict(KNN_LG_34, validData34)
pred_KNN_LG_35 <- predict(KNN_LG_35, validData35)

postResample(pred_KNN_LG_11,validData11$LONGITUDE)
postResample(pred_KNN_LG_12,validData12$LONGITUDE)
postResample(pred_KNN_LG_13,validData13$LONGITUDE)
postResample(pred_KNN_LG_14,validData14$LONGITUDE)

postResample(pred_KNN_LG_21,validData21$LONGITUDE)
postResample(pred_KNN_LG_22,validData22$LONGITUDE)
postResample(pred_KNN_LG_23,validData23$LONGITUDE)
postResample(pred_KNN_LG_24,validData24$LONGITUDE)

postResample(pred_KNN_LG_31,validData31$LONGITUDE)
postResample(pred_KNN_LG_32,validData32$LONGITUDE)
postResample(pred_KNN_LG_33,validData33$LONGITUDE)
postResample(pred_KNN_LG_34,validData34$LONGITUDE)
postResample(pred_KNN_LG_35,validData35$LONGITUDE)

Accuracy_metrics_reg_BF3 <- data.frame(RMSE=as.double(), Rsquared=as.double(), MAE=as.double())

Accuracy_metrics_reg_BF3 <- c(as.double(postResample(pred_KNN_LG_11,validData11$LONGITUDE)))
Accuracy_metrics_reg_BF3 <- rbind(Accuracy_metrics_reg_BF3,c(as.double(postResample(pred_KNN_LG_12,validData12$LONGITUDE))))
Accuracy_metrics_reg_BF3 <- rbind(Accuracy_metrics_reg_BF3,c(as.double(postResample(pred_KNN_LG_13,validData13$LONGITUDE))))
Accuracy_metrics_reg_BF3 <- rbind(Accuracy_metrics_reg_BF3,c(as.double(postResample(pred_KNN_LG_14,validData14$LONGITUDE))))

Accuracy_metrics_reg_BF3 <- rbind(Accuracy_metrics_reg_BF3,c(as.double(postResample(pred_KNN_LG_21,validData21$LONGITUDE))))
Accuracy_metrics_reg_BF3 <- rbind(Accuracy_metrics_reg_BF3,c(as.double(postResample(pred_KNN_LG_22,validData22$LONGITUDE))))
Accuracy_metrics_reg_BF3 <- rbind(Accuracy_metrics_reg_BF3,c(as.double(postResample(pred_KNN_LG_23,validData23$LONGITUDE))))
Accuracy_metrics_reg_BF3 <- rbind(Accuracy_metrics_reg_BF3,c(as.double(postResample(pred_KNN_LG_24,validData24$LONGITUDE))))

Accuracy_metrics_reg_BF3 <- rbind(Accuracy_metrics_reg_BF3,c(as.double(postResample(pred_KNN_LG_31,validData31$LONGITUDE))))
Accuracy_metrics_reg_BF3 <- rbind(Accuracy_metrics_reg_BF3,c(as.double(postResample(pred_KNN_LG_32,validData32$LONGITUDE))))
Accuracy_metrics_reg_BF3 <- rbind(Accuracy_metrics_reg_BF3,c(as.double(postResample(pred_KNN_LG_33,validData33$LONGITUDE))))
Accuracy_metrics_reg_BF3 <- rbind(Accuracy_metrics_reg_BF3,c(as.double(postResample(pred_KNN_LG_34,validData34$LONGITUDE))))
Accuracy_metrics_reg_BF3 <- rbind(Accuracy_metrics_reg_BF3,c(as.double(postResample(pred_KNN_LG_35,validData35$LONGITUDE))))

Accuracy_metrics_reg_BF <- as.data.frame(Accuracy_metrics_reg_BF)

Accuracy_metrics_reg_BF[,1] <- as.double(Accuracy_metrics_reg_BF[,1])
Accuracy_metrics_reg_BF[,2] <- as.double(Accuracy_metrics_reg_BF[,2])
Accuracy_metrics_reg_BF[,3] <- as.double(Accuracy_metrics_reg_BF[,3])
Accuracy_metrics_reg_BF$WRMSE <- 1
Accuracy_metrics_reg_BF$WSQUARED <- 1
Accuracy_metrics_reg_BF$WMAE <- 1



for (i in 1:nrow(Accuracy_metrics_reg_BF)){
  Accuracy_metrics_reg_BF[i,4] <- Accuracy_metrics_reg_BF[i,1] * Weights[i,2]
}


for (i in 1:nrow(Accuracy_metrics_reg_BF)){
  Accuracy_metrics_reg_BF[i,5] <- Accuracy_metrics_reg_BF[i,2] * Weights[i,2]
}


for (i in 1:nrow(Accuracy_metrics_reg_BF)){
  Accuracy_metrics_reg_BF[i,6] <- Accuracy_metrics_reg_BF[i,3] * Weights[i,2]
}

RMSEfin <- sum(Accuracy_metrics_reg_BF[,4])
Rsquaredfin <- sum(Accuracy_metrics_reg_BF[,5])
MAEfin <- sum(Accuracy_metrics_reg_BF[,6])
RF_LG_byBF <- c(RMSEfin,Rsquaredfin,MAEfin)

Accuracy_metrics_reg <- rbind(Accuracy_metrics_reg, c('RF_LG_byBF', RF_LG_byBF))

#PREDICTING LT WITH RANDOM FOREST----
KNN_LT_11 <- randomForest(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData11, ntree=100)
KNN_LT_12 <- randomForest(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData12, ntree=100)
KNN_LT_13 <- randomForest(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData13, ntree=100)
KNN_LT_14 <- randomForest(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData14, ntree=100)

KNN_LT_21 <- randomForest(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData21, ntree=100)
KNN_LT_22 <- randomForest(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData22, ntree=100)
KNN_LT_23 <- randomForest(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData23, ntree=100)
KNN_LT_24 <- randomForest(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData24, ntree=100)

KNN_LT_31 <- randomForest(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData31, ntree=100)
KNN_LT_32 <- randomForest(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData32, ntree=100)
KNN_LT_33 <- randomForest(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData33, ntree=100)
KNN_LT_34 <- randomForest(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData34, ntree=100)
KNN_LT_35 <- randomForest(LATITUDE ~. -FLOOR -LONGITUDE -BUILDINGID, data = ModData35, ntree=100)

pred_KNN_LT_11 <- predict(KNN_LT_11, validData11)
pred_KNN_LT_12 <- predict(KNN_LT_12, validData12)
pred_KNN_LT_13 <- predict(KNN_LT_13, validData13)
pred_KNN_LT_14 <- predict(KNN_LT_14, validData14)

pred_KNN_LT_21 <- predict(KNN_LT_21, validData21)
pred_KNN_LT_22 <- predict(KNN_LT_22, validData22)
pred_KNN_LT_23 <- predict(KNN_LT_23, validData23)
pred_KNN_LT_24 <- predict(KNN_LT_24, validData24)

pred_KNN_LT_31 <- predict(KNN_LT_31, validData31)
pred_KNN_LT_32 <- predict(KNN_LT_32, validData32)
pred_KNN_LT_33 <- predict(KNN_LT_33, validData33)
pred_KNN_LT_34 <- predict(KNN_LT_34, validData34)
pred_KNN_LT_35 <- predict(KNN_LT_35, validData35)

postResample(pred_KNN_LT_11,validData11$LATITUDE)
postResample(pred_KNN_LT_12,validData12$LATITUDE)
postResample(pred_KNN_LT_13,validData13$LATITUDE)
postResample(pred_KNN_LT_14,validData14$LATITUDE)

postResample(pred_KNN_LT_21,validData21$LATITUDE)
postResample(pred_KNN_LT_22,validData22$LATITUDE)
postResample(pred_KNN_LT_23,validData23$LATITUDE)
postResample(pred_KNN_LT_24,validData24$LATITUDE)

postResample(pred_KNN_LT_31,validData31$LATITUDE)
postResample(pred_KNN_LT_32,validData32$LATITUDE)
postResample(pred_KNN_LT_33,validData33$LATITUDE)
postResample(pred_KNN_LT_34,validData34$LATITUDE)
postResample(pred_KNN_LT_35,validData35$LATITUDE)

Accuracy_metrics_reg_BF4 <- data.frame(RMSE=as.double(), Rsquared=as.double(), MAE=as.double())

Accuracy_metrics_reg_BF4 <- c(as.double(postResample(pred_KNN_LT_11,validData11$LATITUDE)))
Accuracy_metrics_reg_BF4 <- rbind(Accuracy_metrics_reg_BF4,c(as.double(postResample(pred_KNN_LT_12,validData12$LATITUDE))))
Accuracy_metrics_reg_BF4 <- rbind(Accuracy_metrics_reg_BF4,c(as.double(postResample(pred_KNN_LT_13,validData13$LATITUDE))))
Accuracy_metrics_reg_BF4 <- rbind(Accuracy_metrics_reg_BF4,c(as.double(postResample(pred_KNN_LT_14,validData14$LATITUDE))))

Accuracy_metrics_reg_BF4 <- rbind(Accuracy_metrics_reg_BF4,c(as.double(postResample(pred_KNN_LT_21,validData21$LATITUDE))))
Accuracy_metrics_reg_BF4 <- rbind(Accuracy_metrics_reg_BF4,c(as.double(postResample(pred_KNN_LT_22,validData22$LATITUDE))))
Accuracy_metrics_reg_BF4 <- rbind(Accuracy_metrics_reg_BF4,c(as.double(postResample(pred_KNN_LT_23,validData23$LATITUDE))))
Accuracy_metrics_reg_BF4 <- rbind(Accuracy_metrics_reg_BF4,c(as.double(postResample(pred_KNN_LT_24,validData24$LATITUDE))))

Accuracy_metrics_reg_BF4 <- rbind(Accuracy_metrics_reg_BF4,c(as.double(postResample(pred_KNN_LT_31,validData31$LATITUDE))))
Accuracy_metrics_reg_BF4 <- rbind(Accuracy_metrics_reg_BF4,c(as.double(postResample(pred_KNN_LT_32,validData32$LATITUDE))))
Accuracy_metrics_reg_BF4 <- rbind(Accuracy_metrics_reg_BF4,c(as.double(postResample(pred_KNN_LT_33,validData33$LATITUDE))))
Accuracy_metrics_reg_BF4 <- rbind(Accuracy_metrics_reg_BF4,c(as.double(postResample(pred_KNN_LT_34,validData34$LATITUDE))))
Accuracy_metrics_reg_BF4 <- rbind(Accuracy_metrics_reg_BF4,c(as.double(postResample(pred_KNN_LT_35,validData35$LATITUDE))))

Accuracy_metrics_reg_BF <- as.data.frame(Accuracy_metrics_reg_BF)

Accuracy_metrics_reg_BF[,2] <- as.double(Accuracy_metrics_reg_BF[,2])
Accuracy_metrics_reg_BF[,3] <- as.double(Accuracy_metrics_reg_BF[,3])
Accuracy_metrics_reg_BF[,4] <- as.double(Accuracy_metrics_reg_BF[,4])
Accuracy_metrics_reg_BF$WRMSE <- 1
Accuracy_metrics_reg_BF$WSQUARED <- 1
Accuracy_metrics_reg_BF$WMAE <- 1



for (i in 1:nrow(Accuracy_metrics_reg_BF)){
  Accuracy_metrics_reg_BF[i,4] <- Accuracy_metrics_reg_BF[i,1] * Weights[i,2]
}


for (i in 1:nrow(Accuracy_metrics_reg_BF)){
  Accuracy_metrics_reg_BF[i,5] <- Accuracy_metrics_reg_BF[i,2] * Weights[i,2]
}


for (i in 1:nrow(Accuracy_metrics_reg_BF)){
  Accuracy_metrics_reg_BF[i,6] <- Accuracy_metrics_reg_BF[i,3] * Weights[i,2]
}

RMSEfin <- sum(Accuracy_metrics_reg_BF[,4])
Rsquaredfin <- sum(Accuracy_metrics_reg_BF[,5])
MAEfin <- sum(Accuracy_metrics_reg_BF[,6])
KNN_LT_byBF <- c(RMSEfin,Rsquaredfin,MAEfin)

Accuracy_metrics_reg <- rbind(Accuracy_metrics_reg, c('RF_LT_byBF', KNN_LT_byBF))

#Creatgin dataset for presentation
metricsLG <- cbind(Accuracy_metrics_reg_BF, Accuracy_metrics_reg_BF3)
metricsLT <- cbind(Accuracy_metrics_reg_BF2, Accuracy_metrics_reg_BF4)

write.csv(metricsLG, file = 'LGmetrics.csv')
write.csv(metricsLT, file = 'LTmetrics.csv')
write.csv(Weights, file= 'Weights.csv')

#TESTING HAVING -105 to -1000 ----
ModData1000 <- ModData
ModData1000[ModData1000 == -105] <- -1000
RF_LT_1000 <- randomForest(LATITUDE ~ . -LONGITUDE -BUILDINGID -FLOOR,
                           data = ModData1000, ntree=100, importance=TRUE, do.trace = TRUE)
pred_RF_LT_1000 <- predict(RF_LT_1000, validData)
postResample(pred_RF_LT_1000, validData$LATITUDE)
Accuracy_metrics_reg <- rbind(Accuracy_metrics_reg,c('RF_LT_1000',postResample(pred_RF_LT_1000,validData$LATITUDE)))

ModData200 <- ModData
ModData200[ModData200 == -105] <- -2000
RF_LT_200 <- randomForest(LATITUDE ~ . -LONGITUDE -BUILDINGID -FLOOR,
                          data = ModData200, ntree=100, importance=TRUE, do.trace = TRUE)
pred_RF_LT_200 <- predict(RF_LT_200, validData)
postResample(pred_RF_LT_200, validData$LATITUDE)
Accuracy_metrics_reg <- rbind(Accuracy_metrics_reg,c('RF_LT_200',postResample(pred_RF_LT_200,validData$LATITUDE)))

ModData0 <- ModData
ModData0[ModData200 == -105] <- -200
RF_LT_0 <- randomForest(LATITUDE ~ . -LONGITUDE -BUILDINGID -FLOOR,
                        data = ModData0, ntree=100, importance=TRUE, do.trace = TRUE)
pred_RF_LT_0 <- predict(RF_LT_0, validData)
postResample(pred_RF_LT_0, validData$LATITUDE)
Accuracy_metrics_reg <- rbind(Accuracy

#Check WAP signal Outliers ----#

TDTotSignal <- data.frame()

for (i in 1:303){
  count = 0
  for(j in 1:nrow(trainingData)){
    if(trainingData[j,i] >= -30){
      count = count + 1
    }
  }
  TDTotSignal[i,1] <- count
}
TDTotSignal %>%
  arrange(V1) # ---GET RID OF OULTLIER

for(i in 1:303){
  count <- 0
  for(j in 1:nrow(validData)){
    if(validData[j,i] = -45){
      count = count + 1
    }
  }
  TDTotSignal[i,2] <- count
}

Idx_remove <- which(TDTotSignal >= 40)

trainingData <- trainingData[,-Idx_remove]
validData <- validData[,-Idx_remove]

par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(TDTotSignal[,1], main="WAP", sub=paste("Outlier rows: ", boxplot.stats(TDTotSignal[,1])$out))  # box plot for 'speed'
boxplot(TDTotSignal[,2], main="WAP", sub=paste("Outlier rows: ", boxplot.stats(TDTotSignal[,2])$out))  # box plot for 'distance'

#----Creation of grpah


by(trainingData[,1:520],colMeans)

testtt<- trainingData %>%
  group_by(USERID) %>%



         
         apply(df, 2, function(x) which(x == "M017"))
  group_by(USERID)%>%)
  summarise_if(,n=n())
  
apply(user19graph,1 ,function(x) (which(which(x > -30)==TRUE)

count30 <- c()

for (i in 1:520){
  count = 0
  for(j in 1:nrow(trainingData)){
    if(trainingData[j,i] >= -30){
      count = count + 1
    }
    count30[i] <- count
  }
  TDTotSignal[i,1] <- count
}
count30index <- which(count30 > 0)

over30traindata <- trainingData[count30index,520:530]
esquisser(over30traindata)
over30traindata %>%
  group_by(PHONEID) %>%
  
TDTotSignal %>%
  arrange(V1) # ---G

plot_grid(e,f)


e <- ggplot(data = Build3) +
  aes(x = BUILD_FLOOR_ID, weight = n) +
  geom_bar(fill = "#0c4c8a") +
  labs(title = "# Observations per Floor in training set",
       y = "# obersevations",
       subtitle = "Building 3") +
  theme_minimal()

f <- ggplot(data = errordfB3) +
  aes(x = BUILD_FLOOR_ID, weight = avg) +
  geom_bar(fill = "#0c4c8a") +
  labs(title = "Average distance between Predictions/Actual per Floor",
       subtitle = 'Building 3',
       y = "Average distance") +
  theme_minimal()
 by

aaaa <- mutate(trainingData, over30 = length(which(trainingData[1:520] > -30)))# %>%
  #group_by(USERID,over30) %>%
  summarise(over30)
h$over30
aaaa$over30



length(which(c(-121, -34,-32,-3,-3,-11,-23,-4,-5456,-20,-23,-34) > -30))
data.frame(Model= character(),Accuracy=double(), Kappa=double()) #Creating a datafram to store resutls for comparison

n30 <- data.frame(over30 = double())

for (i in 1:now(trainingData)){
  n30 <- rbind(n30,length(which(trainingData[i,1:520] > -30)))
               }
length(which(trainingData[1,1:520] > -30))



g<-trainingData %>%
  group_by(BUILD_FLOOR_ID, PHONEID) %>%
  summarise()
esquisser(g)


