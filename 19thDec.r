library(DMwR)
library(Boruta)
library(dplyr)
library(caret)
library(randomForest)
library(hydroGOF)
library(e1071)
library(corrplot)


train<- read.csv(file.path("D:/My Stuff/Kaggle/House Pricing/train.csv"),stringsAsFactors = FALSE)
test<- read.csv(file.path("D:/My Stuff/Kaggle/House Pricing/train.csv"), stringsAsFactors = FALSE)
SUBMISSION<- read.csv(file.path("D:/My Stuff/Kaggle/House Pricing/Sample_Submission.csv"), stringsAsFactors = FALSE)

SalePrice=train$SalePrice
train1<-train


#Remove Id since of no use
train$Id=NULL
train$SalePrice=NULL
test$Id=NULL
y<-train$SalePrice
test<-test[,-80]
#Row binding train & test set for feature engineering
train_test = rbind(train, test)
ntrain=nrow(train)

features=names(train_test)

#convert character into integer
for(f in features){
  if(class(train_test[[f]])=="character"){
    levels=sort(unique(train_test[[f]]))
    train_test[[f]]=as.integer(factor(train_test[[f]],levels = levels))
  }
}

Remove_Cols_40NAs<-function(Data)
{
  Reduced_columns<-Data
  for(i in 1:length(Data)) 
  { 
    if( sum( is.na( Data[, i] ) ) /nrow(Data) >= 0.4 ) 
    {  
      for(j in 1:length(Reduced_columns)) 
      {
        if( length( grep(names(Data[i]), names(Reduced_columns)[j]) ) ==1)
        { 
          Reduced_columns <- Reduced_columns[ , -j] 
        }   
      } 
    }
  }
  return(Reduced_columns)
}
train_test40<-Remove_Cols_40NAs(train_test)


# replace_by_mean<-function(data)
# {
#   for(i in 1:ncol(data))
#   {
#     data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
#   }
#   return(data)
# }
# train<-replace_by_mean(train)

# for(i in seq(1:ncol(train)))
# {
#   print(paste0(table(is.na(train[,i]))))
# }
# 
# train1<-(train[complete.cases(train),])


#-------------------------------------------------------------------------------

inputData <- knnImputation(train_test40)

train2<-inputData[1:1460,]
test2<-inputData[1461:2920,]
train2<-cbind(train2,SalePrice)


# library(party)
# cf1 <- cforest(train2$SalePrice~ ., data = train2, controls = cforest_unbiased(mtry = 2, ntree = 50))
# #get variable importance based on mean decrease in accuracy
# varimp(cf1)
# #based on mean difference in accuracy
# varimp(cf1, conditional = TRUE)
# varimpAUC(cf1)
# 
# 
# #install.packages("relaimpo")
# library(relaimpo)
# 
# #fit lm model
# lmMod <- lm(train2$SalePrice ~ ., data = train2)
# 
# #calculate relative importance scaled upto 100
# relImportance <- calc.relimp(lmMod, type = 'lmg', rela = TRUE)
# 
# #relative importance
# sort(relImportance$lmg, decreasing = TRUE)
# 

set.seed(123)
boruta_output <- Boruta(train2$SalePrice ~ ., data = na.omit(train2), doTrace=2)
boruta_output$finalDecision

table(boruta_output$finalDecision)

# Tentative Confirmed  Rejected 
# 11        45        18 

final.boruta <- TentativeRoughFix(boruta_output)
final.boruta
  
getSelectedAttributes(boruta_output)
getSelectedAttributes(final.boruta, withTentative = F)
getSelectedAttributes(boruta_output)
  
boruta.df <- attStats(final.boruta)
print(boruta.df)
arrange(cbind(attr=rownames(attStats(boruta_output)), attStats(boruta_output)),desc(medianImp))
boruta.df[which(boruta.df$medianImp>2.8),]
  
#-------------------------------------------------------------------------------
## Important Variables
names(train2)
train_impVar<-train2[,getSelectedAttributes(boruta_output)]
names(train_impVar)

train_impVar
#-----------------------------------------------------------------------
# Using traditional RFE method for feature selection

set.seed(123)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
rfe.train <- rfe(train2[,1:74], train2[,75], sizes=1:12, rfeControl=control)
rfe.train
plot(rfe.train, type=c("g", "o"), cex = 1.0, col = 1:74)
predictors(rfe.train)

#-----------------------------------------------------
M <- cor(finalData)
corrplot(M, method="circle")
corrplot(M, type="lower")


cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

res1 <- cor.mtest(train2,0.95)
res2 <- cor.mtest(train2,0.99)
## specialized the insignificant value according to the significant level
corrplot(M, p.mat = res1[[1]], sig.level=0.2)


#########################################################################
#################### Model Building #####################################
#########################################################################


finalData<-train2
good_columns<-names(train_impVar)
finalData <- finalData[,c(good_columns,"SalePrice")]

inTraining <- createDataPartition(finalData$SalePrice, p=0.6, list=FALSE)
trainingStd <- finalData[inTraining,]
testdataStd <- finalData[-inTraining,]
inVal <- createDataPartition(testdataStd$SalePrice, p=0.5, list=FALSE)
crossvalStd <- testdataStd[inVal,]
testingStd <- testdataStd[-inVal,]

#xgbTree
curr.model <- train(SalePrice ~ .,data = trainingStd,method = "xgbTree")
preds<- predict(curr.model,testdataStd)
preds
sqrt(mean((log(testdataStd$SalePrice+1) - log(preds+1))^2))
#0.1393658

curr.model <- train(SalePrice ~ .,data = finalData,method = "xgbTree")
preds<- predict(curr.model,test2)
preds
output_xgboost<-as.data.frame(preds)

#Knn
knn_model <- train(SalePrice ~ .,data = trainingStd,method = "knn")
knnpreds<- predict(knn_model,testdataStd)
knnpreds
sqrt(mean((log(testdataStd$SalePrice+1) - log(knnpreds+1))^2))
#0.2169576

#SVM
model <- svm(SalePrice ~ .,data = trainingStd)
svmoutput<- predict(object = model, newdata = testdataStd)
svmoutput
rmse(svmoutput, testdataStd$SalePrice)
sqrt(mean((log(testdataStd$SalePrice+1) - log(svmoutput+1))^2))
#0.1520209

##Random Forest
rpart.model <- train(SalePrice ~ .,data = trainingStd,method = "rpart")
rpart_preds<- predict(rpart.model,testdataStd)
rpart_preds
rmse(rpart_preds, testdataStd$SalePrice)
sqrt(mean((log(testdataStd$SalePrice+1) - log(rpart_preds+1))^2))
# 0.2658621

## Random Forest
rfFit=randomForest(SalePrice ~ .,data = trainingStd)
rfpreds<- predict(rfFit,testdataStd)
sqrt(mean((log(testdataStd$SalePrice+1) - log(rfpreds+1))^2))
#0.1415965

## GBM
gbmModel<-gbm(formula = trainingStd$SalePrice~ .,
    data = trainingStd[,-48],
    var.monotone = NULL,
    n.trees = 500000,
    interaction.depth = 1,
    n.minobsinnode = 10,
    shrinkage = 0.001,
    bag.fraction = 0.5,
    train.fraction = 1.0,
    cv.folds=0,
    keep.data = TRUE,
    verbose = "CV",
    class.stratify.cv=NULL,
    n.cores = NULL)

gbmpreds<- predict(gbmModel,testdataStd,n.trees = 500000)
sqrt(mean((log(testdataStd$SalePrice+1) - log(gbmpreds+1))^2))
# 0.1420983

## lm
lm_fit<-lm(trainingStd$SalePrice~ .,data=trainingStd[,-48])
lm_preds<- predict(lm_fit,testdataStd)
sqrt(mean((log(testdataStd$SalePrice+1) - log(lm_preds+1))^2))
#0.1655893

## Robust Fitting of Linear Models
library(MASS)
what <- "RLM"
RLM_model <- rlm(trainingStd$SalePrice~ .,data=trainingStd[,-48])
RLM_preds <- predict(RLM_model, type="response", testdataStd)
sqrt(mean((log(testdataStd$SalePrice+1) - log(RLM_preds+1))^2))
#0.1514151

##
library(earth)
what <- "MARS (earth)"
EARTH_model <- earth(trainingStd$SalePrice~ .,data=trainingStd[,-48])
earth_pred <- predict(EARTH_model, testdataStd)
sqrt(mean((log(testdataStd$SalePrice+1) - log(earth_pred+1))^2))
#0.147303


#---------------------------------------------------------------------------------
write.csv(output_xgboost,"D:/My Stuff/Kaggle/House Pricing/xgboost_submission.csv")
preds

gbmImp <- varImp(rpart.model, scale = FALSE)
gbmImp
# rpart variable importance
# only 20 most important variables shown (out of 45)
# Overall

# OverallQual   0.8264
# GrLivArea     0.6621
# YearBuilt     0.5919
# GarageCars    0.3573
# ExterQual     0.3476
# FullBath      0.3003
# GarageFinish  0.2828
# Fireplaces    0.0000
# MSSubClass    0.0000
# MasVnrType    0.0000
# GarageCond    0.0000
# GarageType    0.0000
# YearRemodAdd  0.0000
# BsmtFullBath  0.0000
# HalfBath      0.0000
# PavedDrive    0.0000
# HouseStyle    0.0000
# LotArea       0.0000
# OverallCond   0.0000
# OpenPorchSF   0.0000

gbmImp <- varImp(model, scale = FALSE)
gbmImp





