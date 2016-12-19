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
#install.packages("DMwR")
library(DMwR)
inputData <- knnImputation(train_test40)


library(caret)
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

library(Boruta)
boruta_output <- Boruta(train2$SalePrice ~ ., data = na.omit(train2), doTrace=2)

# 
# boruta_output$finalDecision
# MSSubClass      MSZoning   LotFrontage       LotArea        Street      LotShape 
# Confirmed     Confirmed     Confirmed     Confirmed      Rejected     Tentative 

# LandContour     Utilities     LotConfig     LandSlope  Neighborhood    Condition1 
# Confirmed      Rejected      Rejected     Tentative     Confirmed     Tentative 

# Condition2      BldgType    HouseStyle   OverallQual   OverallCond     YearBuilt 
# Rejected     Confirmed     Confirmed     Confirmed     Confirmed     Confirmed 

# YearRemodAdd     RoofStyle      RoofMatl   Exterior1st   Exterior2nd    MasVnrType 
# Confirmed     Tentative      Rejected     Confirmed     Confirmed     Confirmed 

# MasVnrArea     ExterQual     ExterCond    Foundation      BsmtQual      BsmtCond 
# Confirmed     Confirmed      Rejected     Confirmed     Confirmed      Rejected 

# BsmtExposure  BsmtFinType1    BsmtFinSF1  BsmtFinType2    BsmtFinSF2     BsmtUnfSF 
# Tentative     Confirmed     Confirmed      Rejected      Rejected     Confirmed 

# TotalBsmtSF       Heating     HeatingQC    CentralAir    Electrical     X1stFlrSF 
# Confirmed      Rejected     Confirmed     Confirmed     Tentative     Confirmed 

# X2ndFlrSF  LowQualFinSF     GrLivArea  BsmtFullBath  BsmtHalfBath      FullBath 
# Confirmed      Rejected     Confirmed     Confirmed      Rejected     Confirmed 

# HalfBath  BedroomAbvGr  KitchenAbvGr   KitchenQual  TotRmsAbvGrd    Functional 
# Confirmed     Confirmed     Confirmed     Confirmed     Confirmed     Tentative 

# Fireplaces    GarageType   GarageYrBlt  GarageFinish    GarageCars    GarageArea 
# Confirmed     Confirmed     Confirmed     Confirmed     Confirmed     Confirmed 

# GarageQual    GarageCond    PavedDrive    WoodDeckSF   OpenPorchSF EnclosedPorch 
# Tentative     Confirmed     Confirmed     Confirmed     Confirmed     Tentative 

# X3SsnPorch   ScreenPorch      PoolArea       MiscVal        MoSold        YrSold 
# Rejected     Tentative      Rejected      Rejected      Rejected      Rejected 

# SaleType SaleCondition 
# Rejected     Tentative 
# Levels: Tentative Confirmed Rejected
# 
# table(boruta_output$finalDecision)
# 
# Tentative Confirmed  Rejected 
# 11        45        18 

#-------------------------------------------------------------------------------
## Important Variables
names(train2)

train_impVar<-train2[,c(1:4,7,11,14:19,22:26,28,29,32,33,36,37,39,40,42,43,45,46,48:53,55:60,62:65)]
names(train_impVar)
# [1] "MSSubClass"   "MSZoning"     "LotFrontage"  "LotArea"      "LandContour" 
# [6] "Neighborhood" "BldgType"     "HouseStyle"   "OverallQual"  "OverallCond" 
# [11] "YearBuilt"    "YearRemodAdd" "Exterior1st"  "Exterior2nd"  "MasVnrType"  
# [16] "MasVnrArea"   "ExterQual"    "Foundation"   "BsmtQual"     "BsmtFinType1"
# [21] "BsmtFinSF1"   "BsmtUnfSF"    "TotalBsmtSF"  "HeatingQC"    "CentralAir"  
# [26] "X1stFlrSF"    "X2ndFlrSF"    "GrLivArea"    "BsmtFullBath" "FullBath"    
# [31] "HalfBath"     "BedroomAbvGr" "KitchenAbvGr" "KitchenQual"  "TotRmsAbvGrd"
# [36] "Fireplaces"   "GarageType"   "GarageYrBlt"  "GarageFinish" "GarageCars"  
# [41] "GarageArea"   "GarageCond"   "PavedDrive"   "WoodDeckSF"   "OpenPorchSF" 

#-----------------------------------------------------------------------
### Model Building

require(randomForest)
fit=randomForest(train2$SalePrice~., data=train_impVar)


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
curr.model <- train(SalePrice ~ .,data = finalData,method = "xgbTree")
preds<- predict(curr.model,test2)
preds

#SVM
library(e1071)
model <- svm(SalePrice ~ .,data = finalData)
svmoutput<- predict(object = model, newdata = test2)
submission <- data.frame(Id= test2$Id)
submission$svm<-svmoutput
submission$xgbTree<-preds


write.csv(submission,"D:/My Stuff/Kaggle/House Pricing/submission.csv")











