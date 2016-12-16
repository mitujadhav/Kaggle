library(caret)
library(data.table)
library(Boruta)
library(plyr)
library(dplyr)
library(pROC)

ROOT.DIR <- ".."

ID.VAR <- "Id"
TARGET.VAR <- "SalePrice"

sample.df <- read.csv(file.path("D:/My Stuff/Kaggle/House Pricing/train.csv"),stringsAsFactors = FALSE)

candidate.features <- setdiff(names(sample.df),c(ID.VAR,TARGET.VAR))
data.type <- sapply(candidate.features,function(x){class(sample.df[[x]])})
table(data.type)


explanatory.attributes <- setdiff(names(sample.df),c(ID.VAR,TARGET.VAR))
data.classes <- sapply(explanatory.attributes,function(x){class(sample.df[[x]])})

# categorize data types in the data set?
unique.classes <- unique(data.classes)

attr.data.types <- lapply(unique.classes,function(x){names(data.classes[data.classes==x])})
names(attr.data.types) <- unique.classes


response <- sample.df$SalePrice

# remove identifier and response variables
sample.df <- sample.df[candidate.features]

# for numeric set missing values to -1 for purposes of the random forest run
for (x in attr.data.types$integer)
{
  sample.df[[x]][is.na(sample.df[[x]])] <- -1
}

for (x in attr.data.types$character)
{
  sample.df[[x]][is.na(sample.df[[x]])] <- "*MISSING*"
}



set.seed(13)
bor.results <- Boruta(sample.df, response, maxRuns=101, doTrace=0)
bor.results
table(bor.results$finalDecision)



#---------------------------------

train.raw <- read.csv(file.path("D:/My Stuff/Kaggle/House Pricing/train.csv"),stringsAsFactors = FALSE)
test.raw <- read.csv(file.path("D:/My Stuff/Kaggle/House Pricing/train.csv"), stringsAsFactors = FALSE)


CONFIRMED_ATTR <- c("MSSubClass","MSZoning","LotArea","LotShape","LandContour","Neighborhood",
                    "BldgType","HouseStyle","OverallQual","OverallCond","YearBuilt",
                    "YearRemodAdd","Exterior1st","Exterior2nd","MasVnrArea","ExterQual",
                    "Foundation","BsmtQual","BsmtCond","BsmtFinType1","BsmtFinSF1",
                    "BsmtFinType2","BsmtUnfSF","TotalBsmtSF","HeatingQC","CentralAir",
                    "X1stFlrSF","X2ndFlrSF","GrLivArea","BsmtFullBath","FullBath","HalfBath",
                    "BedroomAbvGr","KitchenAbvGr","KitchenQual","TotRmsAbvGrd","Functional",
                    "Fireplaces","GarageType","GarageYrBlt","GarageFinish",
                    "GarageCars","GarageArea","GarageQual","GarageCond","PavedDrive","WoodDeckSF",
                    "OpenPorchSF")

TENTATIVE_ATTR <- c("Alley","LandSlope","Condition1","RoofStyle","MasVnrType","BsmtExposure",
                    "Electrical","EnclosedPorch","SaleCondition")

REJECTED_ATTR <- c("LotFrontage","Street","Utilities","LotConfig","Condition2","RoofMatl",
                   "ExterCond","BsmtFinSF2","Heating","LowQualFinSF","BsmtHalfBath",
                   "X3SsnPorch","ScreenPorch","PoolArea","PoolQC","MiscFeature","MiscVal",
                   "MoSold","YrSold","SaleType")


PREDICTOR_ATTR <- c(CONFIRMED_ATTR,TENTATIVE_ATTR,REJECTED_ATTR)

# Determine data types in the data set

data_types <- sapply(PREDICTOR_ATTR,function(x){class(train.raw[[x]])})
unique_data_types <- unique(data_types)

# Separate attributes by data type
DATA_ATTR_TYPES <- lapply(unique_data_types,function(x){ names(data_types[data_types == x])})
names(DATA_ATTR_TYPES) <- unique_data_types


# create folds for training
set.seed(13)
data_folds <- createFolds(train.raw$SalePrice, k=5)



columns_NAs<-function(train)
{
  for(i in 1:ncol(train))
  {
    print(paste0(colnames(train)[i]," ",table(is.na(train[,i]))))
  }
}

replace_by_mean<-function(data)
{
  for(i in 1:ncol(data))
  {
    data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
  }
  return(data)
}
train<-replace_by_mean(train)











##################################################################################


prepL0FeatureSet1 <- function(df)
{
  id <- df$Id
  if (class(df$SalePrice) != "NULL") 
  {
    y <- log(df$SalePrice)
  } else 
  {
    y <- NULL
  }
  
  predictor_vars <- c(CONFIRMED_ATTR,TENTATIVE_ATTR)
  predictors <- df[predictor_vars]
  
  # for numeric set missing values to -1 for purposes
  num_attr <- intersect(predictor_vars,DATA_ATTR_TYPES$integer)
  for (x in num_attr)
  {
    predictors[[x]][is.na(predictors[[x]])] <- -1
  }
  
  # for character  atributes set missing value
  char_attr <- intersect(predictor_vars,DATA_ATTR_TYPES$character)
  for (x in char_attr){
    predictors[[x]][is.na(predictors[[x]])] <- "*MISSING*"
    predictors[[x]] <- factor(predictors[[x]])
  }
  
  return(list(id=id,y=y,predictors=predictors))
}


L0FeatureSet1 <- list(train=prepL0FeatureSet1(train.raw),
                      test=prepL0FeatureSet1(test.raw))

# Feature Set 2 (xgboost) - Boruta Confirmed Attributes
prepL0FeatureSet2 <- function(df) 
{
  id <- df$Id
  if (class(df$SalePrice) != "NULL") 
  {
    y <- log(df$SalePrice)
  } else 
  {
    y <- NULL
  }
  
  
  predictor_vars <- c(CONFIRMED_ATTR,TENTATIVE_ATTR)
  
  predictors <- df[predictor_vars]
  
  # for numeric set missing values to -1 for purposes
  num_attr <- intersect(predictor_vars,DATA_ATTR_TYPES$integer)
  for (x in num_attr)
  {
    predictors[[x]][is.na(predictors[[x]])] <- -1
  }
  
  # for character  atributes set missing value
  char_attr <- intersect(predictor_vars,DATA_ATTR_TYPES$character)
  for (x in char_attr)
  {
    predictors[[x]][is.na(predictors[[x]])] <- "*MISSING*"
    predictors[[x]] <- as.numeric(factor(predictors[[x]]))
  }
  return(list(id=id,y=y,predictors=as.matrix(predictors)))
}


L0FeatureSet2 <- list(train=prepL0FeatureSet2(train.raw),
                      test=prepL0FeatureSet2(test.raw))











trainOneFold <- function(this_fold,feature_set) {
  # get fold specific cv data
  cv.data <- list()
  cv.data$predictors <- feature_set$train$predictors[this_fold,]
  cv.data$ID <- feature_set$train$id[this_fold]
  cv.data$y <- feature_set$train$y[this_fold]
  
  # get training data for specific fold
  train.data <- list()
  train.data$predictors <- feature_set$train$predictors[-this_fold,]
  train.data$y <- feature_set$train$y[-this_fold]
  
  
  set.seed(825)
  fitted_mdl <- do.call(train,
                        c(list(x=train.data$predictors,y=train.data$y),
                          CARET.TRAIN.PARMS,
                          MODEL.SPECIFIC.PARMS,
                          CARET.TRAIN.OTHER.PARMS))
  
  yhat <- predict(fitted_mdl,newdata = cv.data$predictors,type = "raw")
  
  score <- rmse(cv.data$y,yhat)
  
  ans <- list(fitted_mdl=fitted_mdl,
              score=score,
              predictions=data.frame(ID=cv.data$ID,yhat=yhat,y=cv.data$y))
  
  return(ans)
  
}

# make prediction from a model fitted to one fold
makeOneFoldTestPrediction <- function(this_fold,feature_set) {
  fitted_mdl <- this_fold$fitted_mdl
  
  yhat <- predict(fitted_mdl,newdata = feature_set$test$predictors,type = "raw")
  
  return(yhat)
}


###################################GBM ############

CARET.TRAIN.PARMS <- list(method="gbm")   

CARET.TUNE.GRID <-  expand.grid(n.trees=100, 
                                interaction.depth=10, 
                                shrinkage=0.1,
                                n.minobsinnode=10)

MODEL.SPECIFIC.PARMS <- list(verbose=0) #NULL # Other model specific parameters

# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="none",
                                 verboseIter=FALSE,
                                 classProbs=FALSE)

CARET.TRAIN.OTHER.PARMS <- list(trControl=CARET.TRAIN.CTRL,
                                tuneGrid=CARET.TUNE.GRID,
                                metric="RMSE")

# generate features for Level 1
gbm_set <- llply(data_folds,trainOneFold,L0FeatureSet1)

# final model fit
gbm_mdl <- do.call(train,
                   c(list(x=L0FeatureSet1$train$predictors,y=L0FeatureSet1$train$y),
                     CARET.TRAIN.PARMS,
                     MODEL.SPECIFIC.PARMS,
                     CARET.TRAIN.OTHER.PARMS))

# CV Error Estimate
cv_y <- do.call(c,lapply(gbm_set,function(x){x$predictions$y}))
cv_yhat <- do.call(c,lapply(gbm_set,function(x){x$predictions$yhat}))
rmse(cv_y,cv_yhat)


cat("Average CV rmse:",mean(do.call(c,lapply(gbm_set,function(x){x$score}))))


test_gbm_yhat <- predict(gbm_mdl,newdata = L0FeatureSet1$test$predictors,type = "raw")
gbm_submission <- cbind(Id=L0FeatureSet1$test$id,SalePrice=exp(test_gbm_yhat))
write.csv(gbm_submission,file="gbm_sumbission.csv",row.names=FALSE)

#################XgbTree############################

CARET.TRAIN.PARMS <- list(method="xgbTree")   

CARET.TUNE.GRID <-  expand.grid(nrounds=800, 
                                max_depth=10, 
                                eta=0.03, 
                                gamma=0.1, 
                                colsample_bytree=0.4, 
                                min_child_weight=1)

MODEL.SPECIFIC.PARMS <- list(verbose=0) #NULL # Other model specific parameters

# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="none",
                                 verboseIter=FALSE,
                                 classProbs=FALSE)

CARET.TRAIN.OTHER.PARMS <- list(trControl=CARET.TRAIN.CTRL,
                                tuneGrid=CARET.TUNE.GRID,
                                metric="RMSE")

# generate Level 1 features
xgb_set <- llply(data_folds,trainOneFold,L0FeatureSet2)

# final model fit
xgb_mdl <- do.call(train,
                   c(list(x=L0FeatureSet2$train$predictors,y=L0FeatureSet2$train$y),
                     CARET.TRAIN.PARMS,
                     MODEL.SPECIFIC.PARMS,
                     CARET.TRAIN.OTHER.PARMS))

# CV Error Estimate
cv_y <- do.call(c,lapply(xgb_set,function(x){x$predictions$y}))
cv_yhat <- do.call(c,lapply(xgb_set,function(x){x$predictions$yhat}))
rmse(cv_y,cv_yhat)

cat("Average CV rmse:",mean(do.call(c,lapply(xgb_set,function(x){x$score}))))


test_xgb_yhat <- predict(xgb_mdl,newdata = L0FeatureSet2$test$predictors,type = "raw")
xgb_submission <- cbind(Id=L0FeatureSet2$test$id,SalePrice=exp(test_xgb_yhat))

write.csv(xgb_submission,file="xgb_sumbission.csv",row.names=FALSE)


CARET.TRAIN.PARMS <- list(method="ranger")   

CARET.TUNE.GRID <-  expand.grid(mtry=2*as.integer(sqrt(ncol(L0FeatureSet1$train$predictors))))

MODEL.SPECIFIC.PARMS <- list(verbose=0,num.trees=500) #NULL # Other model specific parameters

# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="none",
                                 verboseIter=FALSE,
                                 classProbs=FALSE)

CARET.TRAIN.OTHER.PARMS <- list(trControl=CARET.TRAIN.CTRL,
                                tuneGrid=CARET.TUNE.GRID,
                                metric="RMSE")

# generate Level 1 features
rngr_set <- llply(data_folds,trainOneFold,L0FeatureSet1)

# final model fit
rngr_mdl <- do.call(train,
                    c(list(x=L0FeatureSet1$train$predictors,y=L0FeatureSet1$train$y),
                      CARET.TRAIN.PARMS,
                      MODEL.SPECIFIC.PARMS,
                      CARET.TRAIN.OTHER.PARMS))

# CV Error Estimate
cv_y <- do.call(c,lapply(rngr_set,function(x){x$predictions$y}))
cv_yhat <- do.call(c,lapply(rngr_set,function(x){x$predictions$yhat}))
rmse(cv_y,cv_yhat)


cat("Average CV rmse:",mean(do.call(c,lapply(rngr_set,function(x){x$score}))))


test_rngr_yhat <- predict(rngr_mdl,newdata = L0FeatureSet1$test$predictors,type = "raw")
rngr_submission <- cbind(Id=L0FeatureSet1$test$id,SalePrice=exp(test_rngr_yhat))

write.csv(rngr_submission,file="rngr_sumbission.csv",row.names=FALSE)

gbm_yhat <- do.call(c,lapply(gbm_set,function(x){x$predictions$yhat}))
xgb_yhat <- do.call(c,lapply(xgb_set,function(x){x$predictions$yhat}))
rngr_yhat <- do.call(c,lapply(rngr_set,function(x){x$predictions$yhat}))

# create Feature Set
L1FeatureSet <- list()

L1FeatureSet$train$id <- do.call(c,lapply(gbm_set,function(x){x$predictions$ID}))
L1FeatureSet$train$y <- do.call(c,lapply(gbm_set,function(x){x$predictions$y}))
predictors <- data.frame(gbm_yhat,xgb_yhat,rngr_yhat)
predictors_rank <- t(apply(predictors,1,rank))
colnames(predictors_rank) <- paste0("rank_",names(predictors))
L1FeatureSet$train$predictors <- predictors #cbind(predictors,predictors_rank)

L1FeatureSet$test$id <- gbm_submission[,"Id"]
L1FeatureSet$test$predictors <- data.frame(gbm_yhat=test_gbm_yhat,
                                           xgb_yhat=test_xgb_yhat,
                                           rngr_yhat=test_rngr_yhat)


CARET.TRAIN.PARMS <- list(method="nnet") 

CARET.TUNE.GRID <-  NULL  # NULL provides model specific default tuning parameters

# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=5,
                                 repeats=1,
                                 verboseIter=FALSE)

CARET.TRAIN.OTHER.PARMS <- list(trControl=CARET.TRAIN.CTRL,
                                maximize=FALSE,
                                tuneGrid=CARET.TUNE.GRID,
                                tuneLength=7,
                                metric="RMSE")

MODEL.SPECIFIC.PARMS <- list(verbose=FALSE,linout=TRUE,trace=FALSE) #NULL # Other model specific parameters


# train the model
set.seed(825)
l1_nnet_mdl <- do.call(train,c(list(x=L1FeatureSet$train$predictors,y=L1FeatureSet$train$y),
                               CARET.TRAIN.PARMS,
                               MODEL.SPECIFIC.PARMS,
                               CARET.TRAIN.OTHER.PARMS))

l1_nnet_mdl

cat("Average CV rmse:",mean(l1_nnet_mdl$resample$RMSE),"\n")

test_l1_nnet_yhat <- predict(l1_nnet_mdl,newdata = L1FeatureSet$test$predictors,type = "raw")
l1_nnet_submission <- cbind(Id=L1FeatureSet$test$id,SalePrice=exp(test_l1_nnet_yhat))
colnames(l1_nnet_submission) <- c("Id","SalePrice")

write.csv(l1_nnet_submission,file="l1_nnet_submission.csv",row.names=FALSE)

