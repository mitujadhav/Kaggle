train<-read.csv("D:/My Stuff/Kaggle/House Pricing/train.csv")
test<-read.csv("D:/My Stuff/Kaggle/House Pricing/test.csv")

library(caret)
library(doParallel)
library(foreach)
avail_cores <- detectCores() # available cores
p_cluster <- makeCluster(avail_cores-2)
registerDoParallel(p_cluster)
sprintf("Cores registered: %d",getDoParWorkers())

varinfo <- nearZeroVar(train, saveMetrics = TRUE)
sortedVarinfo <- varinfo[order(varinfo["percentUnique"]),]
sprintf("Total number of near-zero var preds: %d", sum(sortedVarinfo$nzv))
head(sortedVarinfo[sortedVarinfo$nzv==TRUE,])

na_cols <- colSums(is.na(train)) >= nrow(train)*0.85
table(na_cols)
na_cols[na_cols==TRUE]
#removing column Id and columns having more than 0.85% NAs
train<-train[,-1]

na_col_names <- colnames(train)[na_cols]
sprintf("NA columns: %d\n",length(na_col_names))
print(na_col_names)
nona_columns <- setdiff(colnames(train),na_col_names)
train <- train[nona_columns]
dropped_columns <- c(na_col_names)

# Done----



empty_cols <- colSums(train=="") >= nrow(train)*0.85
empty_col_names <- colnames(train)[empty_cols]
sprintf("Empty columns: %d\n",length(empty_col_names))
print(empty_col_names)
nonempty_columns <- setdiff(colnames(train),empty_col_names)
train <- train[nonempty_columns] 
dropped_columns <- c(dropped_columns,empty_col_names)

varinfo <- nearZeroVar(train, saveMetrics = TRUE)
sortedVarinfo <- varinfo[order(varinfo["percentUnique"]),]
factor_col_names <- setdiff(names(Filter(is.factor, train)),c("SalePrice"))
print("Near zero variance:")
head(sortedVarinfo)
print("Factors variables:")
print(factor_col_names)
summary(train[factor_col_names])
train <- train[setdiff(colnames(train),factor_col_names)]
dropped_columns <- c(dropped_columns, factor_col_names)

#----
# Correlation Plot
library(corrplot)
M <- cor(train)
corrplot(M, method="number")

library(corrgram)
corrgram(train, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="House Rent")


corrplot(train, type="lower")

inData1 <- createDataPartition(train$SalePrice,p = 0.1, list = FALSE)
rfData1 <- train[inData1,]
rfInData2 <- train[-inData1,]
inData2 <- createDataPartition(rfInData2$SalePrice,p = 0.1, list = FALSE)
rfData2 <- rfInData2[inData2,]

rfData1<-rfData1[complete.cases(rfData1),]
x1 <- rfData1[,setdiff(colnames(rfData1),c("SalePrice"))]
y1 <- rfData1$SalePrice

rfData2<-rfData2[complete.cases(rfData2),]
x2 <- rfData2[,setdiff(colnames(rfData2),c("SalePrice"))]
y2 <- rfData2$SalePrice

rfeCtrl <- rfeControl(functions=rfFuncs, 
                      method="cv", 
                      number=10,
                      repeats = 3)
results1 <- rfe(x1, y1, sizes=c(5,10,15,25,40), rfeControl=rfeCtrl)
results2 <- rfe(x2, y2, sizes=c(5,10,15,25,40), rfeControl=rfeCtrl)
plot(results1, type=c("g", "o"))
plot(results2, type=c("g", "o"))
final_predictors <- unique(c(predictors(results1),predictors(results2)))
final_predictors<-final_predictors[final_predictors != "Id"];
print("Final predictors :")
print(final_predictors)

#---------------

useAutomaticPredictors = TRUE
usePCA = FALSE

if (useAutomaticPredictors)
{
  good_columns <- c(final_predictors,c("SalePrice"))
  pred_columns <- final_predictors 
}else
{
  good_columns <- setdiff(colnames(finalData), dropped_columns)
  pred_columns <- setdiff(good_columns, c("SalePrice"))
}

colname<-c(names(train))
test2<-test[colname[-38]]

columns_having_Nas<-function(data)
{
  for(i in seq(1:length(data)))
  {
    if(sum(is.na(data[,i]))>0)
    {
      print(paste0(names(data)[i]," ",sum(is.na(data[,i]))))     
    }
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


# [1]"Id" "MSSubClass"    "LotFrontage"   "LotArea"       "OverallQual"
# [5] "OverallCond"   "YearBuilt"     "YearRemodAdd"  "MasVnrArea"
# [9] "BsmtFinSF1"    "BsmtFinSF2"    "BsmtUnfSF"     "TotalBsmtSF"
# [13] "X1stFlrSF"     "X2ndFlrSF"     "LowQualFinSF"  "GrLivArea"
# [17] "BsmtFullBath"  "BsmtHalfBath"  "FullBath"      "HalfBath"
# [21] "BedroomAbvGr"  "KitchenAbvGr"  "TotRmsAbvGrd"  "Fireplaces"
# [25] "GarageYrBlt"   "GarageCars"    "GarageArea"    "WoodDeckSF"
# [29] "OpenPorchSF"   "EnclosedPorch" "X3SsnPorch"    "ScreenPorch"
# [33] "PoolArea"      "MiscVal"       "MoSold"        "YrSold"
# [37] "SalePrice"

finalData<-train2
#finalData <- finalData[good_columns]
inTraining <- createDataPartition(finalData$SalePrice, p=0.6, list=FALSE)
trainingStd <- finalData[inTraining,]
testdataStd <- finalData[-inTraining,]
inVal <- createDataPartition(testdataStd$SalePrice, p=0.5, list=FALSE)
crossvalStd <- testdataStd[inVal,]
testingStd <- testdataStd[-inVal,]

if (usePCA)
{
  PCA.model <- preProcess(trainingStd[pred_columns],method="pca", thresh=0.95)
  training <- predict(PCA.model, trainingStd)
  crossvalidation <- predict(PCA.model,crossvalStd )
  testing <- predict(PCA.model, testingStd)  
} else
{
  training <- trainingStd
  crossvalidation <- crossvalStd
  testing <- testingStd
}

All.Methods <- c("lda","rpart","knn","lvq","xgbTree")#,"lssvmRadial")
nr_models <- length(All.Methods)
Cross.Accuracy <- c()
Training.Time <- c()
bestAccuracy <- 0 
pred_columns<-pred_columns[-1]
 
curr.model <- train(SalePrice ~ .,data = train2,method = "xgbTree")
preds<- predict(curr.model,test2)
preds

#SVM
library(e1071)
model <- svm( SalePrice ~., data = train2)
#submission <- data.frame(PassengerId = test$PassengerId)
svmoutput<- predict(object = model, newdata = test2)
submission <- data.frame(Id= test2$Id)
submission$svm<-svmoutput
submission$xgbTree<-preds


write.csv(submission,"D:/My Stuff/Kaggle/House Pricing/submission.csv")



prediction_output<-as.data.frame(preds)
test4<-cbind(test,prediction_output)
names(test4)
colnames(test4)[81]<-"SalePrice"

svm()
