
library(Amelia)
library(mice)
library(ggplot2)
library(lattice)


train_raw <- read.csv(file.path("D:/My Stuff/Kaggle/House Pricing/train.csv"),stringsAsFactors = FALSE)

train_raw$MSSubClass <- as.factor(train_raw$MSSubClass)
train_raw$MoSold <- as.factor(train_raw$MoSold)
train_raw$YrSold <- as.factor(train_raw$YrSold)


missmap(train_raw[-1], col=c('grey', 'steelblue'), y.cex=0.5, x.cex=0.8)

sort(sapply(train_raw, function(x) { sum(is.na(x)) }), decreasing=TRUE)

exclude <- c('PoolQC', 'MiscFeature', 'Alley', 'Fence')
include <- setdiff(names(train_raw), exclude)

train_raw <- train_raw[include]

######### For Numeric Data Imputation #################
# For LotFrontage, MasVnrArea, GarbageYrBlt
imp.train_raw <- mice(train_raw, m=1, method='cart', printFlag=FALSE)
imp.train_raw

xyplot(imp.train_raw, LotFrontage ~ LotArea)
densityplot(imp.train_raw, ~LotFrontage)


table(train_raw$LotFrontage)
table(imp.train_raw$imp$LotFrontage)


table(train_raw$MasVnrArea)
table(imp.train_raw$imp$MasVnrArea)


table(train_raw$GarageYrBlt)
table(imp.train_raw$imp$GarageYrBlt)

############# For Ordered Character Data ###############
# For GarbageQual,GarbageCond,BsmtQual,BsmtFinType1,BsmtFinType2,Electrical,GargaeFinish


imp.train_raw <- mice(train_raw, m=1, method='polr', printFlag=FALSE)
imp.train_raw

densityplot(imp.train_raw, ~GarbageQual)


table(train_raw$GarbageQual)
table(imp.train_raw$imp$GarbageQual)


############# For Unordered Character Data ###############
# For GarbageType, MasVnrType




table(train_raw$MasVnrType)
table(imp.train_raw$imp$MasVnrType)

table(train_raw$GarbageQual)
table(imp.train_raw$imp$GarbageQual)









