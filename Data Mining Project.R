# Importing the dataset
train_data <- read.csv("train.csv", stringsAsFactors = F)
test_data <- read.csv("test.csv", stringsAsFactors = F)

# Look at the data
head(train_data)
head(test_data)
str(train_data)
str(test_data)

# Checking the dimensions 
dim(test_data)
dim(train_data)

# Duplicate rows
nrow(train_data) - nrow(unique(train_data))
nrow(test_data) - nrow(unique(test_data))

# Combining the test and train data
library("dplyr")
mydata <- bind_rows(train_data, test_data)
head(mydata)
summary(mydata)

# Creating dataset one with categorical and another with nnumeric variable
cat_var <- names(mydata)[which(sapply(mydata, is.character))]
mydata_cat <- mydata[cat_var]
num_var <- names(mydata)[which(sapply(mydata, is.numeric))]
mydata_num <- mydata[num_var]
head(mydata_num)

# EDA
library(ggplot2)
library(DataExplorer)
plot_str(mydata)
plot_histogram(mydata)
plot_density(mydata)
plot_bar(mydata)
plot_correlation(mydata_num, type = 'continuous')
hist(mydata$SalePrice)

# Combining the data
test_data$SalePrice <- NA
train_data$isTrain <- 1
test_data$isTrain <- 0
mydata <- rbind(train_data,test_data)

# Finding missing value in the dataset
sum(is.na(mydata)) / (nrow(mydata) * ncol(mydata))
colSums(sapply(mydata, is.na))

# Handling missing values by imputing mean/median or zero
mydata$MasVnrArea[which(is.na(mydata$MasVnrArea))] <- mean(mydata$MasVnrArea,na.rm=T)
mydata$LotFrontage[which(is.na(mydata$LotFrontage))] <- median(mydata$LotFrontage,na.rm = T)
mydata$GarageYrBlt[which(is.na(mydata$GarageYrBlt))] <- 0 

# Changing NA values to None
mydata$AlleyNew <- as.character(mydata$Alley)
mydata$AlleyNew[which(is.na(mydata$Alley))] <- "None"
mydata$Alley <- as.factor(mydata$AlleyNew)
mydata <- subset(mydata,select = -AlleyNew)

mydata$MasVnrTypeNew <- as.character(mydata$MasVnrType)
mydata$MasVnrTypeNew[which(is.na(mydata$MasVnrType))] <- "None"
mydata$MasVnrType <- as.factor(mydata$MasVnrTypeNew)
mydata <- subset(mydata,select = -MasVnrTypeNew)

mydata$FireplaceQuNew <- as.character(mydata$FireplaceQu)
mydata$FireplaceQuNew[which(is.na(mydata$FireplaceQu))] <- "None"
mydata$FireplaceQu <- as.factor(mydata$FireplaceQuNew)
mydata <- subset(mydata,select = -FireplaceQuNew)

mydata$PoolQCNew <- as.character(mydata$PoolQC)
mydata$PoolQCNew[which(is.na(mydata$PoolQC))] <- "None"
mydata$PoolQC <- as.factor(mydata$PoolQCNew)
mydata <- subset(mydata,select = -PoolQCNew)

mydata$FenceNew <- as.character(mydata$Fence)
mydata$FenceNew[which(is.na(mydata$Fence))] <- "None"
mydata$Fence <- as.factor(mydata$FenceNew)
mydata <- subset(mydata,select = -FenceNew)

mydata$MiscFeatureNew <- as.character(mydata$MiscFeature)
mydata$MiscFeatureNew[which(is.na(mydata$MiscFeature))] <- "None"
mydata$MiscFeature <- as.factor(mydata$MiscFeatureNew)
mydata <- subset(mydata,select = -MiscFeatureNew)

mydata$GarageTypeNew <- as.character(mydata$GarageType)
mydata$GarageTypeNew[which(is.na(mydata$GarageType))] <- "None"
mydata$GarageType <- as.factor(mydata$GarageTypeNew)
mydata <- subset(mydata,select = -GarageTypeNew)

mydata$GarageFinishNew <- as.character(mydata$GarageFinish)
mydata$GarageFinishNew[which(is.na(mydata$GarageFinish))] <- "None"
mydata$GarageFinish <- as.factor(mydata$GarageFinishNew)
mydata <- subset(mydata,select = -GarageFinishNew)

mydata$GarageQualNew <- as.character(mydata$GarageQual)
mydata$GarageQualNew[which(is.na(mydata$GarageQual))] <- "None"
mydata$GarageQual <- as.factor(mydata$GarageQualNew)
mydata <- subset(mydata,select = -GarageQualNew)

mydata$GarageCondNew <- as.character(mydata$GarageCond)
mydata$GarageCondNew[which(is.na(mydata$GarageCond))] <- "None"
mydata$GarageCond <- as.factor(mydata$GarageCondNew)
mydata <- subset(mydata,select = -GarageCondNew)

mydata$BsmtQualNew <- as.character(mydata$BsmtQual)
mydata$BsmtQualNew[which(is.na(mydata$BsmtQual))] <- "None"
mydata$BsmtQual <- as.factor(mydata$BsmtQualNew)
mydata <- subset(mydata,select = -BsmtQualNew)

mydata$BsmtCondNew <- as.character(mydata$BsmtCond)
mydata$BsmtCondNew[which(is.na(mydata$BsmtCond))] <- "None"
mydata$BsmtCond <- as.factor(mydata$BsmtCondNew)
mydata <- subset(mydata,select = -BsmtCondNew)

mydata$BsmtExposureNew <- as.character(mydata$BsmtExposure)
mydata$BsmtExposureNew[which(is.na(mydata$BsmtExposure))] <- "None"
mydata$BsmtExposure <- as.factor(mydata$BsmtExposureNew)
mydata <- subset(mydata,select = -BsmtExposureNew)

mydata$BsmtFinType1New <- as.character(mydata$BsmtFinType1)
mydata$BsmtFinType1New[which(is.na(mydata$BsmtFinType1))] <- "None"
mydata$BsmtFinType1 <- as.factor(mydata$BsmtFinType1New)
mydata <- subset(mydata,select = -BsmtFinType1New)

mydata$BsmtFinType2New <- as.character(mydata$BsmtFinType2)
mydata$BsmtFinType2New[which(is.na(mydata$BsmtFinType2))] <- "None"
mydata$BsmtFinType2 <- as.factor(mydata$BsmtFinType2New)
mydata <- subset(mydata,select = -BsmtFinType2New)

mydata$ElectricalNew <- as.character(mydata$Electrical)
mydata$ElectricalNew[which(is.na(mydata$Electrical))] <- "None"
mydata$Electrical <- as.factor(mydata$ElectricalNew)
mydata <- subset(mydata,select = -ElectricalNew)

# Factorizing
mydata$MSZoning<- factor(mydata$MSZoning)
mydata$Street <- factor(mydata$Street)
mydata$LotShape <-factor(mydata$LotShape )
mydata$LandContour<-factor(mydata$LandContour)
mydata$Utilities<-factor(mydata$Utilities)
mydata$LotConfig<-factor(mydata$LotConfig)
mydata$LandSlope<-factor(mydata$LandSlope)
mydata$Neighborhood<-factor(mydata$Neighborhood)
mydata$Condition1<-factor(mydata$Condition1)
mydata$Condition2<-factor(mydata$Condition2)
mydata$BldgType<-factor(mydata$BldgType)
mydata$HouseStyle<-factor(mydata$HouseStyle)
mydata$RoofStyle<-factor(mydata$RoofStyle)
mydata$RoofMatl<-factor(mydata$RoofMatl)
mydata$Exterior1st<-factor(mydata$Exterior1st)
mydata$Exterior2nd<-factor(mydata$Exterior2nd)
mydata$ExterQual<-factor(mydata$ExterQual)
mydata$ExterCond<-factor(mydata$ExterCond)
mydata$Foundation<-factor(mydata$Foundation)
mydata$Heating<-factor(mydata$Heating)
mydata$HeatingQC<-factor(mydata$HeatingQC)
mydata$CentralAir<-factor(mydata$CentralAir)
mydata$KitchenQual<-factor(mydata$KitchenQual)
mydata$Functional<-factor(mydata$Functional)
mydata$PavedDrive<-factor(mydata$PavedDrive)
mydata$SaleType<-factor(mydata$SaleType)
mydata$SaleCondition<-factor(mydata$SaleCondition)

# Splitting train and test 
train <- mydata[mydata$isTrain==1,]
test <- mydata[mydata$isTrain==0,]
smp_size <- floor(0.75 * nrow(train))

# Skewness
library('e1071')
Column_classes <- sapply(names(mydata),function(x){class(mydata[[x]])})
numeric_columns <-names(Column_classes[Column_classes != "factor"])

#determining skew of each numeric variable
skew <- sapply(numeric_columns,function(x){skewness(mydata[[x]],na.rm = T)})

# Let us determine a threshold skewness and transform all variables above the treshold.
skew <- skew[skew > 0.75]

# transform excessively skewed features with log(x + 1)
for(x in names(skew)) 
{
  mydata[[x]] <- log(mydata[[x]] + 1)
}

# setting the seed to make the partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(train)), size = smp_size)

train_new <- train[train_ind, ]
validate <- train[-train_ind, ]
train_new <- subset(train_new,select=-c(Id,isTrain))
validate <- subset(validate,select=-c(Id,isTrain))
nrow(train_new)

# Random Forest
library(randomForest)
ran_model <- randomForest(SalePrice~.,
                            data = train_new)
prediction <- predict(ran_model,test)
summary(prediction)
plot(prediction)

# Variable Importance
var_importance <- importance(ran_model)
varImpPlot(ran_model)

# Evaluation RMSE function
RMSE <- function(x,y){
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}

RMSE1 <- RMSE(prediction, validate$SalePrice)
RMSE1

RMSE1 <- round(RMSE1, digits = 5)

prediction[which(is.na(prediction))] <- mean(prediction,na.rm=T)
submit <- data.frame(Id=test$Id,SalePrice=prediction)
write.csv(submit,file="House_Price.csv",row.names=F)

# Linear Regression
ggplot(train_new,aes(y=SalePrice,x=GrLivArea))+geom_point()
linear_model <- lm(SalePrice ~ GrLivArea, data = train_new)
summary(linear_model)
plot(linear_model)

