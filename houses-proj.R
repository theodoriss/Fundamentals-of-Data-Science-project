setwd("D:\\sapienza\\bressan project")
#setwd("D:/DataScience/FDS_18/2019")
#setwd("C:\\Users\\Alessandra\\Documents\\DataScience\\1year\\fds-project\\fds-project")

#NN libraries
library(VIM)
library(dplyr)
library(keras)
library(caret)
library(tidyverse)
library(moments)
#LR libraries
library(corrplot)


houseTrain=read.csv("train.csv",stringsAsFactors = F)
houseTest= read.csv("test.csv",stringsAsFactors = F)

houseTest$SalePrice<- NA

house<-rbind(houseTrain,houseTest)

# transform the ExterCond variable in numeric, since it has 2 new levels in test set. We channelled into the already known train set levels values
houseTrain$ExterCond[houseTrain$ExterCond == "Po"] <- 1
houseTrain$ExterCond[houseTrain$ExterCond == "Fa"] <- 2
houseTrain$ExterCond[houseTrain$ExterCond == "TA"] <- 3
houseTrain$ExterCond[houseTrain$ExterCond == "Gd"] <- 4
houseTrain$ExterCond[houseTrain$ExterCond == "Ex"] <- 5

houseTrain$ExterCond= as.numeric(as.character(houseTrain$ExterCond))

# create a new column, total surface, as the sum of all surfaces(basement+1st floor+2nd floor)

houseTest$TotalSF = houseTest$TotalBsmtSF+houseTest$X1stFlrSF + houseTest$X2ndFlrSF
tempTrain = houseTrain$TotalBsmtSF+houseTrain$X1stFlrSF + houseTrain$X2ndFlrSF
SPtemp <- houseTrain$SalePrice
houseTrain$SalePrice <- NULL
houseTrain$TotalSF <- tempTrain
houseTrain$SalePrice <- SPtemp


# if we dont have the year of garage built, we replace it with the year built of the house or if it s NA, with YearRemodAdd
# its clear from the plots that garage year built is the same with either year built or yearremodadd
houseTrain[which(houseTrain$GarageYrBlt == "None"),] %>% dplyr::select(YearBuilt, YearRemodAdd,GarageYrBlt)


for (i in c("Alley", "Fence", "PoolQC","FireplaceQu", "MiscFeature","BsmtQual","BsmtCond","BsmtExposure",
            "BsmtFinType2","GarageType","GarageFinish","GarageQual","GarageCond","MasVnrType", "BsmtFinType1")){
  house[,i]<-house[,i] %>%  replace_na("None")
}

checkNAs <- function(df){
  # identify columns (by column number) with missing values
  naCols <- which(colSums(is.na(df))>0)
  # get columns with missing values sorted by number of missing values
  sort(colSums(sapply(house[naCols],is.na)), decreasing = TRUE)
}

checkNAs(house)

allSet <- house
# get median of lot frontage for each neighborhood
medNgbrLotFr <- allSet[!is.na(allSet$Neighborhood), c("Neighborhood","LotFrontage")] %>% 
  group_by(Neighborhood) %>% 
  summarize(median = median(LotFrontage, na.rm = T))
medNgbrLotFr

# replace missing lot frontage values with the median of lot frontage with the same neighborhood
rIndx <- which(is.na(allSet$LotFrontage))

# run a vlookup-like loop to get median value for each missing value by linkage to neighborhood
for(i in rIndx){
  medVal <- medNgbrLotFr[medNgbrLotFr$Neighborhood == allSet$Neighborhood[i],"median"]
  allSet$LotFrontage[i] <- medVal[[1]]
}

allSet[is.na(allSet$MasVnrType) | is.na(allSet$MasVnrArea), c("MasVnrType","MasVnrArea")]

#Out of 24 houses, we have 23 with missing values in both features. So, we# ll replace missing values where both are missing in the house by *None* in the *MasVnrType* and by zero in the *MasVnrArea*.

allSet$MasVnrType[is.na(allSet$MasVnrType) & is.na(allSet$MasVnrArea)] <- "None"
allSet$MasVnrArea[is.na(allSet$MasVnrArea)] <- 0

#For the house with missing type but with recorded area, we will use the median of Masonry areas across the data to estimate its masonry type.


missArea <- allSet$MasVnrArea[is.na(allSet$MasVnrType)]
allSet[allSet$MasVnrType != "None", c("MasVnrType","MasVnrArea")] %>% 
        ggplot(aes(x = MasVnrType, y = MasVnrArea)) +
        geom_boxplot()+
        geom_hline(yintercept = missArea, color = "red")
medMason <- allSet[allSet$MasVnrType != "None", c("MasVnrType","MasVnrArea")] %>% 
        group_by(MasVnrType) %>% 
        summarize(median = median(MasVnrArea))
medMason

rIndx <- which(is.na(allSet$MasVnrType))

for(i in rIndx){
  medVal <- medMason[which((abs(medMason$median - allSet$MasVnrArea[i])) == min(abs(medMason$median - allSet$MasVnrArea[i]), na.rm = T)),"MasVnrType"]
  allSet$MasVnrType[i] <- medVal[[1]]
}

allSet[is.na(allSet$MSZoning), c("MSSubClass","MSZoning")]

missZoning <- unique(allSet$MSSubClass[is.na(allSet$MSZoning)])
allSet[!is.na(allSet$MSZoning) & allSet$MSSubClass %in% missZoning, c("MSZoning","MSSubClass")] %>% 
  ggplot(aes(x = MSZoning, fill = factor(MSSubClass)))+
  geom_histogram(stat = "count")

allSet$MSZoning[is.na(allSet$MSZoning) & allSet$MSSubClass %in% c(70,30)] <- "RM"
allSet$MSZoning[is.na(allSet$MSZoning) & allSet$MSSubClass == 20] <- "RL"

allSet[is.na(allSet$SaleType), c("SaleType","SaleCondition")]

allSet[!is.na(allSet$SaleType) & !is.na(allSet$SaleCondition), c("SaleType","SaleCondition")] %>% 
  ggplot(aes(x = SaleType, fill = factor(SaleCondition)))+
  geom_histogram(stat = "count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

allSet$SaleType[is.na(allSet$SaleType)] <- "WD"

allSet[is.na(allSet$Electrical), c("OverallCond","Electrical")]

allSet[!is.na(allSet$Electrical), c("OverallCond","Electrical")] %>% 
  ggplot(aes(x = factor(OverallCond), fill = factor(Electrical)))+
  geom_histogram(stat = "count")

allSet$Electrical[is.na(allSet$Electrical)] <- "SBrkr"

allSet[is.na(allSet$KitchenQual), c("KitchenAbvGr","KitchenQual")]

allSet[!is.na(allSet$KitchenQual), c("KitchenAbvGr","KitchenQual")] %>% 
  ggplot(aes(x = KitchenAbvGr, fill = factor(KitchenQual)))+
  geom_histogram(stat = "count")

allSet$KitchenQual[is.na(allSet$KitchenQual)] <- "TA"

allSet[is.na(allSet$PoolQC),c("PoolQC","PoolArea")] %>% 
  group_by(PoolArea) %>% 
  summarize(count = n())



allSet$PoolQC[allSet$PoolArea == 0] <- "None"


#However, we have three houses with missing PoolQC values while having non-zero PoolAreas.

allSet[is.na(allSet$PoolQC) & allSet$PoolArea >0,c("PoolQC","PoolArea")]

#Since PoolArea is a likely predictor to PoolQC, we can impute the missing PoolQC values with corresponding values of other houses having means of PoolAreas near to those of the missing PoolQC values.

# calculate the means of PoolAreas for each of the PoolQC in the data set
meanArea <- allSet[!is.na(allSet$PoolQC),c("PoolQC","PoolArea")] %>% 
  group_by(PoolQC) %>% 
  summarize(AreaMean = round(mean(PoolArea),0))
meanArea

#Impute missing PoolQC values with the values of nearest PoolArea means.

rIndx <- which(is.na(allSet$PoolQC) & allSet$PoolArea >0)

for(i in rIndx){
  poolQc <- meanArea[which((abs(meanArea$AreaMean - allSet$PoolArea[i])) == min(abs(meanArea$AreaMean - allSet$PoolArea[i]), na.rm = T)),"PoolQC"]
  allSet$PoolQC[i] <- poolQc[[1]]
}

#* **Garage** features: By checking the Garage-related features with missing values, we notice that we have 7 features all related to garage and have maximum number of missing values of 159 in *GarageYrBlt* column.

# identify all Garage features as those starting with Garage text
garageCols <- names(allSet)[grepl("Garage.*", names(allSet))]
garageCols


#By studying garage features description in the *data_description.txt* file, we see that garage features: *GarageType*, *GarageFinish*, *GarageQual*, and *GarageCond* all have *NA* values to indicate No-Garage rather than a missing value recorded. Yet, we need to change the *NAs* to *None* for these features, and to zero for the integer-type features: *GarageYrBlt*, * GarageCars* and * GarageArea*. This is to avoid problems in modeling the data.

#First, we will consider houses with a complete set of *NAs* or zeros- as explained above- as houses with *None* values.


# find indexes of houses having all Garage features flagged with NAs or zeros
noGarage <- which((is.na(allSet$GarageArea) | allSet$GarageArea == 0)
                  & (is.na(allSet$GarageCars) | allSet$GarageCars == 0)
                  & is.na(allSet$GarageCond)
                  & is.na(allSet$GarageFinish)
                  & is.na(allSet$GarageQual)
                  & is.na(allSet$GarageType)
                  & (is.na(allSet$GarageYrBlt) | allSet$GarageYrBlt == 0))


allSet[noGarage,garageCols]


# impute with NAs or zeros
allSet[noGarage,c("GarageType","GarageFinish","GarageQual","GarageCond")] <- "None"
allSet[noGarage, c("GarageYrBlt","GarageCars","GarageArea")] <- 0


#Let s see all houses with at least one garage feature having a missing value. Such houses do not indicate a house with no garage, and their missing values need to be imputed with reasonable values.


# find indexes of houses having at least one missing Garage value
missGarage <- which(is.na(allSet$GarageArea) 
| is.na(allSet$GarageCars)
| is.na(allSet$GarageCond)
| is.na(allSet$GarageFinish)
| is.na(allSet$GarageQual)
| is.na(allSet$GarageType)
| is.na(allSet$GarageYrBlt))
allSet[missGarage,garageCols]

allSet[missGarage,"GarageYrBlt"] <- allSet[missGarage,"YearBuilt"]


#House *2577* is missing more values than house *2127*. Hence, we will impute its missing values first based on the matching values of houses of the same *GarageType* and *GarageYrBlt*.


# index houses with missing garageCars or area
miss <- which(is.na(allSet$GarageCars) | is.na(allSet$GarageArea))

# group houses by garage features
grpdVals1 <- allSet%>% 
group_by(GarageType, GarageYrBlt, GarageFinish, GarageQual, GarageCond) %>% 
summarize(medCars = median(GarageCars), meanArea = round(mean(GarageArea),0),count = n()) %>% 
arrange(desc(count))
comp <- complete.cases(grpdVals1)
grpdVals1 <- grpdVals1[comp,]
grpdVals1

# look-up missing values based on houses with the same type and year built
rIndx <- miss
for(i in rIndx){
missVals <- grpdVals1[which((grpdVals1$GarageYrBlt == allSet$GarageYrBlt[i]) & (grpdVals1$GarageType == allSet$GarageType[i])),c("GarageFinish", "medCars", "meanArea", "GarageQual","GarageCond")]
allSet$GarageFinish[i] <- missVals[[1]][1]
allSet$GarageCars[i] <- missVals[[2]][1]
allSet$GarageArea[i] <- missVals[[3]][1]
allSet$GarageQual[i] <- missVals[[4]][1]
allSet$GarageCond[i] <- missVals[[5]][1]
}


#House *2127* has a garage with and area of 360 square feet and 1-GarageCars value. So, we will impute its missing values (*GarageFinish*, *GarageQual* and *GarageCond*) with those having similar *GarageArea*, *GarageCars*, and *GarageType*.


grpdVals <- allSet[allSet$GarageType == "Detchd" & allSet$GarageCars == 1,garageCols] %>% 
group_by(GarageFinish, GarageQual, GarageCond) %>% 
summarize(meanArea = round(mean(GarageArea),0),count = n()) %>% 
arrange(desc(count))
comp <- complete.cases(grpdVals)
grpdVals <- grpdVals[comp,]
grpdVals


#Impute missing values with the nearest values found in the table above.


rIndx <- missGarage

for(i in rIndx){
missVals <- grpdVals[which((abs(grpdVals$meanArea - allSet$GarageArea[i])) == min(abs(grpdVals$meanArea - allSet$GarageArea[i]),na.rm = T)),c("GarageFinish", "GarageQual","GarageCond")]
allSet$GarageFinish[i] <- missVals[[1]][1]
allSet$GarageQual[i] <- missVals[[2]][1]
allSet$GarageCond[i] <- missVals[[3]][1]
}

# identify all basement features as those containing Bsmt text
bsmtCols <- names(allSet)[grepl("Bsmt.*", names(allSet))]
bsmtCols

#First, let s check missing values in all Basement features. Assuming that if a house has *NA* values in all basement features, then the house has no basement. Basement values of such houses will be replaced by *None* for string features and by zero for numeric features.

# check all Bsmt houses having all Bsmt features NAs or zeros
bsmtAllNa <- which((allSet[,bsmtCols[1]]== 0 | is.na(allSet[,bsmtCols[1]])) &
                     (allSet[,bsmtCols[2]]== 0 | is.na(allSet[,bsmtCols[2]])) & 
                     (allSet[,bsmtCols[3]]== 0 | is.na(allSet[,bsmtCols[3]])) &
                     (allSet[,bsmtCols[4]]== 0 | is.na(allSet[,bsmtCols[4]])) &
                     (allSet[,bsmtCols[5]]== 0 | is.na(allSet[,bsmtCols[5]])) &
                     (allSet[,bsmtCols[6]]== 0 | is.na(allSet[,bsmtCols[6]])) &
                     (allSet[,bsmtCols[7]]== 0 | is.na(allSet[,bsmtCols[7]])) &
                     (allSet[,bsmtCols[8]]== 0 | is.na(allSet[,bsmtCols[8]])) &
                     (allSet[,bsmtCols[9]]== 0 | is.na(allSet[,bsmtCols[9]])) &
                     (allSet[,bsmtCols[10]]== 0 | is.na(allSet[,bsmtCols[10]])) &
                     (allSet[,bsmtCols[11]]== 0 | is.na(allSet[,bsmtCols[11]]))
)
allSet[bsmtAllNa,bsmtCols]

#e have 79 houses that can be flagged as not having basements.

# flag the non-basement houses with "No Basement" for string bsmt features and with zeros
# for numeric bsmt feautres
allSet[bsmtAllNa,c("BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2")] <- "None"
allSet[bsmtAllNa, c("BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "BsmtFullBath", "BsmtHalfBath")] <- 0


#Now, let s check remaining basement houses which have missing values but seem to have basements based on at least one non-NA basement feature.

 
bsmtSmNa <- which(is.na(allSet[,bsmtCols[1]]) |
is.na(allSet[,bsmtCols[2]]) | 
is.na(allSet[,bsmtCols[3]]) | 
is.na(allSet[,bsmtCols[4]]) |
is.na(allSet[,bsmtCols[5]]) | 
is.na(allSet[,bsmtCols[6]]) |
is.na(allSet[,bsmtCols[7]]) | 
is.na(allSet[,bsmtCols[8]]) | 
is.na(allSet[,bsmtCols[9]]) |
is.na(allSet[,bsmtCols[10]])| 
is.na(allSet[,bsmtCols[11]]))
bsmtNa <- allSet[bsmtSmNa,bsmtCols]
bsmtNa
  


sum( allSet$TotalBsmtSF != (allSet$BsmtFinSF1 + allSet$BsmtFinSF2 + allSet$BsmtUnfSF))
  

#All basement areas numbers are correct and according to the formula.

#We have one house (333) whose *BsmtFinType2* value is missing while its *BsmtFinSF2* is 479 square feet. #Let s see the distribution of all *BsmtFinType2* values versus *BsmtFinSF2*.

  
# check all houses having non-zero BsmtFinSF2 versus their BsmtFinType2
missFinSF2 <- bsmtNa$BsmtFinSF2[is.na(bsmtNa$BsmtFinType2)]
bsmt2 <- allSet[allSet$BsmtFinSF2 >0 & !is.na(allSet$BsmtFinType2), c("BsmtFinSF2", "BsmtFinType2")]
bsmt2 %>% ggplot(aes(x = BsmtFinType2, y = BsmtFinSF2))+
geom_boxplot()+
geom_hline(yintercept = missFinSF2, color = "red")
# calculate medians of finished square feet for each bsmt finish type 2
medSF <- bsmt2 %>% 
group_by(BsmtFinType2) %>% 
summarize(median = median(BsmtFinSF2))
medSF
  

 
rIndx <- which(is.na(allSet$BsmtFinType2))

for(i in rIndx){
  bsmtVal <- medSF[which((abs(medSF$median - allSet$BsmtFinSF2[i])) == min(abs(medSF$median - allSet$BsmtFinSF2[i]), na.rm = T)),"BsmtFinType2"]
  allSet$BsmtFinType2[i] <- bsmtVal[[1]]
}
  

#For basement exposure (*BsmtExposure*), we have 3 missing values. Let s see if there s any relationship between basement types and exposure.

  
allSet[!is.na(allSet$BsmtExposure), c("BsmtExposure","BsmtFinType1")] %>% 
  ggplot(aes(x = BsmtExposure, fill = BsmtFinType1))+
  geom_histogram(stat = "count")
  


 
allSet$BsmtExposure[is.na(allSet$BsmtExposure)] <- "No"
  

#Now, we are left with missing values in basement quality and basement condition. Let s see if there s some relationship between basement quality (which refers to its height) and the exposure.

allSet[!is.na(allSet$BsmtQual), c("BsmtQual","BsmtExposure")] %>% 
ggplot(aes(x = BsmtQual, fill = BsmtExposure))+
geom_histogram(stat = "count")
  


 
allSet$BsmtQual[is.na(allSet$BsmtQual)] <- "TA"
  

# relationship between basement quality and condition.

allSet[!is.na(allSet$BsmtQual) & !is.na(allSet$BsmtCond), c("BsmtQual","BsmtCond")] %>% 
ggplot(aes(x = BsmtQual, fill = BsmtCond))+
geom_histogram(stat = "count")
  
#It#s obvious from the plot that the majority of *TA* and *Gd* quality have *TA* basement condition. So, we ll use *TA* basement condition for the missing values.

 
allSet$BsmtCond[is.na(allSet$BsmtCond)] <- "TA"
  
checkNAs(allSet)
  


allSetNew <- house

# get classes of features in the data set
featureClasses <- sapply(names(allSetNew), function(x){class(allSetNew[[x]])})

# get numeric or integer class features
numFeatures <- names(featureClasses[featureClasses == "numeric" | featureClasses == "integer"])

# get character class features
charFeatures <- names(featureClasses[featureClasses == "character"])

# determine skewness of each numeric feature
skewedVals <- sapply(numFeatures, function(x){skewness(allSetNew[[x]],na.rm = T)})

# identify skewed features with threshold of -2,+2
skewedFeatures <- skewedVals[skewedVals < -2 | skewedVals > 2]

# log-transform skewed features
for (i in names(skewedFeatures)) {
  allSetNew[[i]] <- log(allSetNew[[i]] + 1)
}


## Convert character features into dummy variables
 

#install.packages("caret")
library(caret)
# create a full set of dummy variables for the character features
dummies <- dummyVars(~., allSetNew[charFeatures])
dummyVariables <- predict(dummies, allSetNew[charFeatures])

# compile the data set again by combining both numerical and dummy variables features
allSetNew <- cbind(allSetNew[numFeatures], dummyVariables)
  

# Model Training and Testing

#Now, having all features processed and engineered, we can start the model training then testing with all existing features (except the *Id*) as model predictors.

#First, we will re-split the full data set into its original *train* and *test* sets.

 
salesPriceNA <- which(is.na(allSetNew["SalePrice"]))
train <- allSetNew[-salesPriceNA,]
test <- allSetNew[salesPriceNA,]


# convert the train and test data sets into matrices excluding the Id and SalePrice features

train.Matrix =as.matrix(train[,names(train) != c("Id","SalePrice")])
test.Matrix = as.matrix(test[,names(test) != c("Id","SalePrice")])

# create a vector of the log-transformed response (SalePrice)
train.y = log(train$SalePrice + 1)


set.seed(5)
# split the train data set into training and testing data sets (75/25 ratio).
# number of training rows
nTrain <- round(0.75 * nrow(train.Matrix))

# sample row IDs
sampleTrain <- sample(nrow(train.Matrix),nTrain)

# create training and testing data sets
training <- train.Matrix[sampleTrain,]
testing <- train.Matrix[-sampleTrain,]
training.y <- train.y[sampleTrain]
testing.y <- train.y[-sampleTrain]
  

salesPriceNA <- which(is.na(allSetNew["SalePrice"]))
train <- allSetNew[-salesPriceNA,]
test <- allSetNew[salesPriceNA,]

train$SalePrice <- log(train$SalePrice + 1)



training <- as.data.frame(training)
testing <- as.data.frame(testing)


sum(is.na(train))
train <- kNN(train)
full_model <- lm(SalePrice ~ OverallQual+GrLivArea+GarageArea+YearBuilt+OverallCond+WoodDeckSF     
+GarageCars+Fireplaces+BsmtFinSF1+OpenPorchSF+MSSubClass+LotArea+ScreenPorch    
+YrSold , data = train)

sum(is.na(testing))
testing2 <- kNN(testing)


test.predict.step <- predict(full_model,newdata = testing2)

sum(is.infinite(test.predict.step))

-------------------------------NN--------------------------------

  
dmy2 <- dummyVars(" ~ .", data = allSet)
trsf2 <- data.frame(predict(dmy2, newdata = allSet))


#norm=scale(noume)
#norm=as.data.frame(norm)

#norm$Id= house$Id
cmplt <- kNN(trsf2)

nomissing= cmplt%>%select(2:303)


train_nn= nomissing[1:1460,]
test_nn=nomissing[1461:2919,]


library(reticulate)
modello <- keras_model_sequential()
modello%>%
  layer_dense(units=128,activation = 'relu',input_shape = c(302)) %>%
  layer_dropout(rate=0.1)                   %>%
  #layer_dense(units=128,activation = 'relu') %>%
  #layer_dropout(rate=0.1)                   %>%
  #layer_dense(units=64,activation = 'relu') %>%
  #layer_dropout(rate=0.05)                   %>%
  layer_dense(units=1)


modello%>% compile(loss='mse',optimizer='adam',metrics='mae')

housePrice<-houseTrain$SalePrice

modello$optimizer$lr=0.001

train_nn=as.matrix(train_nn)
mymodel<-modello%>%
  fit(train_nn,
      housePrice,
      epochs=1000,
      batch_size=10,
      validation_split=0.2)

test_nn=as.matrix(test_nn)

pred_nn<- modello%>% predict(test_nn)

#------------------------END NN -------------------------------------------------------

sum(is.na(allSet))
allSet <- as.data.frame(unclass(allSet))

nomissing=kNN(allSet)
nomissing= nomissing%>%select(2:80)

library(e1071)

train_svm=nomissing[1:1460,]
test_svm=nomissing[1461:2919,]


price=houseTrain$SalePrice

model.svm <- svm(price ~ ., data = train_svm, cost = 3,epsilon=0.1) #just try it. deafult value is 0.1


price.svm = predict(model.svm, test_svm)

#solution <- data.frame(Id = as.integer(rownames(test)),SalePrice =  as.numeric(price.svm*.98+test.predict.step*.01+pred_nn*.01)) 
solution <- data.frame(Id = as.integer(rownames(test)),SalePrice =  as.numeric(price.svm))

check = cbind(solution,as.integer(houseTest$Id))

colnames(check)=c('Id', 'SalePrice')
teliko=check[,c(2,1)]

write.csv(teliko, file = "ensemble-lm-svm-NN.csv",row.names=FALSE)
