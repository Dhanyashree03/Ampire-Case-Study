
# Package loading
library(car)
library(DescTools)
library(corrplot)

getwd()
# Set Working Directory
setwd("/Users/dhanyashreegowda/Desktop/github/ampire_case_study")
getwd() # Check/ Confirm if the directory has changed
# Read the data file
rawDf = read.csv("TRAINING.csv",stringsAsFactors = TRUE)
predictionDf = read.csv("TEST.csv", stringsAsFactors = TRUE)
# View the data
View(rawDf) # "Value" is our "dependent variable"
View(predictionDf)

# Check for NAs
colSums(is.na(rawDf))
colSums(is.null(rawDf))


############################
# Sampling: Divide the data into TrainRaw and TestRaw
############################
# Before we do anything, we need to divide our Raw Data into train and test sets (validation)

# Divide trainDf further into trainDf and testDf by random sampling
set.seed(123) # This is used to reproduce the SAME composition of the sample EVERYTIME
RowNumbers = sample(x = 1:nrow(rawDf), size = 0.80*nrow(rawDf)) # 80%-20% split
head(RowNumbers)  # Printing the first 6 randomly picked numbers

# head(RowNumbers)
trainDf = rawDf[RowNumbers, ] # Trainset
testDf = rawDf[-RowNumbers, ] # Testset

# checking the dimension rows and coloums are equal or not 
dim(trainDf)
dim(testDf)
dim(predictionDf)

predictionDf$Value=""  #added a new column of value

colSums(is.na(trainDf))
colSums(is.na(predictionDf))

# Create Source Column in both trainDf and testDf
trainDf$Source = "Train"
testDf$Source = "Test"
predictionDf$Source = "Prediction"


# Combine trainDf , testDf, predictionDf
fullDf = rbind(trainDf, testDf, predictionDf)

View(fullDf)

#  drop "Item.code" column from the data as it is not going to assist us in our model
fullDf$Item.Code = NULL 
fullDf$Quality.Code = NULL
fullDf$Date = NULL
fullDf$Month = NULL
fullDf$State = NULL


# Validate the deletion
dim(fullDf) # Should be 1 column less
colnames(fullDf) # Should not have the "Item.code" column


# Check the summary of the file
summary(fullDf)
str(fullDf)
# changing values into numeric
fullDf$Value = as.numeric(as.character(fullDf$Value))



# Check for NAs
colSums(is.na(fullDf))

# Variables having missing value AD4.Sqm , AD5.Sqm, AD6.Sqm, Total.AD.Sqm, Billing.Rate.Sqm ,Buyer.Rate.Sqm 

# AD4.Sqm

# Step 1: Find median
median(trainDf$AD4.Sqm) # Inference (WILL ALWAYS BE FROM TRAIN DATA)
tempMedian = median(trainDf$AD4.Sqm, na.rm = TRUE)
tempMedian

# Step 2: Find missing value rows
missingValueRows = is.na(fullDf[, "AD4.Sqm"])
sum(missingValueRows)

# Step 3: Impute (or Fill) missing values in data
fullDf[missingValueRows, "AD4.Sqm"] = tempMedian # Action/ Manipulation (WILL ALWAYS BE ON FULL DATA)
colSums(is.na(fullDf))

# AD5.Sqm

# Step 1: Find median
median(trainDf$AD5.Sqm) # Inference (WILL ALWAYS BE FROM TRAIN DATA)
tempMedian = median(trainDf$AD5.Sqm, na.rm = TRUE)
tempMedian

# Step 2: Find missing value rows
missingValueRows = is.na(fullDf[, "AD5.Sqm"])
sum(missingValueRows)

# Step 3: Impute (or Fill) missing values in data
fullDf[missingValueRows, "AD5.Sqm"] = tempMedian # Action/ Manipulation (WILL ALWAYS BE ON FULL DATA)
colSums(is.na(fullDf))

# AD6.Sqm

# Step 1: Find median
median(trainDf$AD6.Sqm) # Inference (WILL ALWAYS BE FROM TRAIN DATA)
tempMedian = median(trainDf$AD6.Sqm, na.rm = TRUE)
tempMedian

# Step 2: Find missing value rows
missingValueRows = is.na(fullDf[, "AD6.Sqm"])
sum(missingValueRows)

# Step 3: Impute (or Fill) missing values in data
fullDf[missingValueRows, "AD6.Sqm"] = tempMedian # Action/ Manipulation (WILL ALWAYS BE ON FULL DATA)
colSums(is.na(fullDf))

# Total.AD.Sqm

# Step 1: Find median
median(trainDf$Total.AD.Sqm) # Inference (WILL ALWAYS BE FROM TRAIN DATA)
tempMedian = median(trainDf$Total.AD.Sqm, na.rm = TRUE)
tempMedian

# Step 2: Find missing value rows
missingValueRows = is.na(fullDf[, "Total.AD.Sqm"])
sum(missingValueRows)

# Step 3: Impute (or Fill) missing values in data
fullDf[missingValueRows, "Total.AD.Sqm"] = tempMedian # Action/ Manipulation (WILL ALWAYS BE ON FULL DATA)
colSums(is.na(fullDf))

# Billing.Rate.Sqm

# Step 1: Find median
median(trainDf$Billing.Rate.Sqm) # Inference (WILL ALWAYS BE FROM TRAIN DATA)
tempMedian = median(trainDf$Billing.Rate.Sqm, na.rm = TRUE)
tempMedian

# Step 2: Find missing value rows
missingValueRows = is.na(fullDf[, "Billing.Rate.Sqm"])
sum(missingValueRows)

# Step 3: Impute (or Fill) missing values in data
fullDf[missingValueRows, "Billing.Rate.Sqm"] = tempMedian # Action/ Manipulation (WILL ALWAYS BE ON FULL DATA)
colSums(is.na(fullDf))

# Buyer.Rate.Sqm

# Step 1: Find median
median(trainDf$Buyer.Rate.Sqm) # Inference (WILL ALWAYS BE FROM TRAIN DATA)
tempMedian = median(trainDf$Buyer.Rate.Sqm, na.rm = TRUE)
tempMedian

# Step 2: Find missing value rows
missingValueRows = is.na(fullDf[, "Buyer.Rate.Sqm"])
sum(missingValueRows)

# Step 3: Impute (or Fill) missing values in data
fullDf[missingValueRows, "Buyer.Rate.Sqm"] = tempMedian # Action/ Manipulation (WILL ALWAYS BE ON FULL DATA)
colSums(is.na(fullDf))


summary(fullDf)

############################
# Correlation check
############################

library(corrplot)
continuous_variable_check = function(x)
{
  return(is.numeric(x) | is.integer(x))
}

continuousVars = sapply(fullDf, continuous_variable_check)
continuousVars

corrDf = cor(fullDf[fullDf$Source == "Train", continuousVars])
View(corrDf)

# didnot use window as it showed perfectly in my screen 
window()
corrplot(corrDf)


############################
# Dummy variable creation/ one hot encoding
############################

factorVars = sapply(fullDf, is.factor)
factorVars
dummyDf = model.matrix(~ ., data = fullDf[,factorVars])
View(dummyDf)
colnames(dummyDf)

dim(dummyDf)
fullDf2 = cbind(fullDf[,!factorVars], dummyDf[,-1])
View(fullDf2)

# Check the dimensions of fullDf2
dim(fullDf2)

# Check if all variables are now numeric/integer
str(fullDf2) 

############################
# Divide the fullDf2 back into trainDf, testDf, predictionDf
############################


# Step 1: Divide trainDf into trainDf and testDf
trainDf = subset(fullDf2, subset = fullDf2$Source == "Train", select = -Source)
testDf = subset(fullDf2, subset = fullDf2$Source == "Test", select = -Source)
predictionDf = subset(fullDf2, subset = fullDf2$Source == "Prediction", select = -Source)

############################
# Multicollinearity check
############################

# Remove variables with VIF > 5



M1= lm(Value ~ ., data = testDf)

library(car)
vif(M1)

#  Aliased coefficents error #
alias(lm(Value ~ ., data = testDf))

vif(M1)
# since the model shows perfect collinearity between billing rate & buyer rate#
# removing one of the variables might better the prediction #


traindf$Billing.Rate.Sqm   = NULL
testdf$Billing.Rate.Sqm    = NULL
predictdf$Billing.Rate.Sqm = NULL

#checking of collinearity once again #

M2 = lm(Value ~ ., data = testDf)

vif(M2)
# since the model shows perfect collinearity between buyer rate & mrp.sqm#
# removing one of the variables might better the prediction #

trainDf$MRP..Sqm    = NULL
testDf$MRP..Sqm     = NULL
predictionDf$MRP..Sqm  = NULL 

#checking of collinearity once again #

M3 = lm(Value ~ ., data = trainDf )

VIF(M3)

# since the model shows perfect collinearity between Ad6 & AD7 #
# removing one of the variables might better the prediction #


trainDf$AD6.Sqm = NULL
testDf$AD6.Sqm = NULL
predictionDf$AD6.Sqm = NULL

#checking of collinearity once again #

M4 = lm(Value ~ ., data = trainDf )

VIF(M4)

# in sufficeint collapse of energy in finding aliased coefficients#
# Moving to the step function #

summary(M4)

M5 = step(M4)

# model   diagnostics #
# homoskadasticity check #

plot(M5$fitted.values,M5$residuals)

#Normality check

summary(M5$residuals)   # for range detection #

#plotting histogram #

hist(M5$residuals, breaks =  seq(-1400, 1600, 20))

#Prediction on test data #

M5_Pred = Predict(M5, testDf)

head(M5_Pred)


#Rmse Calculation #
Actual= testDf$Value
Prediction = M5_Pred

sqrt(mean((Actual - Prediction)^2))


#MAPE

mean(abs((Actual - Prediction)/Actual))*100



#Predicting on predictionDf

predictionDf$Value = predict(M5, predictionDf)


#Extracting the datafile
install.packages("writexl")
library("writexl")
write_xlsx(predictdf,"/Users/dhanyashreegowda/Desktop/github/ampire_case_study/predict.xlsx")


