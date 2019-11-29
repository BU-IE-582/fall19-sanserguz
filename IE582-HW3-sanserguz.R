# IE 582
# HW 3

#Task A

setwd("~/Downloads/UniStuff/IE 582/HW3")

# I used consumption data from 01.01.2016 to 20.11.2019.
# Data up to 01.11.2019 are used as training set while the remaining 20 days are used for test.

data <- read.csv(file="RealTimeConsumption-01012016-20112019.csv",header=TRUE)

# I prepared the data before starting iterations
#   Format of Hour column changed from character to numeric value
#   Name of the Consumption column is corrected self explanatory
#   Data checked for NA values
#   Comma used for thousands separator removed for Consumption feature and numeralized
#   I decided to use indices instead of dates so Date column remained untouched

data[,2]=as.numeric(data[,2])-1
names(data)[3] = "Consumption"
data <- na.omit(data)
data$Consumption<-as.numeric(sub(",", "",data$Consumption,fixed = TRUE))

# In order to generate 48 and 168 hour naive predictions, Consumption values 48 and 168 hours prior to test period was extracted from data and binded together.
# MAPE values were calculated and binded to the same matrix. 
# MAPE statistics plotted for both approaches.

task_a_naive48 = cbind(tail(data, n=24*20),Naive_48=(head(tail(data,n=24*20+48),24*20))$Consumption)
task_a_naive48 = cbind(task_a_naive48,MAPE = 100*abs((task_a_naive48$Consumption-task_a_naive48$Naive_48)/task_a_naive48$Consumption))

task_a_naive168 = cbind(tail(data, n=24*20),Naive_168=(head(tail(data,n=24*20+168),24*20))$Consumption)
task_a_naive168 = cbind(task_a_naive168,MAPE = 100*abs((task_a_naive168$Consumption-task_a_naive168$Naive_168)/task_a_naive168$Consumption))

par(mfrow=c(1,2))

plot(task_a_naive48$MAPE, type='l', ylab ="MAPE", main="MAPE for Naive 48")
plot(task_a_naive168$MAPE, type='l', ylab ="MAPE", main="MAPE for Naive 168")

par(mfrow=c(1,1))

plot(task_a_naive48$MAPE-task_a_naive168$MAPE, type='l',ylab ="MAPE", main="MAPE difference (Naive 48 - Naive 168)")
abline(h=0, col='red')
# We see that Naive168 is better than Naive48 in terms of MAPE accuracy.


#Task B

# Training period is defined as 01.01.2016 - 31.10.2019 (excluding last 20 days)

trainset=head(data, n= 34056-20*24)

# 48 and 168 hours lag values for each row is extracted and matched using an initial buffer of zero values.
# Note that since we do not have required lag information for the first week of training period.

lag48 = c(seq(0,0,length.out=48),head(trainset,n=34056-20*24-48)$Consumption)
lag168 = c(seq(0,0,length.out=168),head(trainset,n=34056-20*24-168)$Consumption)
trainset = cbind(trainset, lag48, lag168)


# A linear repression model was fitted using lm function.
# Consumption is used as response while lag48 and lag168 are used as predictors.
# Note that I exluded first 168 rows (first week of training set) while fitting since required predictors are not available. We do not have 168 hours lag info for first 7 days and 48 hours lag info for first 2 days.
# If we were to use the first week too, model coefficients would be distorted to incorporate 0 valued predictors of the first week as well.

# Summary shows that model can explain 76.27 % of the variance and the predictors are indeed statistically significant.

lagfit = lm(Consumption ~ lag48 + lag168, data=tail(trainset,n=33576-168))
summary(lagfit)

# Last 20 days (20*24 hours in total) are taken as the testing set. Their matching lag values were extracted from data and binded together.
# Predictions are obtained using predict function.

testset=tail(data, n= 24*20)
testset=cbind(testset,lag48=(head(tail(data,n=24*20+48),24*20))$Consumption,lag168=(head(tail(data,n=24*20+168),24*20))$Consumption)

testset_lag=cbind(testset,forecast = predict(lagfit,testset))

# MAPE values are calculated, binded and plotted just like previous part.

testset_lag$MAPE = 100*abs((testset_lag$Consumption-testset_lag$forecast)/testset_lag$Consumption)
head(testset_lag)
plot(testset_lag$MAPE,type='l', ylab = "MAPE", main="Linear Model MAPE")


#Task C

# I wrote a for loop to carry out the procedure described in task c.
# In each run:
#   hourlytemp holds the data for a certain hour value.
#   Data is fitted to a linear regression model. (Like task B, first week is excluded)
#   Consumption is used as response while lag48 and lag168 are used as predictors.
#       Note that the first week in training is removed for insufficient lag info.
#   Linear models created each turn are hold temporarily and used for prediction on test set
#   Predictions are recorded accordingly.

testset_hourly = cbind(testset,forecast=0)
hourlyfit =list()
adjr2=vector()

for(i in 1:24){
  hourlytemp = subset(trainset, Hour == i-1)
  hourlyfit[[i]] = lm(Consumption ~ lag48 + lag168,data=tail(hourlytemp,1399-7))
  adjr2[i]=summary(hourlyfit[[i]])$adj.r.squared
  for(j in 1:480){
    if(testset_hourly$Hour[j]==i-1)
     {testset_hourly$forecast[j]= predict(hourlyfit[[i]],testset[j,])}
  }
}

plot(adjr2, pch = 17, col="darkgreen", ylab = "Adjusted R-squared", xlab= "Hourly Linear Model - Hour", main ="Model's Performance in Variability Explaination")


# Adjusted R-squared values are recorded for each model and plotted for model evaluation
# We see that R squared values decrease as it goes from day to night, reaching minimum value mid-day
# This means that models explain the variability in the data better for the night than day
# It can be inferred that night hours are more predictable than day hours in terms of electricity consumption

# MAPE statistic is processed similarly as in previous parts.

testset_hourly$MAPE = 100*abs((testset_hourly$Consumption-testset_hourly$forecast)/testset_hourly$Consumption)
head(testset_hourly)
plot(testset_hourly$MAPE,type='l', ylab = "MAPE", main="Hourly Linear Model MAPE")

 
#Task D
 
library("glmnet")

# After this point, we dont need lag48 and lag168 values in training and testing sets.
 
trainset$lag48=NULL
trainset$lag168=NULL
testset$lag48=NULL
testset$lag168=NULL

# Transformation of data to wide format is required. 
# Wide format will hold lag features from 2 and 7 days ago in addition to default Date, Hour and Consumption features.
# A for loop was written to carry out the extraction of data in order.

wideform=matrix(NA,nrow=34056,ncol=48)

jlist = seq (168,34032,24)
 
for(i in 1:24){
  for(j in jlist){
    wideform[(j+1):(j+24),i]=data[j-48+i,3]
    wideform[(j+1):(j+24),i+24]=data[j-168+i,3]
  }
}


# Extracted wide format data is matched with their respective rows in training and testing set.
# Names of the features are set to show lag it represents with the row's actual date.
# For example, feature 217 of a row has the value of real consumption value of 17th hour of 2 days ago.

trainwideform = cbind(trainset[169:33576,],wideform[169:33576,])
names(trainwideform)[4:27] = seq(200,223,1)
names(trainwideform)[28:51] = seq(700,723,1)
head(trainwideform)

testwideform = cbind(testset,wideform[33577:34056,])
names(testwideform)[4:27] = seq(200,223,1)
names(testwideform)[28:51] = seq(700,723,1)
testwideform$forecast=0
head(testwideform)

# Now, I will train penalized regression (lasso) models for each hour and acquire predictions on test set.
# A for loop is written to carry out: cross validation, construction of lasso models and prediction.
# cv.glmnet function is used for 10 fold cross validation for each hourly model
# minimum lambda values are taken to be used for training lasso models
# lasso models are constructed using consumption level as response, 48 lag value features as predictors and minlambda as regularization parameter and recorded in a list.
# Models are then used to predict consumption values in testing set, hour by hour.

lassomodels=list()
par(mfrow=c(2,3))
minlambda = vector(length=24)

for(i in 1:24){
  set.seed(666)
  hourlytemp = subset(trainwideform, Hour == i-1)
  
  lagsbyhour = as.matrix(hourlytemp[4:51])
  consumption = as.matrix(hourlytemp[3])
  
  cv = cv.glmnet(lagsbyhour,consumption,nfolds = 10)
  minlambda[i]=cv$lambda.min
  plot(cv, main=c("10-Fold CV for Hour",i))
  lassom = glmnet(lagsbyhour,consumption, alpha = 1, lambda = minlambda[i])
  lassomodels[[i]]=lassom
  
  for(j in 1:480){
    if(testwideform$Hour[j]==i-1){
      testwideform$forecast[j]=predict(lassom, s=minlambda[i] ,as.matrix(testwideform[j,4:51]) )
    }
  }
  
   
}

par(mfrow=c(2,3))
for(i in 1:24){
  plot(coefficients(lassomodels[[i]])[2:49], xlab="2DaysLag(24) - 7DaysLag(24)", ylab="Model Coefficients", main=c("Hour",i), pch=19, col="purple")
  abline(v=24.5, col="orangered")
}

# Coefficients other than intercepts are plotted
# We see that most of the model coefficients are aligned around zero and models select a handful number of useful hours and use them as main predictors.
# Rest of the predictors are given model predictors around zero, indicating minimal influence over prediction.
# Also, it can be observed that latter 24 predictors (7 days lags) are more inclined to align around zero while the former 24 (2 days lags) are scattered away from zero.
# This means that lasso models prefer lags from 2 days ago over lags from 7 days ago.

par(mfrow=c(1,1))
testwideform$MAPE = 100*abs((testwideform$Consumption-testwideform$forecast)/testwideform$Consumption)
head(testwideform)
plot(testwideform$MAPE,type='l', ylab = "MAPE", main="Lasso Regression MAPE")


# Task F

boxplot(task_a_naive48$MAPE, task_a_naive168$MAPE, testset_lag$MAPE,
        testset_hourly$MAPE, testwideform$MAPE,
        main = "MAPE Boxplots Across Approaches",
        names = c("Naive 48", "Naive 168", "Linear Model", "Hourly LM", "Hourly Lasso"),
        col = c("cyan2","lightpink","snow","lightpink","cyan2"),
        border = "midnightblue", notch = TRUE, pch=18)

# In this part I will use MAPE statistics as the main performance measure to compare different approaches. 
# It can be observed that 2 days naive prediction approach fails to provide a good model as it yields a MAPE of 8.12% mean and 6.4% median which are the highest among all approaches. (Task a, 1st boxplot)
# 7 Days naive prediction approach provides really good results but there is a considereable number of outlying points above the upper whisker. (Task a, 2nd boxplot)
# Predicitons from the linear regression model on 48 & 168 hours lags (Task b, 3rd boxplot) and predictions from the 24 regression models on each hour (Task c, 4th boxplot) both provide adequate results with similar mean and median MAPE values.
# Hourly Lasso Models (Task d, 5th boxplot) provides the best predictions in terms of MAPE. How? As follows:
# Boxplot for Naive168 may look like it yields more accurate predictions than Hourly Lasso but if we look at the mean of the MAPE for both approaches; mean of MAPE for Naive168 is 2.89 while mean of Hourly Lasso is 2.57.
# In terms of simplicity and median value, Naive168 (Task a) is the best approach but in terms of accuracy, Hourly Lasso (Task d) is the best fit to be used based on test results.












