
# Read the data file

data<-read.csv("car data.csv")
data<-data[,2:8] # I don't want the rest of the features
summary(data) # I am checking the nature and quality of the data

# Outlier analysis and reduction

quantile(data$Kms_Driven,prob=c(0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.91,0.93,0.95,0.97,0.99,1))
boxplot(data$Kms_Driven[data$Kms_Driven>15399 & data$Kms_Driven<60260.24])

# Feeding the removed features into the dataset 'data'

data<-data[data$Kms_Driven>15399 & data$Kms_Driven<60260.24,]
summary(data) # Checking the condition of the dataset

# Training the regression model

mdl<-lm(Selling_Price~Year+Present_Price,data=data) # Features with coefficients 0 i.e, having p>0.05 are reduced
summary(mdl) # Checking the condition of the model trained

# Looking up for multi-collinearity among the variables using VIF
 
library(car)
vif(mdl)
data$Pred_Selling<-fitted(mdl) # Predicted values are fitted into the dataframe
data$residual<-data$Selling_Price-data$Pred_Selling # Error values are collected

# Error assumption techniques

mape<-(sum((abs(data$Selling_Price-data$Pred_Selling))/data$Selling_Price))/nrow(data) # MAPE looks good enough
library(nortest) 
library(lmtest)
library(zoo)
ad.test(data$residual) # Normality test for the residual values 
dwtest(mdl) # Autocorrelation Hypothesis test
bptest(mdl) # Homoscedasticity Hypothesis test 

# Plotting 
plot(data$Pred_Selling~data$Selling_Price,pch=20,cex=2,col="blue")
