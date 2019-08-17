# Importing the dataset

data<-read.csv("wine.csv")
summary(data)

# Model 

attach(data)        
reg<-lm(Price ~ Year+WinterRain+AGST+HarvestRain+Age+FrancePop,data=data) 

# Removing variables having p values higher than the significance level

reg<-lm(Price ~ WinterRain+AGST+HarvestRain+FrancePop,data=data)
summary(reg)

# Checking for muilticollinearity

library(car) # Package for loading vif()
vif(reg) # VIF should be less than 2 

# Checking MAPE if the deviation of error ateast supports good forcasting

data$Pred_price<-fitted(reg) # Storing the predicted prices from the model
data
mape<-(sum(abs(Price-Pred_price)/Price)/nrow(data)) # any MAPE less than or around 10-12% is perfect

# Error assumption tests

# Normality test for residuals (Price-Pred_price)

library(nortest) # Package for doing AD-Test 
data$residuals<-Price-Pred_price
ad.test(data$residuals) # > Alpha 0.05, i.e Residuals are Normally distributed

# Autocorrelation test for the residuals, i.e checking whether the residuals are corralated 

library(zoo) # Package for getting Durbin-Watson Test dwtest()
dwtest(reg) # > Alpha value, i.e residuals have 0 correlation

# Homoscedasticity check for the residuals

library(lmtest) # Package for checking Homoscedasticity
bptest(reg) # Hypothesis shows p-value > alpha, i.e residuals have even variance or fails to reject H0

# I have to master plotting
plot(data,pch=20,cex=1,col="blue") 

# This was my first Machine Learning model. Kudos to me!! 
