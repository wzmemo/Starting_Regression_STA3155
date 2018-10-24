pizza <- read.table('Frozen_Pizza.txt',sep = '\t', header = TRUE)
# response var(y) = sales, predictor var(x) = price;
# Fit reg model for four cities, baltimore, chicago, dallas, denver
par(mfrow=c(2,2))

1.
plot(pizza$Baltimore.Volume~pizza$Baltimore.Price,xlab = 'Price', ylab = 'Sales')
imod_baltimore <- lm(Baltimore.Price~Baltimore.Volume, data = pizza)
summary(imod_baltimore)
abline(imod_baltimore) # R-square = 0.4992
title('Baltimore scatterplot')

plot(pizza$Chicago.Volume~pizza$Chicago.Price,xlab = 'Price', ylab = 'Sales')
imod_chicago <- lm(Chicago.Price~Chicago.Volume, data = pizza)
summary(imod_chicago)
abline(imod_chicago) #R-square = 0.7565
title('Chicago scatterplot')

plot(pizza$Dallas.Volume~pizza$Dallas.Price,xlab = 'Price', ylab = 'Sales')
imod_dallas <- lm(Dallas.Price~Dallas.Volume, data = pizza)
summary(imod_dallas)
abline(imod_dallas) #R-square = 0.2823
title('Dallas scatterplot')

plot(pizza$Denver.Volume~pizza$Denver.Price,xlab = 'Price', ylab = 'Sales')
imod_denver <- lm(Denver.Price~, data = pizza)
summary(imod_denver)
abline(imod_denver) #R-Square = 0.5924
title('Denver scatterplot')


2.
# Baltimore 
#Time series
par(mfrow=c(2,2))
plot(pizza$Week,imod_baltimore$residuals,xlab='Weeks',ylab='Residuals')
title('Baltimore Time Series'); abline(0,0)
# residual plot of fitted values
plot(imod_baltimore$fitted.values,imod_baltimore$residuals,xlab = 'Fitted Values', ylab = 'Residuals')
title('Baltimore Residual Plot Fitted Values'); abline(0,0)
# QQ plot
qqnorm(imod_baltimore$residuals);qqline(imod_baltimore$residuals)

# Chicago
# Time Series
par(mfrow=c(2,2))
plot(pizza$Week,imod_chicago$residuals,xlab='Weeks',ylab='Residuals')
title('Chicago Time Series'); abline(0,0)
# Residual Plot of Fitted Values
plot(imod_chicago$fitted.values,imod_chicago$residuals,xlab = 'Fitted Values', ylab = 'Residuals')
title('Chicago Residual Plot Fitted Values'); abline(0,0)
# QQ plot
qqnorm(imod_chicago$residuals);qqline(imod_chicago$residuals)

#Dallas
#Time Series
par(mfrow=c(2,2))
plot(pizza$Week,imod_dallas$residuals,xlab='Weeks',ylab='Residuals')
title('Dallas Time Series'); abline(0,0)
#Residual plot
plot(imod_dallas$fitted.values,imod_dallas$residuals,xlab = 'Fitted Values', ylab = 'Residuals')
title('Dallas Residual Plot Fitted Values'); abline(0,0)
#QQ plot
qqnorm(imod_dallas$residuals);qqline(imod_dallas$residuals)

#Denver
# Time Series
par(mfrow=c(2,2))
plot(pizza$Week,imod_denver$residuals,xlab='Weeks',ylab='Residuals')
title('Denver Time Series'); abline(0,0)
# Residual Plot
plot(imod_denver$fitted.values,imod_denver$residuals,xlab = 'Fitted Values', ylab = 'Residuals')
title('Denver Residual Plot Fitted Values'); abline(0,0)
#QQ plot
qqnorm(imod_denver$residuals);qqline(imod_denver$residuals)
3.
# Focus on denver and show and interpt an 90% confidence interval
plot(pizza$Dallas.Volume~pizza$Dallas.Price,xlab = 'Price', ylab = 'Sales')
imod_dallas <- lm(Dallas.Volume~Dallas.Price, data = pizza)
summary(imod_dallas)
abline(imod_dallas) #R-square = 0.2823
title('Dallas scatterplot')
confint(imod_dallas,level=0.90) # CI = (-40655.79, -26398.58) a decrease! 
# By the confidence interval alone, we cant say signifigance

4.
# H0: beta0 = 0; HA: beta0 != 0;
summary(imod_dallas) # reject H0 bc p-value (almost 0) is less than alpha (.1)

5. 
range(pizza$Dallas.Price) #range is between 2.21 and 3.05

new1 <- data.frame(Dallas.Price = c(2.50))
new2 <- data.frame(Dallas.Price = c(3.00))

predict(imod_dallas,newdata = new1, interval = 'confidence', level = 0.95)
# predicted mean = 55729.47, CI = (54063.14,57395.79)
predict(imod_dallas,newdata = new2, interval = 'confidence', level = 0.95)
# predicted mean = 33965.87, CI = (35464.31, 42467.44)

# No we cant estimate the mean if price = 3.50 becuase it is out of the sample range

6.

new3 <- data.frame(Dallas.Price = c(2.99))

predict(imod_dallas,newdata = new3, interval = 'prediction', level = 0.95)
# predicted mean = 39301.15, Prediction interval = (22425.65, 56176.65)
# No resulting prediction is not useful becuase it is too wide of a prediction



