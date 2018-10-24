
#Prices of Certain Items in Selected Cities vs New York

cost <- read.table("Cost_of_Living_2013.txt", sep = "\t", header = TRUE)
names(cost)
# [1] "City"                         "Cost.of.Living.Index"         "Rent.Index"                  
# [4] "Groceries.Index"              "Restaurant.Price.Index"       "Local.Purchasing.Power.Index"

# Produce scatterplot
plot(cost$Rent.Index ~ cost$Cost.of.Living.Index, xlab = "Cost of Living Index", ylab = "Cost of Rent Index", data = cost)

plot(cost$Groceries.Index ~ cost$Cost.of.Living.Index, xlab = "Cost of Living Index", ylab = "Cost of Groceries Index", data = cost)

plot(cost$Restaurant.Price.Index ~ cost$Cost.of.Living.Index, xlab = "Cost of Living Index", ylab = "Restaurant Price Index", data = cost)

plot(cost$Local.Purchasing.Power.Index ~ cost$Cost.of.Living.Index, xlab = "Cost of Living Index", ylab = "Local Purchasing Power Index", data = cost)

# Correlation Coefficients
cor(cost$Rent.Index, cost$Cost.of.Living.Index)
    # 0.7722926
cor(cost$Cost.of.Living.Index, cost$Groceries.Index)
    # 0.9538616
cor(cost$Cost.of.Living.Index, cost$Restaurant.Price.Index)
    # 0.9493554
cor(cost$Cost.of.Living.Index, cost$Local.Purchasing.Power.Index)
    # 0.525902


# Linear Regression Model Check if Meet Conditions for Coorelation


rentReg <- lm(Rent.Index ~ Cost.of.Living.Index, data = cost)
plot(cost$Rent.Index ~ cost$Cost.of.Living.Index, xlab = "Cost of Living Index", ylab = "Cost of Rent Index", data = cost)
abline(rentReg)
summary(rentReg)


groceriesReg <- lm(Groceries.Index ~ Cost.of.Living.Index, data = cost)
plot(cost$Groceries.Index ~ cost$Cost.of.Living.Index, xlab = "Cost of Living Index", ylab = "Cost of Groceries Index", data = cost)
abline(groceriesReg)
summary(groceriesReg)


restaurantPriceReg <- lm(Restaurant.Price.Index ~ Cost.of.Living.Index, data = cost)
plot(cost$Restaurant.Price.Index ~ cost$Cost.of.Living.Index, xlab = "Cost of Living Index", ylab = "Restaurant Price Index", data = cost)
abline(restaurantPriceReg)
summary(restaurantPriceReg)


localPurchasingPowerReg <- lm(Local.Purchasing.Power.Index ~ Cost.of.Living.Index, data = cost)
plot(cost$Local.Purchasing.Power.Index ~ cost$Cost.of.Living.Index, xlab = "Cost of Living Index", ylab = "Local Purchasing Power Index", data = cost)
abline(localPurchasingPowerReg)


# Find Cost of Living as predicted by Groceries Index...
#... and residual for Beijing, China
range(cost$Cost.of.Living.Index)

new <- data.frame(Cost.of.Living.Index = c(83.57))
predict(groceriesReg,newdata = new)
groceriesReg$residuals[172] #Index of Beijing

    # Prediction cost of living in Beijing = 88.85556
#Cost of living and residual
