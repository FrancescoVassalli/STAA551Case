##############################################################################
## Using cleaned data but not manipulating number of cats in doors ## 
attach(toyota.df)

plot(Price, cex = 0.75)
plot(log(Price)) 

# Correlation matrix for quant vars 
cor(toyota.df[,c("Price", "Age", "KM", "HP", "CC", "QuartTax", "Weight")])

fit1 <- lm(log(Price) ~ Age)
summary(fit1)
plot(log(Price) ~ Age)

fit2 <- lm(log(Price) ~ Age + KM)
summary(fit2)
vif(fit2)

fit3 <- lm(log(Price) ~ Age + KM + Weight)
summary(fit3)
vif(fit3)

fit4 <- lm(log(Price) ~ Age + KM + Weight + HP)
summary(fit4)
vif(fit4)
plot(fit4)

fit5 <- lm(log(Price) ~ Age + KM + Weight + HP + QuartTax)
summary(fit5) #R2_adj = 0.849
plot(fit5)

resid5 <- resid(fit5) 
# look at resids v predictors to think about transformations
par(mfrow=c(3,2))
par(mar=c(4, 4, 2, 0.5))
plot(Age,resid5,xlab="Age",ylab="residual")
abline(a=0,b=0)
plot(KM,resid5,xlab="KM",ylab="residual")
abline(a=0,b=0)
plot(Weight,resid5,xlab="Weight",ylab="residual") # some concern on the right
abline(a=0,b=0)
plot(HP,resid5,xlab="HP",ylab="residual") # some concern on the right
abline(a=0,b=0)
plot(QuartTax,resid5,xlab="QuartTax",ylab="residual")
abline(a=0,b=0)

summary(powerTransform(cbind(Price, Age, KM, Weight, HP, QuartTax) ~ 1))

# try sqrt(Price) as response
fit5.5 <- lm(sqrt(Price) ~ Age + KM + Weight + HP + QuartTax)
summary(fit5.5)
plot(fit5.5) #this has issues

# Transform predictors
toyota.df$HP.trans <- HP^(-1/3)
toyota.df$Weight.trans <- Weight^-4
attach(toyota.df)


fit6 <- lm(log(Price) ~ Age + KM + Weight.trans + HP.trans + QuartTax + Fuel_Type)  
summary(fit6)

fit7 <- lm(log(Price) ~ Age + KM + Weight.trans + HP.trans + QuartTax + Fuel_Type + Guarantee)  
summary(fit7) 
plot(fit7)
vif(fit7)

fit8 <- lm(log(Price) ~ Age + KM + Weight.trans + HP.trans + QuartTax + Guarantee)  
summary(fit8) 
vif(fit8)

fit9 <- lm(log(Price) ~ Age + KM + Weight.trans + HP.trans + Fuel_Type + Guarantee)  
summary(fit9)  #not as useful as 8

resid8 <- resid(fit8)
par(mfrow=c(3,2))
par(mar=c(4, 4, 2, 0.5))
plot(Age,resid8,xlab="Age",ylab="residual")
abline(a=0,b=0)
plot(KM,resid8,xlab="KM",ylab="residual")
abline(a=0,b=0)
plot(Weight.trans,resid8,xlab="Weight",ylab="residual") 
abline(a=0,b=0)
plot(HP.trans,resid8,xlab="HP",ylab="residual") #i think this just is this way
abline(a=0,b=0)
plot(QuartTax,resid8,xlab="QuartTax",ylab="residual")
abline(a=0,b=0)

fit10 <- lm(log(Price) ~ Age + KM + Weight.trans + HP.trans + QuartTax*Fuel_Type2 + Guarantee)  
summary(fit10)
vif(fit10)
par(mfrow=c(2,2))
plot(fit10) #qq tails are an issue
