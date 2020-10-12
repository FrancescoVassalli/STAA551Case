neededPackages <- c("tidyverse"
                   , "magrittr"
                   , "rstudioapi"
                   , "rlang"
                   , "car"
                   #add more packages if needed here
)

for (i in neededPackages){
  if( !is.element(i, .packages(all.available = TRUE)) ) {
    install.packages(i)
  }
  library(i,character.only = TRUE)
}
rm(i, neededPackages)

wd <- getActiveDocumentContext()$path %>% dirname() %>% setwd()
toyota.df <- na.omit(read_csv('ToyotaCorollaData.csv'))

toyota.df$Fuel_Type <- --(toyota.df$Fuel_Type == 'Diesel')
toyota.df$Doors <- --(toyota.df$Doors >= 4)

toyota.df %<>% select(-c('Mfg_Month', 'Mfg_Year', 'Cylinders')) 
toyota.df$Fuel_Type %<>% as.factor()
toyota.df$Metallic %<>% as.factor()
toyota.df$Automatic %<>% as.factor()
toyota.df$Doors %<>% as.factor()
toyota.df$Gears %<>% as.factor()
toyota.df$BOVAG %<>% as.factor()
toyota.df$Guarantee %<>% as.factor()

toyota.df$CC[toyota.df$CC>10000]<-toyota.df$CC[toyota.df$CC>10000]/10 # replace typo

summary(toyota.df)
#Only 13 of fuel type CNG, or compressed natural gas. Typically these sorts of vehicles are busses
#only 28 of the 694 are automatic? That seems odd, but even if that's backwards it won't affect the model, only the interpretation of the results. 
#55 of the 694 have 4 doors (Including trunk door) which is, uh, something
#Everything has 
#Manufacture month and year is redundent due to age variable
#Cylinders variable is not needed, everything is 4 Cylinders
#only 19 of the data points have 6 gears, the rest have 5
#69 of the 694 are not BOVAG dealers
#Most have a CC of 6 different values, but 9 data points have other values 
#Of the factor levels that don't have many records, want to be careful about including them
plot(toyota.df$Price, toyota.df$Age) #fairly linear
plot(toyota.df$KM, toyota.df$Price) #transform here might make sense
plot(sqrt(toyota.df$KM), toyota.df$Price) #looks much better
toyota.df$KM.sqrt <- sqrt(toyota.df$KM)
plot(toyota.df$Weight, toyota.df$Price) #Doesnt look like much of a relationship at all here
plot(toyota.df$Period, toyota.df$Price) #Not entirely sure whats going on here, should this be a factor? 
plot(toyota.df$Age, toyota.df$KM) #unsurprisingly, somewhat of a relationship between KM and age, although the sqrt(KM) variable is more linear



boxcox.lm <- lm(Price ~ KM.sqrt + Age + HP + QuartTax + Weight, data = toyota.df)
boxCox(boxcox.lm)
abline(v = .5)

summary(powerTransform(cbind(KM, Age, HP, QuartTax) ~ 1, toyota.df))

toyota.df$Price.log <- log(toyota.df$Price)
toyota.df$HP.cube <- toyota.df$HP^(-.33)
toyota.df$QuartTax.cube <- toyota.df$QuartTax^(.33)

write_csv(toyota.df, "cleanedToyotadata.csv")
cor(toyota.df$QuartTax, toyota.df$Weight)
summary(boxcox.lm)
