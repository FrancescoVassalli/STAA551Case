neededPackages <- c("tidyverse"
                   , "magrittr"
                   , "rstudioapi"
                   , "rlang"
                   , "ggplot2"
                    ,"reshape2"
                    ,"car"
                    ,"comprehenr"
                   #add more packages if needed here
)

for (i in neededPackages){
  if( !is.element(i, .packages(all.available = TRUE)) ) {
    install.packages(i)
  }
  library(i,character.only = TRUE)
}
rm(i, neededPackages)

wd <- getwd()
toyota.df <- read_csv("cleanedToyotadata.csv")
head(toyota.df)

toyota.df$Doors_clean<- to_list(for (x in toyota.df$Doors) if (x>=4) 1 else 0)
toyota.df$Fuel_clean<- to_list(for (x in toyota.df$Fuel_Type) if (x=='Diesel') 1 else 0)
toyota.df <- as.data.frame(lapply(toyota.df, unlist))


toyota.df$Price_log<- log(toyota.df$Price)
toyota.df$KM_log<- log(toyota.df$KM)    
toyota.df$Age_log<- log(toyota.df$Age)    

head(toyota.df)

mod.df = toyota.df[ , -which(names(toyota.df) %in% c("Age_log","KM.sqrt","KM","Period","Price","Fuel_Type","Doors"))]

fit = lm(mod.df$Price_log~.,data=mod.df)
summary(fit)

backAIC = step(fit,direction="backward",data=mod.fit)

summary(backAIC)

vif(backAIC)

mod.fit.int1 = lm(Price_log~Age + HP + QuartTax+(QuartTax*Fuel_clean) + Weight + Guarantee + 
    BOVAG +Fuel_clean + KM_log + (KM_log*Age),data=mod.df)

summary(mod.fit.int1)


