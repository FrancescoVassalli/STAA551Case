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
toyota.df$Metallic %<>% as.factor()
toyota.df$Automatic %<>% as.factor()
toyota.df$Fuel_Type%<>% as.factor()
toyota.df$Doors4%<>% as.factor()
toyota.df$Doors5%<>% as.factor()
toyota.df$Gears %<>% as.factor()
toyota.df$BOVAG %<>% as.factor()
toyota.df$Guarantee %<>% as.factor()
head(toyota.df)

df.factors=toyota.df[,-which(sapply(toyota.df, class) == "factor")]
df.factors = df.factors[ , -which(names(df.factors) %in% c("KM","Price","QuartTax","HP","Doors"))]

head(df.factors)

fit.nofactor = lm(df.factors$Price.sqrt~.,data=df.factors)
summary(fit.nofactor)

mod.df = toyota.df[ , -which(names(toyota.df) %in% c("Doors","HP","Gears","QuartTax","KM","Period","Price"))]

head(mod.df)

fit = lm(mod.df$Price.sqrt~.,data=mod.df)
summary(fit)

mod2.df = mod.df[ , -which(names(mod.df) %in% c("Doors4","Doors5","Metallic","Automatic","Fuel_Type"))]

fit2 = lm(mod2.df$Price.sqrt~.,data=mod2.df)
summary(fit2)

fitW = lm(mod2.df$Price.sqrt~Age+CC+Guarantee+BOVAG+KM.sqrt+HP.cube+QuartTax.cube+I(Weight^-2)+I(Weight^-3)+I(Weight^-4),data=mod2.df)
summary(fitW)

confint(fit,"Age",.95)

confint(fit2,"Age",.95)

confint(fitW,"Age",.95)

vif(fit2)

plot(fitW,1)

plot(fitW,2)

plot(fitW,3)

plot(fitW,4)

plot(fitW,5)
