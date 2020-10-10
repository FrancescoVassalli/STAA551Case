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
toyota.df$Fuel_Type %<>% as.factor()
toyota.df$Metallic %<>% as.factor()
toyota.df$Automatic %<>% as.factor()
toyota.df$Doors %<>% as.factor()
toyota.df$Gears %<>% as.factor()
toyota.df$BOVAG %<>% as.factor()
toyota.df$Guarantee %<>% as.factor()
head(toyota.df)

mod.df = toyota.df[ , -which(names(toyota.df) %in% c("HP","QuartTax","KM","Period","Price","Fuel_Type","Doors"))]

head(mod.df)

df.factors=toyota.df[,-which(sapply(toyota.df, class) == "factor")]
df.factors = df.factors[ , -which(names(df.factors) %in% c("KM","Price","QuartTax"))]

head(df.factors)

fit.nofactor = lm(df.factors$Price.log~.,data=df.factors)
summary(fit.nofactor)

fit = lm(mod.df$Price.log~.,data=mod.df)
summary(fit)

vif(fit)

mod.df.small =mod.df %>% select(-c('CC', 'Automatic', 'Metallic','Doors_clean','Gears'))

fit.small = lm(mod.df.small$Price.log~.,data=mod.df.small)
summary(fit.small)

fit.int1 = lm(mod.df.small$Price.log~Age+Weight+Guarantee+BOVAG+KM.sqrt+HP.cube+QuartTax.cube+Fuel_clean+(QuartTax.cube*Fuel_clean),data=mod.df.small)
summary(fit.int1)

fit.int2 = lm(mod.df.small$Price.log~Age+Weight+Guarantee+BOVAG+KM.sqrt+HP.cube+QuartTax.cube+Fuel_clean+(QuartTax.cube*Fuel_clean)+(QuartTax.cube*Weight),data=mod.df.small)
summary(fit.int2)

plot(fit.int2,1)

plot(fit.int2,2)

plot(fit.int2,3)

plot(fit.int2,4)

toyota.df[222,]

boxplot(toyota.df$Weight)

mod.df.small[222,]

boxplot(mod.df.small$Age)


