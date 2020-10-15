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

toyota.df$Doors4 <- --(toyota.df$Doors == 4)
toyota.df$Doors5 <- --(toyota.df$Doors == 5)

toyota.df$Metallic %<>% as.factor()
toyota.df$Automatic %<>% as.factor()
toyota.df$Fuel_Type %<>% as.factor()
toyota.df$Doors4%<>% as.factor()
toyota.df$Doors5%<>% as.factor()
toyota.df$Gears %<>% as.factor()
toyota.df$BOVAG %<>% as.factor()
toyota.df$Guarantee %<>% as.factor()
head(toyota.df)

df.factors <- toyota.df[,-which(sapply(toyota.df, class) == "factor")] %>% 
  select(-c("KM","Price","QuartTax","HP","Doors"))

head(df.factors)

fit.nofactor = lm(Price.log ~ ., data = df.factors)
summary(fit.nofactor)

mod.df <- toyota.df %>% 
  select(-c("Doors", "HP", "Gears", "QuartTax", "KM", "Period", "Price"))

fit1 <- lm(Price.log ~ ., data = mod.df)
summary(fit1)

mod2.df <- mod.df %>% 
  select(-c("Doors4", "Doors5", "Metallic", "Automatic", "Fuel_Type"))

fit2 <- lm(Price.log ~ ., data=mod2.df)
summary(fit2)

fit3 <- lm(Price.log ~ Age + CC + Guarantee + BOVAG + KM.sqrt + HP.cube + 
             QuartTax.cube + I(Weight^-2) + I(Weight^-3) + I(Weight^-4)
           , data=mod2.df)
summary(fit3)


fit_final <- 
  lm(Price.log ~ 
       Age + CC + Guarantee + BOVAG + KM.sqrt + HP.cube + QuartTax.cube + Weight
     , data=toyota.df)

summary(fit_final)
vif(fit_final)
plot(fit_final, 1:4)

