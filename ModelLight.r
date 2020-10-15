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

toyota.df$Gears %<>% as.factor()
toyota.df$BOVAG %<>% as.factor()
toyota.df$Guarantee %<>% as.factor()
head(toyota.df)

fitW = lm(log(Price)~Age+CC+Guarantee+BOVAG+Guarantee+KM.sqrt+HP.cube+QuartTax.cube+Weight,data=toyota.df)
summary(fitW)

plot(fitW,1)

plot(fitW,2)

toyota.df[601,1-ncol(toyota.df)]

boxplot(toyota.df$KM) 


