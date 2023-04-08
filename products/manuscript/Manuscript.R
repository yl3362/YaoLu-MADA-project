## ----packages--------
#load packages
library(here)
library(dplyr)
library(ggplot2)


## ----loaddata--------
#Path to data. Note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")
#load data
mydata <- readRDS(data_location)

## ----lm--------
# linear regression
lm1 <- lm(`14 day case rate`~PCTBOOSTER+population1,data=mydata)

summary(lm1)

lm2 <- lm(`14 day case rate`~PCTCUMPCVAX+population1,data=mydata)

summary(lm2)

## ----lm1--------
#prescreen

#pre-screen
coln <- colnames(mydata[c(2:7,10:12,14:22)])
#get  
num <- 1
pvalue <- c()
slope <- c()
for(j in coln){
  lm11 <- lm(mydata$`14 day case rate`~mydata[[j]])
  pvalue[num] <- summary(lm11)$coefficients[2,4] 
  slope[num] <- summary(lm11)$coefficients[2,1]
  num <- num+1
}

prescreen <- data.frame(cbind(coln,pvalue,slope))

#population1 have 3 catogaries, we do this manually

lmp <- lm(mydata$`14 day case rate`~mydata$population1)
pa <- summary(lmp)$coefficients[3,4]
sa <- summary(lmp)$coefficients[3,1]
popu <- c('popusmall',pa,sa)

prescreen1 <- as.data.frame(rbind(prescreen,popu))


 prescreen1[,2:3] <-  as.numeric(unlist(prescreen1[,2:3]))
prescreen1[,2:3] <- round(prescreen1[,2:3], digits = 3)

prescreen1
## ---- anova --------
#anove test for categorical population

lmp1 <- anova(lm(mydata$`14 day case rate`~mydata$population1))
lmp1

## ---- lm2 --------
# linear regression
lmt1 <- lm(mydata$`14 day case rate`~mydata$`case rate`+mydata$population1)
summary(lmt1)
