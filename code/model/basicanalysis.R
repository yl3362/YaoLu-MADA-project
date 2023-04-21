## ----packages--------
#load packages
library(here)
library(dplyr)
library(ggplot2)
library(gridExtra)


## ----loaddata--------
#Path to data. Note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")
#load data
mydata <- readRDS(data_location)

## ----lm--------
# linear regression
lm1 <- lm(`14 day case rate`~PCTBOOSTER+population1,data=mydata)
sum1 <- round(summary(lm1)$coefficients,digit=3)
save_data_location <- here::here("results","rds","2_bo_po1.rds")
saveRDS(sum1, file = save_data_location)

lm3 <- lm(`14 day case rate`~PCTBOOSTER+population,data=mydata)
sum3 <- round(summary(lm3)$coefficients,digit=3)
save_data_location <- here::here("results","rds","3_bo_po.rds")
saveRDS(sum3, file = save_data_location)

lm2 <- lm(`14 day case rate`~PCTCUMPCVAX+population1,data=mydata)
sum2 <- round(summary(lm2)$coefficients,digit=3)
save_data_location <- here::here("results","rds","4_fu_po.rds")
saveRDS(sum3, file = save_data_location)

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

prescreen1 #population 1 means the difference between medium and high
#population small means the difference between small and high

save_data_location <- here::here("results","rds","5_prescreen.rds")
saveRDS(prescreen1, file = save_data_location)

## ---- anova --------
#anove test for categorical population

lmp1 <- anova(lm(mydata$`14 day case rate`~mydata$population1))
lmp1
save_data_location <- here::here("results","rds","6_anova_po.rds")
saveRDS(lmp1, file = save_data_location)



## ---- lm2 --------
# linear regression
lmt1 <- lm(mydata$`14 day case rate`~mydata$`case rate`+mydata$population1)
sumt1 <- round(summary(lmt1)$coefficient,digit=3)
row.names(sumt1)[2] <- 'case_rate'
row.names(sumt1)[3] <- 'population_medium'
row.names(sumt1)[4] <- 'poplation_high'

save_data_location <- here::here("results","rds","7_cr_po.rds")
saveRDS(sumt1, file = save_data_location)
