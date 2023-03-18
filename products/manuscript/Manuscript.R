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
