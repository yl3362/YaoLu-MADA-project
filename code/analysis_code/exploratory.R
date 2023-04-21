
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


## ----table--------
summary_df = skimr::skim(mydata[,c('14 day case rate','RTCUMVAXADMIN',
                                   'PCTCUMPVAX',"PCTCUMPCVAX","PCTBOOSTER")])
print(summary_df)
summary_df1 <- summary_df %>%
              select(c(2,5,6,12))
# save to file
summarytable_file = here("results","rds", "1_summarytable.rds")
saveRDS(summary_df1, file = summarytable_file)

## ----figurey--------

p1 <- mydata %>% ggplot(aes(x=`14 day case rate`)) + 
                geom_histogram() +ylab('Number of county')

                  
plot(p1)
figure_file = here("results","plot","2_14_day_case_rate.png")
ggsave(filename = figure_file, plot=p1) 

## ----figurey1--------
mydata$county_name[which(mydata$`14 day case rate`>300)]
mydata$`14 day case rate`[which(mydata$county_name=='Quitman')]
mydata$`14 day case rate`[which(mydata$county_name=='Stewart')]


## ----figurex5--------
px5 <- mydata %>% ggplot(aes(x=PCTBOOSTER)) + geom_histogram() +
  ylab('Number of county')
plot(px5)
figure_file = here("results","plot","1_PCTBOOSTER_distribution.png")
ggsave(filename = figure_file, plot=px5) 

## ----population1--------
quantile(mydata$population,c(0.33,0.66))

## ----function1--------
pf1 <- mydata %>% ggplot(aes(x=PCTBOOSTER, y=`14 day case rate`)) + 
  geom_point() + geom_smooth(method='lm')+
  xlim(5, 40)+ylim(-100,1200)+labs(x='Percentage of booster',y='14 day case rate',col='Population category')
plot(pf1)
figure_file = here("results","plot","case_BOOSTER.png")
ggsave(filename = figure_file, plot=pf1) 

## ----function2--------
pf2 <- mydata %>% ggplot(aes(x=PCTBOOSTER, y=`14 day case rate`, color = population1)) + 
  geom_point() + geom_smooth(method='lm')+ theme(legend.position = c(0.8,0.8))+xlim(5, 40)+ylim(-100,1200)+
  labs(x='Percentage of booster',y='14 day case rate',col='Population')
plot(pf2)
figure_file = here("results","plot","case_BOOSTER_stratified.png")
ggsave(filename = figure_file, plot=pf2) 



## ----function3 --------

require(gridExtra)
pf3 <- grid.arrange(pf1, pf2, ncol=2)
plot(pf3)
figure_file = here("results","plot","3_case_BOOSTER_stratified.png")
ggsave(filename = figure_file, plot=pf3) 
