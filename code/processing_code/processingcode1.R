

## ---- packages --------
#loading packages
library(readxl)
library(readr)
library(DataExplorer)
library(dplyr)
library(ggplot2)
library(here)
library(skimr)
library(stringr)

## ----loaddata --------
#load data here
data_location <- here::here("data","raw_data","Georgia_DPH_PUBLIC_Vaccination_Public_Data_in_Excel.xlsx")
vacin <- readxl::read_excel(data_location,sheet='COUNTY_SUMMARY')
data_location1 <- here::here("data","raw_data",'ga_covid_data',"county_cases.csv")
cases <- read_csv(data_location1)

## ----exploredata --------
#take a look at the data
head(cases)
head(vacin)

## ----cleandata1 --------
#pick the string after US- 
cases$newid <- stringr::str_sub(cases$county_id,4)
#sort by id
cases <- cases[order(cases$newid),]
vacin <- vacin[order(vacin$COUNTY_ID),]

## ----cleandata2 --------
vacin1 <- vacin[-c(1,161),]
cases1 <- cases[-c(1,2),]

## ----cleandata3--------

vacin1$newid <- vacin1$COUNTY_ID
all(cases1$newid %in% vacin1$newid)

## ----cleandata4--------

df1 <- full_join(cases1,vacin1,
          by='newid')

## ----cleandata5--------

df2 <- subset(df1,select = -c(county_id,`State FIPS code`,`County FIPS code`,
                              COUNTY_ID,COUNTY_NAME,DISTRICT_ID,`DISTRICT_NAME`))

## ----codebook--------

myCols <- as.character(read_excel(data_location, sheet ="DATA_DESCRIPTION", n_max = 1, skip=2,col_names = FALSE))
codebook <- read_excel(data_location, sheet ="DATA_DESCRIPTION", skip = 3, col_names = myCols)
print(codebook[c(23,25,27,29),])

## ----cleandata6--------
df2$PCTBOOSTER <- as.numeric(df2$PCTBOOSTER)
df2$PCTCUMPCVAX <- as.numeric(df2$PCTCUMPCVAX)
df2$PCTCUMPVAX <- as.numeric(df2$PCTCUMPVAX)

## ----cleandata7--------
df2$population1[df2$population<=15359.34] <- 'small'
df2$population1[df2$population>15359.34&df2$population<=34056.8] <- 'medium'
df2$population1[df2$population>34056.8] <- 'high'

## ----savedata--------
# makes it easier to add steps above
processeddata <- df2
# location to save file
save_data_location <- here::here("data","processed_data","processeddata.rds")
saveRDS(processeddata, file = save_data_location)

              