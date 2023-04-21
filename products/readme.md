Here is how people reproduce all the steps I did.

1.Download the vaccination data from 
https://experience.arcgis.com/experience/3d8eea39f5c1443db1743a4cb8948a9c 
2. Download the symptom and death data from https://ga-covid19.ondemand.sas.com/docs/ga_covid_data.zip
3.Use Georgia_DPH_PUBLIC_Vaccination_Public_Data_in_Excel.xlsx as our major vaccination source.
4.Use ga_covid_data/county_cases.csv as our major cases source.
5. Run code/processing_code/processingcode1.R to clean the data.
6. Run code/analysis_code/exploratory_analysis.R to have initial explore
7. Run code/model/basicanalysis.R
8. Run code/model/machinelearning.R
9. Run products/Manuscriptnew0421.qmd to render docx file.