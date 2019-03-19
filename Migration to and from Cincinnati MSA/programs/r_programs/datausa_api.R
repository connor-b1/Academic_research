#Set working directory
setwd("~/School/Fall_2018/Economic_Data_Analysis/ECON-6010-Group-Project/programs")
#Load packages
library(jsonlite)
library(tidyverse)

####################################################
##American community survey data at the MSA level###
####################################################

#api function
acs_data_api <- function(year) {
  api <- paste0('http://api.datausa.io/api/csv/?show=geo&sumlevel=msa&year=',year,'&required=pop,pop_rank,age,us_citizens,non_us_citizens,non_eng_speakers_pct,income,income_rank,mean_commute_minutes,median_property_value,owner_occupied_housing_units')
  
  #Pull in CSV data and storing in df
  return(read_csv(api))}

#for loop to loop over the api function for each year of data
##Initilize empty list
datalist <- list()

for(i in seq(2013,2016,1)){
  #Call function
  data <- acs_data_api(as.character(i))
  data$year <- as.character(i)

  #Save result to list
  datalist[[i - 2012]] <- data
}
#Extract data from datalist into dataframe
acs_data <- do.call(rbind, datalist)
#Clean up
rm(data,datalist,i,x)

#Save data
save(acs_data, file = 'acs_data.rda')

##################
##Property taxes##
##################
api <- paste0('https://api.datausa.io/api/csv/?sort=desc&force=acs.yg_property_tax&show=geo&sumlevel=msa&year=all')
#Pull in CSV data and storing in df
df_prop_tax <- read_csv(api)

#Save data
save(df_prop_tax, file = 'property_tax_data.rda')

##################################
##Crime data at the county level##
##################################
api <- paste0('https://api.datausa.io/api/csv/?sort=desc&show=geo&sumlevel=county&year=all&required=homicide_rate,violent_crime') 
#Pull in CSV data and storing in df
df_crime <- read_csv(api)

#Save data
save(df_crime, file = 'crime_data.rda')

############################################
##Number of graduates from degree programs##
############################################
#Set of MSAs 
msas <- c('Cincinnati, OH-KY-IN',
          'Denver-Aurora-Lakewood, CO',
          'Austin-Round Rock, TX',
          'Columbus, OH',
          'Kansas City, MO-KS',
          'Cleveland-Elyria, OH',
          'Pittsburgh, PA',
          'Indianapolis-Carmel-Anderson, IN',
          'St. Louis, MO-IL',
          'Charlotte-Concord-Gastonia, NC-SC',
          'Louisville/Jefferson County, KY-IN',
          'Nashville-Davidson--Murfreesboro--Franklin, TN',
          'Memphis, TN-MS-AR')

msa_codes <- crosswalk %>% filter(cbsatitle %in% msas) %>% distinct(cbsacode)

#api function
edu_data_api <- function(msa_code) {
  api <- paste0('https://api.datausa.io/api/csv/?sort=desc&sumlevel=4&show=cip&year=all&geo=31000US',msa_code) 
  print(api)
  
  #Pull in CSV data and storing in df
  return(read_csv(api))}

#for loop to loop over api funciton for each msa
##Initilize empty list
datalist <- list()

for(i in 1:13){
  #Call function
  data <- edu_data_api(as.character(msa_codes[i,1]))
  
  #Save result to list
  datalist[[i]] <- data
}
#Extract data from datalist into dataframe
edu_data <- do.call(rbind, datalist)
#Clean up
rm(data,datalist,i,x)
