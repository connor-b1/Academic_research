# Set working directory
setwd("~/School/Fall_2018/Economic_Data_Analysis/ECON-6010-Group-Project")

# Load packages
library(jsonlite)
library(tidyverse)

# Load data
crosswalk <- read_csv('inputs/county_to_msa_crosswalk.csv')

# Set of MSAs 
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

# Get distinct msas codes. Store in vector
msa_codes <- crosswalk %>% filter(cbsatitle %in% msas) %>% distinct(cbsacode)
msa_codes <- as.vector(msa_codes$cbsacode)


#########
## GDP ##
#########

# Initilize empty list
datalist <- list()

# For loop to loop over msa_codes and query GDP data from FRED
for(msa_code in msa_codes) {
  # Creating the URL to pull data from census bureau
  resURL <- paste0('https://api.stlouisfed.org/fred/series/observations?series_id=RGMP',msa_code,'&api_key=a2541dacf2fe0876e9ad7748fc97a381&file_type=json')
  
  # Pull in JSON data and storing in json_list
  json_list <- fromJSON(resURL)
  # Convert json_list to data frame
  df <- as_data_frame(json_list$observations)
  # Remove first two columns
  df <- df[,-c(1:2)]
  # Change column name of df
  colnames(df) <- c('date', 'gdp')
  # Add MSA code and name to df
  df$msa_code <- msa_code
  df$msa_title <- crosswalk %>% filter(cbsacode == msa_code) %>% distinct(cbsatitle) %>% pull(cbsatitle)
  
  # Convert gdp column to numeric
  df$gdp <- as.numeric(df$gdp)
  
  # Save df to list
  datalist[[msa_code]] <- df
}

# Extract data from datalist into dataframe
gdp_data <- do.call(rbind, datalist)

# Save data frame
save(gdp_data, file = "programs/prepped_data/gdp_data.rda")
