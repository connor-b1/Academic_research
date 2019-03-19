# Set working directory
setwd("~/School/Fall_2018/Economic_Data_Analysis/ECON-6010-Group-Project")
# Load packages
library(censusapi)
library(tidycensus)
library(jsonlite)
library(tidyverse)

# Load data
load('programs/prepped_data/msa_counties.rda')

# Stuff to set census bureau API key
# Sys.setenv(CENSUS_KEY='4406314b19dc50883b841ee1476ba327f14ef275')
# readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

#Function to pull data via census burea API
migration_data_api <- function(year, vars, county_code, state_code) {
  # Creating the URL to pull data from census bureau
  resURL <- paste0('https://api.census.gov/data/', year,'/acs/flows?get=', vars,'&for=county:', county_code, '&in=state:', state_code, '&key=4406314b19dc50883b841ee1476ba327f14ef275')

  # Pull in JSON data and storing in json_list
  json_list <- fromJSON(resURL)
  # Convert json_list to data frame
  df <- as_data_frame(json_list)
  # Change column name of df
  colnames(df) <- c('county1', 'county2', 'moved_in', 'moved_out', 'moved_net')
  # Add year variable as numeric
  df$year <- as.numeric(year)
  # Drop first row and columns 6-7
  df <- df[-c(1), -c(6:7)]
  # Remove rows with na values for moved columns
  df <- df %>% filter(!is.na(moved_in))
  df <- df %>% filter(!is.na(moved_out))
  df <- df %>% filter(!is.na(moved_net))
  # Convert moved columns to numeric
  df$moved_in <- as.numeric(df$moved_in)
  df$moved_out <- as.numeric(df$moved_out)
  df$moved_net <- as.numeric(df$moved_net)
  # Add state and county code to df for joining
  df$state_code <- as.character(state_code)
  df$county_code <- as.character(county_code)
  # Return df from the function
  return(df)
}

# There are probably better ways to do this, but I don't feel like messing around to figure it out
# We are going to use a for loop to loop through every combination of country code/state code, and year
# and call the census api function. At the end of each loop we save the results to a list
# Once the loop is finished, we will rbind all the list together into one data frame
# The loop takes abount 15 miniutes to run

# Creating date frame of each county, state, and year combo
codes <- data_frame(msa_counties$fipscountycode, msa_counties$fipsstatecode, msa_counties$msa)
year <- c('2009','2010','2011','2012','2013','2014','2015')
var <- c('FULL2_NAME,FULL1_NAME,MOVEDIN,MOVEDOUT,MOVEDNET')
# Each row in a unique api call we need
api_calls <- distinct(crossing(codes, year, var))
colnames(api_calls) <- c('county_code', 'state_code', 'msa', 'year', 'var')

# Clean up
rm(codes,year,var)

##########################################
# for loop to loop over the api calls
##########################################

# Initilize empty list
datalist <- list()

for(i in 1:nrow(api_calls)) {
  # Assign each cell in a row to a variable
  year_var <- as.character(api_calls[i, 'year'])
  var_var <- as.character(api_calls[i, 'var'])
  county_var <- as.character(api_calls[i, 'county_code'])
  state_var <- as.character(api_calls[i, 'state_code'])
  msa_var <- as.character(api_calls[i, 'msa'])
  
  # Call function with created variables
  data <- migration_data_api(year_var, var_var, county_var, state_var)
  # Add interation for tracking
  data$i <- i
  data$state_code <- state_var
  data$county_code <- county_var
  data$county2_msa <- msa_var
  
  # Save result to list
  datalist[[i]] <- data
}

#Extract data from datalist into dataframe
migration_data <- do.call(rbind, datalist)

#Save data frame for future loading to save time
save(migration_data, file = "programs/prepped_data/migration_data.rda")

