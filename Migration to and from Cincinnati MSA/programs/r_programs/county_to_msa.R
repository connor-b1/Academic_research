#Set working directory
setwd("~/School/Fall_2018/Economic_Data_Analysis/ECON-6010-Group-Project")

#Packages
require(tidyverse)
require(lazyeval)

#Read in MSA to Country crosswalk
crosswalk <- read_csv('inputs/county_to_msa_crosswalk.csv')

# Set of comparison MSAs 
comparison_msas_vec <- c('Cincinnati, OH-KY-IN',
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

# Save comparison MSA vector data frame
save(comparison_msas_vec, file ='programs/prepped_data/comparison_msas_vec.rda')

# Also make a copy as a data frame
comparison_msas_df <- as.tibble(comparison_msas_vec)
names(comparison_msas_df)[names(comparison_msas_df) == "value"] <- "cbsatitle"

# All MSAs
all_msas <- crosswalk %>% filter(!is.na(cbsatitle)) %>% distinct(cbsatitle)

# Modeling MSAs
# Force comparison MSAs to be in modeling MSAs
# Then take a random sample of 100 other MSAs
set.seed(34) # setting seed so we can reproduce results
modeling_msas <- anti_join(all_msas, comparison_msas_df) %>% sample_n(250, replace = FALSE)

# rbind comparison MSAs to modeling df
modeling_msas <- rbind(modeling_msas, comparison_msas_df)

# Save as a vector to be used in for loop 
modeling_msas_vec <- pull(modeling_msas, cbsatitle)

# Save to prepped_data
save(modeling_msas_vec, file ='programs/prepped_data/modeling_msas_vec.rda')


# Function to find counties
# The main info we need is the fips state and fips county codes. We can use use these codes in to census api to pull migration data
find_counties <- function(msa_name) {
    counties <- crosswalk %>% 
    filter(cbsatitle == msa_name) %>% 
    distinct(cbsatitle, cbsacode, countycountyequivalent, statename, fipsstatecode, fipscountycode)
    
    return(counties)}

############################################
# Run function for MSAs with for loop
# Results are appended to empty data frame
############################################

# Initilize emplty data frame
msa_counties <- data_frame()

# For loop to get all relevent counties
for (msa in modeling_msas_vec){
  # users defined function
  counties <- find_counties(msa)
  # add msa to counties so we know what msa the county belong to
  counties$msa <- msa
  # Save results to date frame
  msa_counties <- rbind(msa_counties, counties)}

# Concatatate county and state name together
msa_counties <- msa_counties %>% 
  mutate(county_state = paste0(countycountyequivalent, ', ', statename))

# Save results
save(msa_counties, file = "programs/prepped_data/msa_counties.rda")

