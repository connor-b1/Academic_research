############################################################################
# Creating a modelling dataframe composed of all the Independent Variables

require(tidyverse)
require(gsubfn)

# Manipulated and Cleaned Migration Data
# CBSAtitle = "departing from" county, county1 = arriving to MSA
load("programs/prepped_data/clean_mig_data.rda")

# Generic Demographic Information
load("programs/prepped_data/acs_data.rda")

# Now for more Specific Candidate Indepedent Variables, such as:

#Property Tax Data
load("programs/prepped_data/property_tax_data.rda")

# GDP Data
gdp_2013 <- read_csv("programs/prepped_data/msa_gdp_2013.csv")
gdp_2014 <- read_csv("programs/prepped_data/msa_gdp_2014.csv")
gdp_2015 <- read_csv("programs/prepped_data/msa_gdp_2015.csv")
msa_gdp <- bind_rows(gdp_2013,gdp_2014,gdp_2015)
#Switched data sources to a richer dataset, previously used:
#load("programs/prepped_data/gdp_data.rda")

###################
# Decided not to use crime dataset because there's only three years of data (only two of which overlap with the migration data).


######################################################################################
# Modelling Data:
# all of which is already aggregated to MSA level: ACS, clean migration data, property tax, GDP
# All that must be done is to standardize the ACS column names to act as a join key

#MSA for __ year: migration in, migration out, net migration, then all the indpendent variables tied back to MSA


#################################
#MSA labels and content

# Standardize the name of the msa column in each dataset to match the column name in clean_mig
# Standardized column name = cbsatitle

acs_cols <- colnames(acs_data)
acs_cols_revised <- sub("geo_name", "cbsatitle", acs_cols)
colnames(acs_data) <- acs_cols_revised

prop_cols <- colnames(df_prop_tax)
prop_cols_revised <- sub("geo_name", "cbsatitle", prop_cols)
colnames(df_prop_tax) <- prop_cols_revised

gdp_cols <- colnames(gdp_data)
gdp_cols_revised <- sub("msa_title", "cbsatitle", gdp_cols)
colnames(gdp_data) <- gdp_cols_revised

# Clean up values
rm(list = c('acs_cols', 'acs_cols_revised', 'gdp_cols', 'gdp_cols_revised', 'prop_cols', 'prop_cols_revised'))

# This is to standardize the *content* of the data itself in the msa column
# require(gsubfn)
cbsatitle <- gsub(" Metro Area", "", acs_data$cbsatitle)
cbsatitle <- gsub(" Micro Area", "", cbsatitle)
acs_data$cbsatitle <- cbsatitle

cbsatitle <- gsub(" Metro Area", "", df_prop_tax$cbsatitle)
cbsatitle <- gsub(" Micro Area", "", cbsatitle)
df_prop_tax$cbsatitle <- cbsatitle

cbsatitle <- c(msa_gdp$cbsatitle)
msa_gdp$cbsatitle <- gsub(" \\(Metropolitan Statistical Area)", "", cbsatitle)

####################################
# Change data formats for joins
acs_data$year <- as.double(acs_data$year)

gdp_data$year <- gsub('-01-01', '', gdp_data$date)
gdp_data$year <- as.double(gdp_data$year)
gdp_data$date <- NULL

df_prop_tax$year <- as.double(df_prop_tax$year)

####################################
# Aggergate state/year migration
msa_state_year_agg <- clean_mig_data %>% 
  group_by(cbsatitle, year) %>% 
  summarise(total_mig_in = sum(moved_in, na.rm=TRUE),
            total_mig_out = sum(moved_out, na.rm=TRUE),
            total_mig_net = sum(moved_net, na.rm=TRUE))

#########################################################################
############# Now to actually join ######################################

# Join together clean_mig_data and acs
cleanmig_acs <- msa_state_year_agg %>% 
  filter(year %in% c('2013', '2014', '2015')) %>% 
  left_join(acs_data, by = c("cbsatitle", "year"))

# Add GDP data
cleanmig_acs_GDP <- msa_gdp %>% 
  right_join(cleanmig_acs, by = c("cbsatitle", "year"))

# Finally, add property tax
cleanmig_acs_GDP_pt <- left_join(cleanmig_acs_GDP, df_prop_tax, by = c("cbsatitle", "year"))

########

modeling_df <- cleanmig_acs_GDP_pt
modeling_df$geo.x <- NULL
modeling_df <- modeling_df[, -grep("_moe", colnames(modeling_df))]
modeling_df
save(modeling_df, file = 'programs/prepped_data/modeling_df.rda')

# Contains all modelling data, with standardized content for MSA data, which is a column named "cbsatitle", upon which it has been joined.

