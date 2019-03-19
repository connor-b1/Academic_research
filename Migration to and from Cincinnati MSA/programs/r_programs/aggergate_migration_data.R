# Set working directory
setwd("~/School/Fall_2018/Economic_Data_Analysis/ECON-6010-Group-Project")

# Packages
require(tidyverse)

# Load clean migration data
load('programs/prepped_data/clean_mig_data.rda')
# load('programs/msa_counties.rda')
# crosswalk <- read_csv('inputs/county_to_msa_crosswalk.csv')


# Migration at the MSA/year level
msa_year_agg <- clean_mig_data %>% 
  group_by(cbsatitle, year) %>% 
  summarise(total_mig_in = sum(moved_in, na.rm=TRUE),
            total_mig_out = sum(moved_out, na.rm=TRUE),
            total_mig_net = sum(moved_net, na.rm=TRUE))

# Save to prepped data
save(msa_year_agg, file = 'programs/prepped_data/msa_year_agg.rda')

# Migration in/out of Cincinnati MSA

# At state/year level
cincy_mig_state_year_agg <- clean_mig_data %>% 
  filter(cbsatitle == 'Cincinnati, OH-KY-IN') %>% 
  group_by(county_state, year) %>% 
  summarise(total_mig_in = sum(moved_in, na.rm=TRUE),
            total_mig_out = sum(moved_out, na.rm=TRUE),
            total_mig_net = sum(moved_net, na.rm=TRUE))

# At state level
cincy_mig_state_agg <- clean_mig_data %>% 
  filter(cbsatitle == 'Cincinnati, OH-KY-IN') %>% 
  group_by(county_state) %>% 
  summarise(total_mig_in = sum(moved_in, na.rm=TRUE),
            total_mig_out = sum(moved_out, na.rm=TRUE),
            total_mig_net = sum(moved_net, na.rm=TRUE))

# At county/year level
cincy_mig_county_year_agg <- clean_mig_data %>% 
  filter(cbsatitle == 'Cincinnati, OH-KY-IN') %>% 
  group_by(county1, year) %>% 
  summarise(total_mig_in = sum(moved_in, na.rm=TRUE),
            total_mig_out = sum(moved_out, na.rm=TRUE),
            total_mig_net = sum(moved_net, na.rm=TRUE))

# Add state and county name to cincy_mig_county_year_agg
cincy_mig_county_year_agg <- cincy_mig_county_year_agg %>% 
  left_join(clean_mig_data) %>% 
  select(names(cincy_mig_county_year_agg), county_name, county_state) %>% 
  distinct()

# At county level
cincy_mig_county_agg <- clean_mig_data %>% 
  filter(cbsatitle == 'Cincinnati, OH-KY-IN') %>% 
  group_by(county1) %>% 
  summarise(total_mig_in = sum(moved_in, na.rm=TRUE),
            total_mig_out = sum(moved_out, na.rm=TRUE),
            total_mig_net = sum(moved_net, na.rm=TRUE))

# Add state and county name to cincy_mig_county_agg
cincy_mig_county_agg <- cincy_mig_county_agg %>% 
  left_join(clean_mig_data) %>% 
  select(names(cincy_mig_county_agg), county_name, county_state) %>% 
  distinct()

# Save out csvs for heat maps
write.csv(x = cincy_mig_state_year_agg, file = 'outputs/cincy_mig_state_year_agg.csv')
write.csv(x = cincy_mig_state_agg, file = 'outputs/cincy_mig_state_agg.csv')
write.csv(x = cincy_mig_county_year_agg, file = 'outputs/cincy_mig_county_year_agg.csv')
write.csv(x = cincy_mig_county_agg, file = 'outputs/cincy_mig_county_agg.csv')

# Look at top state / counties for highest/lowest net migration

# Which state did we have the highest net migration over entire time period?
cincy_mig_state_agg %>% arrange(desc(total_mig_net))

# Which state did we have the lowest net migration over entire time period?
cincy_mig_state_agg %>% arrange(total_mig_net)

# Which state did we have the highest net migration over entire time period?
cincy_mig_county_agg %>% arrange(desc(total_mig_net))

# Which state did we have the lowest net migration over entire time period?
cincy_mig_county_agg %>% arrange(total_mig_net)
