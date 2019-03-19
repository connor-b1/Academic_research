library(tidyverse)
load('programs/clean_mig_data.rda')


head(clean_mig_data)
cincy_msa_mig <- clean_mig_data %>% 
  filter(cbsatitle == 'Cincinnati, OH-KY-IN')
head(cincy_msa_mig)

# Creating new Net Migration variable
cincy_msa_mig <- cincy_msa_mig %>% 
  mutate(county_year_net = moved_in - moved_out, na.rm = T)
cincy_msa_mig

# Getting rid of old moved net
cincy_msa_mig <- cincy_msa_mig %>% 
  select(cbsatitle, county1, moved_in, moved_out, county_year_net, year)
cincy_msa_mig

# Getting County Totals for the period
cincy_msa_mig_total_net <- cincy_msa_mig %>% 
  group_by(county1) %>% 
  mutate(county_total_net = sum(county_year_net), na.rm = T)

# Arranging
cincy_msa_mig_total_net <- cincy_msa_mig_total_net %>% arrange(desc(county_total_net))
cincy_msa_mig_total_net


cincy_msa_mig_total_net %>% 
  distinct(year, county_total_net)

# Net totals for each year
cincy_msa_mig_total_net <- cincy_msa_mig_total_net %>% 
  group_by(year) %>% 
  mutate(year_net_total = sum(county_year_net, na.rm = T)) %>% 
  arrange(desc(year_net_total))
cincy_msa_mig_total_net

# Changing CBSA title and decreasing amount of seen variables
cincy_msa_mig_total_net <- cincy_msa_mig_total_net %>% 
  mutate(metro_area = cbsatitle) %>% 
  select(metro_area, county1, county_year_net, county_total_net, year, year_net_total)
cincy_msa_mig_total_net

# Table with just the year totals
year_totals <- cincy_msa_mig_total_net %>% 
  distinct(year, year_net_total)
year_totals

# County Total Table
county_totals <- cincy_msa_mig_total_net %>% 
  ungroup(year) %>% 
  distinct(county1, county_total_net) %>% 
  select(county1, county_total_net) %>% 
  arrange(county_total_net)

# Year plot
year_totals_plot <- year_totals %>% 
  ggplot(aes(x = year, y = year_net_total)) +
  geom_line(col = 'red', size = 3) +
  xlab('Year') +
  ylab('Net Migration Flow') +
  ggtitle('Cincinnati MSA Net Migration Flows') +
  theme_dark() +
  geom_point(aes(size = 4, shape = 17))
year_totals_plot

# Most Inflows to Cincinnati for each year
cincy_msa_mig_total_net %>%
  filter(year == 2009) %>%
  arrange(desc(county_year_net)) %>%
  select(county1, county_year_net)
# 2015 Montgomery County, Ohio 942
# 2014 Montgomery County, Ohio 1599
# 2013 Montgomery County, Ohio 701
# 2012 Anchorage Municipality, Alaska 466
# 2011 Anchorage Municipality, Alaska 534
# 2010 Montgomery County, Ohio 1232
# 2009 Montgomery County, Ohio 1799

# Most Outflows from Cincinnati for each year
cincy_msa_mig_total_net %>%
  filter(year == 2010) %>%
  arrange(county_year_net) %>%
  select(county1, county_year_net)
# 2015 Fayette County, Kentucky -910
# 2014 Fayette County, Kentucky -1181
# 2013 Fayette County, Kentucky -1130
# 2012 Franklin County, Ohio -1502
# 2011 Franklin County, Ohio -866
# 2010 Madison County, Ohio -710
# 2009 Madison County, Ohio -626







