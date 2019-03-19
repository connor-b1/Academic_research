setwd("~/School/Fall_2018/Economic_Data_Analysis/ECON-6010-Group-Project")

# Load libraries and data
library(tidyverse)
library(ggrepel)
load('programs/prepped_data/gdp_data.Rda')
load('programs/prepped_data/migration_data.Rda')
load('programs/prepped_data/acs_data.Rda')
load('programs/prepped_data/crime_data.Rda')
load('programs/prepped_data/property_tax_data.Rda')
load('programs/prepped_data/msa_counties.Rda')
load('programs/prepped_data/clean_mig_data.Rda')
load('programs/prepped_data/comparison_msas_vec.rda')
crosswalk <- read_csv('inputs/county_to_msa_crosswalk.csv')

# Migration trends

# Aggergate to total year migration
yearly_mig <- clean_mig_data %>% 
  filter(cbsatitle %in% comparison_msas_vec) %>% 
  group_by(cbsatitle, year) %>% 
  summarise(total_moved_in = sum(moved_in, na.rm = T),  
            total_moved_out = sum(moved_out, na.rm = T), 
            total_moved_net = sum(moved_net, na.rm = T))

# ggplot of time tread

# Convert year to date type
yearly_mig$year <- as.Date(as.character(yearly_mig$year), format = "%Y")
breaks <- seq.Date(as.Date('2009/01/01'), as.Date('2015/01/01'), 'years')

# Cincinnati ggplot time plot
cincinnati_mig_trend <- yearly_mig %>% 
  filter(cbsatitle == 'Cincinnati, OH-KY-IN') %>% 
  mutate(label = if_else(year == max(year), as.character(cbsatitle), NA_character_)) %>%
  ggplot(aes(x = year, y=total_moved_net, color = cbsatitle)) + geom_line(size = 2) +
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) +
  ggtitle('Cincinnati Net Migration Trend by MSA') +
  ylab('Total Net Migration') +
  xlab('Year') +
  scale_y_continuous(limits = c(-8000, 5000), breaks=seq(-8000, 5000, 1000)) +
  #scale_x_date( breaks = breaks, date_labels = "%Y") +
  labs(color = 'MSA') +
  theme(legend.position="none")

ggsave('outputs/cincinnati_mig_trend.png', cincinnati_mig_trend)

# ggplot time plot
msa_mig_trend <- yearly_mig %>% 
  mutate(label = if_else(year == max(year), as.character(cbsatitle), NA_character_)) %>%
  ggplot(aes(x = year, y=total_moved_net, color = cbsatitle)) + geom_line(size = 2) +
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE,
                   show.legend = F) +
  ggtitle('Net Migration Trend by MSA') +
  ylab('Total Net Migration') +
  xlab('Year') +
  scale_y_continuous(limits = c(-40000, 85000), breaks=seq(-40000, 85000, 10000)) +
  labs(color = 'MSA')  +
  theme(legend.position="none")

ggsave('outputs/msa_mig_trend.png', msa_mig_trend)
