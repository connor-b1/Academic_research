
# HAS BEEN INTEGRATED INTO "modelling_aggregation_df.R"

library(tidyverse)
install.packages("gsubfn")
library(gsubfn)

load("modeling_df.rda")

drop <- c("gdp")
modeling <- modeling_df[ , !(names(modeling_df) %in% drop)]

gdp_2013 <- read_csv("msa_gdp_2013.csv")
gdp_2014 <- read_csv("msa_gdp_2014.csv")
gdp_2015 <- read_csv("msa_gdp_2015.csv")

msa_gdp <- bind_rows(gdp_2013,gdp_2014,gdp_2015)

cbsatitle <- c(msa_gdp$cbsatitle)
msa_gdp$cbsatitle <- gsub(" \\(Metropolitan Statistical Area)", "", cbsatitle)

modeling_df <- msa_gdp %>% 
  right_join(modeling_df, by = c("cbsatitle", "year"))
  
head(modeling_df)

# HAS BEEN INTEGRATED INTO "modelling_aggregation_df.R"
