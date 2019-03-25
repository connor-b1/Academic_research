# Remember to set working directory

library(tidyverse)
library(readxl)

data <- read_excel("auction_data_FINAL_DATASET for export.xlsx")

raised <- data$total_amount_raised
countries <- data$Country
year <- data$Year_of_Auction
licenses <- data$number_licenses
validity <- data$license_validity_yrs
nasdaq <- data$NASDAQ
hhi <- data$HHI
gnipc <- data$GNIPC
pop_dens <- data$Population_Density
urb <- data$urban_rate
total_pop <- data$total_population
legal <- data$Legal
lib_dmy <- factor(data$Liberalization)
auct_dmy <- factor(data$Auction_Dummy)
MP <- data$MP
THRT <- factor(data$THRT)
data1 <- mutate(data, data$total_amount_raised/data$number_licenses)
avg_p <- as.numeric(data1$`data$total_amount_raised/data$number_licenses`)
                       

is.factor(THRT)
head(THRT.f)

##########################
# In the case of NA: using mean value for the variable:
avg_p[is.na(avg_p)] <- mean(avg_p, na.rm = TRUE)
validity[is.na(validity)] <- mean(validity, na.rm = TRUE)
legal[is.na(legal)] <- mean(legal, na.rm = TRUE)
hhi[is.na(hhi)] <- mean(hhi, na.rm = TRUE)

##########################
# Data exploration
ln_avgp <- log(avg_p)

rplot(raised)
ln_raised <- log(raised)
barplot(ln_raised)

barplot(licenses)
ln_licenses <- log(licenses)
barplot(ln_licenses)

barplot(validity)
ln_validity <- log(validity)
barplot(ln_validity)

barplot(nasdaq)
ln_nasdaq <- log(nasdaq)
barplot(ln_nasdaq)

barplot(hhi)
ln_hhi <- log(hhi)
barplot(ln_hhi)

barplot(gnipc)
ln_gnipc <- log(gnipc)
barplot(ln_gnipc)

barplot(pop_dens)
ln_popdens <- log(pop_dens)
barplot(ln_popdens)

barplot(urb)
ln_urb <- log(urb)
barplot(ln_urb)

barplot(total_pop)
ln_tpop <- log(total_pop)
barplot(ln_tpop)

barplot(legal)
ln_legal <- log(legal)
barplot(ln_legal)

ln_MP <- log(MP)

# Log seems to work well for raised, licenses, pop_dens, total_pop
# Log doesn't seem to work well for validity, nasdaq, hhi, gnipc, urb

Use_these_for_regression_model <- data_frame(ln_raised, ln_licenses, validity, nasdaq, hhi, gnipc, ln_popdens, urb, ln_tpop, legal, lib_dmy, auct_dmy)
Use_these_for_regression_model

##########################
# MODELING

####
model1 <- lm(ln_avgp ~ ln_validity + ln_nasdaq + ln_hhi + ln_gnipc + ln_popdens + ln_legal + lib_dmy + auct_dmy + THRT + ln_MP)

summary(model1)
AIC(model1)
BIC(model1)

####
model2 <- lm(ln_avgp ~ ln_validity + ln_nasdaq + ln_hhi + ln_gnipc + ln_popdens + ln_legal + lib_dmy)
summary(model2)
AIC(model2)
BIC(model2)

# Graphs aside, log works better for all of them

####
model3_no_lnurb <- lm(ln_avgp ~  ln_validity + ln_nasdaq + ln_hhi + ln_gnipc + ln_popdens + ln_legal)

summary(model3_no_lnurb)
AIC(model3_no_lnurb)
BIC(model3_no_lnurb)

#########

# use the thrown out variables to make an IV
# Also Bruesch-Pagan, Durbin-Watson, DEFINITELY cover the endodeneity topic clearly


#########
rm(list=ls())

View(legal)
