load("modeling_df.rda")
# We're trying to model total migration 

# Correlation plot to avoid multicollinearity (package: corrplot)
# Then star gazer on all these variables

require(tidyverse)
require(corrplot)

#This vector contains all the variables that will not be considered for the correlation matrix (some dropped for multicollinearity, some dropped because they aren't numeric)
correlation_kosher <- c("cbsatitle", "year", "total_mig_in", "total_mig_out", "pop_rank", "non_us_citizens", "us_citizens", "income_rank", "msa_code", "geo.y")
modeling_num_corr <- modeling_df[ , !(names(modeling_df) %in% correlation_kosher)]
modeling_matrix <- data.matrix(modeling_num_corr, rownames.force = NA)
correlation_matrix <- cor(modeling_matrix)
corrplot(correlation_matrix, method="circle")

#This vector contains all the variables that will not be considered for the regression calculation (some dropped for multicollinearity, some dropped because they aren't numeric)
regression_kosher <- c("cbsatitle", "year", "total_mig_in", "total_mig_out", "pop_rank", "non_us_citizens", "us_citizens", "income_rank", "msa_code", "geo.y")
modeling_reg <- modeling_df[ , !(names(modeling_df) %in% regression_kosher)]
everything_model <- lm(data = modeling_reg, total_mig_net ~ .)
summary(everything_model)

AIC(everything_model)
BIC(everything_model)

# Getting Rid of some more variables
regression_kosher1 <- c(regression_kosher <- c("cbsatitle", "year", "total_mig_in", "total_mig_out", "pop_rank", "non_us_citizens", "us_citizens", "income_rank", "msa_code", "geo.y", "gdp.x", "gdp.y")
)

# Regression seems too good to be true so I got rid of the observations where gdp was null. Brought it down to 303 observations
modeling_df1 <- modeling_df %>% 
  filter(gdp != "NA")
head(modeling_df1)

# AIC and BIC are much different
AIC(everything_model);BIC(everything_model)
AIC(gdp_not_null_model);BIC(gdp_not_null_model)

modeling_reg1 <- modeling_df1[ , !(names(modeling_df1) %in% regression_kosher1)]
gdp_not_null_model <- lm(data = modeling_reg1, total_mig_net ~ .)
summary(gdp_not_null_model)

# Residual Plots
residual_plot_everything <- ggplot(lm(total_mig_net ~ ., data = modeling_reg)) +
  geom_point(aes(x = .fitted, y = .resid)) +
  geom_abline(modeling_reg, slope = 0, intercept = 0, color = "red")
residual_plot_everything

residual_plot1 <- ggplot(lm(total_mig_net~ ., data = modeling_reg1)) + 
  geom_point(aes(x=.fitted, y=.resid)) + 
  geom_abline(modeling_reg1, slope = 0, intercept = 0)
residual_plot1

# Trying another regression
modeling_reg2 <- lm(data = modeling_df1, total_mig_net ~ gdp + age + income + mean_commute_minutes + 
                      owner_occupied_housing_units + propertytax_1500to2000 + propertytax_3000over + 
                      propertytax_800to1500 + propertytax_less800)
summary(modeling_reg2)
AIC(everything_model);BIC(everything_model)
AIC(gdp_not_null_model);BIC(gdp_not_null_model)
AIC(modeling_reg2);BIC(modeling_reg2)

# Dropping income
modeling_reg3 <- lm(data = modeling_df1, total_mig_net ~ gdp + age + mean_commute_minutes + 
                      owner_occupied_housing_units + propertytax_1500to2000 + propertytax_3000over + 
                      propertytax_800to1500 + propertytax_less800)
summary(modeling_reg3)
AIC(everything_model);BIC(everything_model)
AIC(gdp_not_null_model);BIC(gdp_not_null_model)
AIC(modeling_reg2);BIC(modeling_reg2)
AIC(modeling_reg3);BIC(modeling_reg3)

# AIC and BIC are lower with the third model
# Residual Plot with last regression
residual_plot2 <- ggplot(lm(data = modeling_df1, total_mig_net ~ gdp + age + mean_commute_minutes + 
                              owner_occupied_housing_units + propertytax_1500to2000 + propertytax_3000over + 
                              propertytax_800to1500 + propertytax_less800)) + 
  geom_point(aes(x=.fitted, y=.resid)) + 
  geom_abline(modeling_df1, slope = 0, intercept = 0)
residual_plot2


rm(list=ls())
