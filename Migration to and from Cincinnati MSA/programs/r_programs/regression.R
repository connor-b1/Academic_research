# Modeling Code
head(modeling_df)

install.packages("corrplot")
require(corrplot)

# Numeric Data Frame for Correlation Plot
numeric_modeling_df <- modeling_df %>% 
  ungroup(cbsatitle) %>% 
  select(-cbsatitle, -year, -msa_code)

# Correlation Matrix
modeling_matrix <- data.matrix(numeric_modeling_df)
modeling_matrix <- cor(modeling_matrix)

corrplot(modeling_matrix, method = "circle")

everything_model <- lm(data = modeling_df, total_mig_net ~ pop + age + gdp + income + mean_commute_minutes + median_property_value + owner_occupied_housing_units + propertytax_none + propertytax_less800 + propertytax_800to1500 + propertytax_1500to2000 + propertytax_2000to3000 + propertytax_3000over)
summary(everything_model)

aicbic1 <- c(AIC(everything_model), BIC(everything_model))

aicbic1

model_2 <- lm(data = modeling_df, total_mig_net ~income + gdp + mean_commute_minutes + owner_occupied_housing_units + propertytax_none + propertytax_less800 + propertytax_800to1500 + propertytax_1500to2000 + propertytax_2000to3000 + propertytax_3000over)
summary(model_2)
aicbic2 <- c(AIC(model_2), BIC(model_2))

aicbic1
aicbic2

head(modeling_df)
