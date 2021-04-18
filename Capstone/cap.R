
#Connor Boone
#University of Cincinnati, Charles H. Lindner College of Business
#Department of Economics
#Masters Practicum, Summer 2019

#Determing the effect of homebuyer downpayment assistance on foreclosure rates in Covington, Kentucky

####notes#### 

#R version 3.6.01 REQUIRED for ggplot2 survival curves

####required packages####
#install.packages("survival")
#install.packages("lubridate")
#install.packages("ggplot2")
#install.packages('survminer')
#install.packages("dummies")
#install.packages("readxl")
#install.packages("tidyverse")
#install.packages("stargazer")
#install.packages("sjPlot")

library("sjPlot")
library("stargazer")
library("survminer")
library("ggplot2")
library("tidyverse")
library("readxl")
library("survival")
library("dummies")
library("lubridate")

####data import####

#SALES DATA#

##2014 SALES
jan14 <- read.table("MNTHSALEJAN14.txt", sep='~', fill = T)
#feb14 <- read.table("MNTHSALEFEB14.txt", sep='~', fill = T) missing
mar14 <- read.table("MNTHSALEAPR20.txt", sep='~', fill = T)
#apr14 <- read.table("MNTHSALEAPR20.txt", sep='~',fill = T) missing
may14 <- read.table("MNTHSALEMAY14.txt", sep='~', fill = T)
jun14 <- read.table("MNTHSALEJUL14.txt", sep='~', fill = T)
jul14 <- read.table("MNTHSALEAUG14.txt", sep='~', fill = T)
aug14 <- read.table("MNTHSALESEPT14.txt", sep='~', fill = T)
sep14 <- read.table("MNTHSALEOCT14.txt", sep='~', fill = T)
oct14 <- read.table("MNTHSALENOV14.txt", sep='~', fill = T)
#nov14 <- read.table("MNTHSALENOV14.txt", sep='~', fill = T) missing
dec14 <- read.table("MNTHSALEJAN15.txt", sep='~', fill = T) 

##2015 SALES
#jan15 <- read.table("MNTHSALEJAN15.txt", sep='~', fill = T) missing
feb15 <- read.table("MNTHSALEMAR15.txt", sep='~', fill = T)
mar15 <- read.table("MNTHSALEAPR15.txt", sep='~', fill = T)
apr15 <- read.table("MNTHSALEMAY15.txt", sep='~', fill = T)
may15 <- read.table("MNTHSALEJUN15.txt", sep='~', fill = T) 
jun15 <- read.table("MNTHSALEJULY15.txt", sep='~', fill = T)
jul15 <- read.table("MNTHSALEAUG15.txt", sep='~', fill = T)
aug15 <- read.table("MNTHSALESEP15.txt", sep='~', fill = T)
sep15 <- read.table("MNTHSALEOCT15.txt", sep='~', fill = T)
oct15 <- read.table("MNTHSALENOV15.txt", sep='~', fill = T)
#nov15 <- read.table("MNTHSALENOV15.txt", sep='~', fill = T) MISSING
dec15 <- read.table("MNTHSALEJAN16.txt", sep='~', fill = T)

##2016 SALES
jan16 <- read.table("MNTHSALEFEB16.txt", sep='~', fill = T)
feb16 <- read.table("MNTHSALEMAR16.txt", sep='~', fill = T) 
#mar16 <- read.table("MNTHSALEMAR16.txt", sep='~', fill = T) MISSING
apr16 <- read.table("MNTHSALEAPR16.txt", sep='~', fill = T)
#may16 <- read.table("MNTHSALEMAY16.txt", sep='~', fill = T) MISSING
jun16 <- read.table("MNTHSALEJUN16.txt", sep='~', fill = T)
jul16 <- read.table("MNTHSALEJUL16.txt", sep='~', fill = T)
aug16 <- read.table("MNTHSALEAUG16.txt", sep='~', fill = T)
sep16 <- read.table("MNTHSALESEPT16.txt", sep='~', fill = T)
oct16 <- read.table("MNTHSALEOCT.txt", sep='~', fill = T)
nov16 <- read.table("MNTHSALENOV16.txt", sep='~', fill = T)
#dec16 <- read.table("MNTHSALEAPR16.txt", sep='~', fill = T) ON 2017 FILES

### negotiating datasets to match
totsales14 = rbind(jan14, mar14, may14, jun14, jul14, aug14, sep14, oct14, dec14)
totsales14
sales14 = select(totsales14, V8, V10, V12)
names(sales14) <- c("id", "date", "saleval")
sales14$id <- gsub('-', '.', sales14$id)
sales14$date <- as.Date(sales14$date , "%m/%d/%y")
sales14$date <- floor_date(sales14$date, "month")
sales14

totsales15 = rbind(feb15, mar15, apr15, may15, jun15, jul15, aug15, sep15, oct15, dec15)
sales15 = select(totsales15, V8, V10, V12)
names(sales15) <- c("id", "date", "saleval")
sales15$id <- gsub('-', '.', sales15$id)
sales15$date <- as.Date(sales15$date ,"%m/%d/%y")
sales15$date <- floor_date(sales15$date, "month")
sales15

totsales16 = rbind(jan16, feb16, apr16,jun16, jul16, aug16, sep16, oct16)
sales16 = select(totsales16, V8, V10, V12)
names(sales16) <- c("id", "date", "saleval")
sales16$id <- gsub('-', '.', sales16$id)
sales16
sales16$date <- as.Date(sales16$date , "%m/%d/%y")
sales16$date <- floor_date(sales16$date, "month")
sales16
##2017-2018 sales
library(readxl)
sales17to18 <- read_excel("~/Desktop/capstone/comprehensive sale data 2017 to 2019.xlsx", 
                                                   col_types = c("text", "blank", "blank", 
                                                                 "blank", "blank", "blank", "blank", 
                                                                 "date", "blank", "blank", "blank", 
                                                                 "blank", "blank"))
names(sales17to18) <- c("id", "date")
sales17to18$id <- gsub('-', '.', sales17to18$id)
sales17to18$date <- as.Date(sales17to18$date , "%m/%d/%y")
sales17to18$date <- floor_date(sales17to18$date, "month")
sales17to18
sales17to18 = sales17to18[sales17to18$date <= "2018-12-31",]
sales17to18

totsales = rbind(sales14, sales15, sales16, sales17to18)
names(totsales) = c("id", "saledate")
##foreclosures
library(readxl)
fclose <- read_excel("~/Desktop/capstone/foreclosure.b.xlsx", 
                     col_types = c("text", "date", "blank"))
names(fclose) <- c("id", "fdate")
fclose$id <- gsub('-', '.', fclose$id)
fclose$fdate <- as.Date(fclose$fdate)
fclose$fdate <- floor_date(fclose$fdate, "month")
fclose = select(fclose, id, fdate)
fclose

##covariates (direct import required)
covars <- read_excel("new data.xlsx", col_types = c("text", 
                                                    "text", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric"))
View(covars)

##hap list
hap_list <- read_excel("~/Desktop/capstone/hap list.xlsx")

save.image(file = "complete_data.RData")
save(totsales, fclose, hap_list, covars, file = "sales-and-covar-data.RData")
save(sales14, sales15, sales16, sales17to18, totsales, file = "sales-data.RData")

####descriptive statistics about covariates* ####

##code for txt output

```{r word_table, comment = ''}
stargazer(coxph141, coxph151, coxph161, type = 'text')

```

```{r word_table, comment = ''}
stargazer(coxph14, coxph15, coxph16, type = 'text')

```

#code for htm output

```{r word_table, comment = ''}
stargazer(coxph141, coxph151, coxph161, title = "Cox Proportional Analysis of Foreclosure Rates", type = 'latex', out = "coxphhap.htm")

```

#summary export 

library(stargazer)

stargazer(cohort161, type = "text", title="2016 Property Sales Summary Statistics", digits=1, out = "cohort14 11.htm")
data1

####value matching and cohort identification ####

#cohort 14
cohort14 = merge(sales14, covars, by = "id")
#excluding NAs
cohort141 = cohort14[!(is.na(cohort14$id) | cohort14$id==""), ]
cohort141 = cohort14[!(is.na(cohort14$ADDRESS) | cohort14$ADDRESS==""), ]
cohort141 = cohort14[!(is.na(cohort14$BLDG_SQFT) | cohort14$BLDG_SQFT==""), ]
cohort141 = cohort14[!(is.na(cohort14$age) | cohort14$age==""), ]

sum(cohort14$hap)
###unifying data sets

cohort14 = merge(cohort14, fclose, by = "id", all.x = T) #merging fclose
cohort141 = merge(cohort141, fclose, by = "id", all.x = T)

sales15to18 = totsales[totsales$saledate >= "2015-01-01",] #excluding 14 from total sales
cohort14 = merge(cohort14, sales17to18, by = "id", all.x = T)
cohort141 = merge(cohort141, sales17to18, by = "id", all.x = T)

cohort14 = select(cohort14, id, date, hap, fdate, saledate) #selecting values
cohort141 = select(cohort141, id, date, hap, fdate, saledate, age, BLDG_SQFT)
cohort14$fdummy <- ifelse(is.na(cohort14$fdate), 0, 1) # creating dummies
cohort14$sdummy <- ifelse(is.na(cohort14$saledate), 0, 1)
cohort141$fdummy <- ifelse(is.na(cohort141$fdate), 0, 1) # creating dummies
cohort141$sdummy <- ifelse(is.na(cohort141$saledate), 0, 1)

write.csv(cohort141, file = "cohort141.csv")
write.csv(cohort14, file = "cohort14.csv")
write.csv(totsales14,file = "14.csv")

#cohort 15
cohort15 = merge(sales15, covars, by = "id")
#excluding NAs
cohort151 = cohort15[!(is.na(cohort15$id) | cohort15$id==""), ]
cohort151 = cohort15[!(is.na(cohort15$ADDRESS) | cohort15$ADDRESS==""), ]
cohort151 = cohort15[!(is.na(cohort15$BLDG_SQFT) | cohort15$BLDG_SQFT==""), ]
cohort151 = cohort15[!(is.na(cohort15$age) | cohort15$age==""), ]

sum(cohort151$hap)
###unifying data sets

cohort15 = merge(cohort15, fclose, by = "id", all.x = T) #merging fclose
cohort151 = merge(cohort151, fclose, by = "id", all.x = T)

sales14to18 = totsales[totsales$saledate >= "2014-01-01",] #excluding 15 from total sales
cohort15 = merge(cohort15, sales16to18, by = "id", all.x = T)
cohort151 = merge(cohort151, sales16to18, by = "id", all.x = T)

match(cohort15$id, sales16to18$id)


cohort15 = select(cohort15, id, date, hap, fdate, saledate) #selecting values
cohort151 = select(cohort151, id, date, hap, fdate, saledate, age, BLDG_SQFT)
cohort15$fdummy <- ifelse(is.na(cohort15$fdate), 0, 1) # creating dummies
cohort15$sdummy <- ifelse(is.na(cohort15$saledate), 0, 1)
cohort151$fdummy <- ifelse(is.na(cohort151$fdate), 0, 1) # creating dummies
cohort151$sdummy <- ifelse(is.na(cohort151$saledate), 0, 1)

write.csv(cohort151, file = "cohort151.csv")
write.csv(cohort15, file = "cohort15.csv")
write.csv(totsales15,file = "15.csv")
#save(cohort15, file = "cohort15.RData")

#cohort 16
cohort16 = merge(sales16, covars, by = "id")
#excluding NAs
cohort161 = cohort16[!(is.na(cohort16$id) | cohort16$id==""), ]
cohort161 = cohort16[!(is.na(cohort16$ADDRESS) | cohort16$ADDRESS==""), ]
cohort161 = cohort16[!(is.na(cohort16$BLDG_SQFT) | cohort16$BLDG_SQFT==""), ]
cohort161 = cohort16[!(is.na(cohort16$age) | cohort16$age==""), ]

sum(cohort161$hap)
###unifying data sets

cohort16 = merge(cohort16, fclose, by = "id", all.x = T) #merging fclose
cohort161 = merge(cohort161, fclose, by = "id", all.x = T)

sales17to18 = totsales[totsales$saledate >= "2017-01-01",] #excluding 16 from total sales
cohort16 = merge(cohort16, sales17to18, by = "id", all.x = T)
cohort161 = merge(cohort161, sales17to18, by = "id", all.x = T)

cohort16 = select(cohort16, id, date, hap, fdate, saledate) #selecting values
cohort161 = select(cohort161, id, date, hap, fdate, saledate, age, BLDG_SQFT)
cohort16$fdummy <- ifelse(is.na(cohort16$fdate), 0, 1) # creating dummies
cohort16$sdummy <- ifelse(is.na(cohort16$saledate), 0, 1)
cohort161$fdummy <- ifelse(is.na(cohort161$fdate), 0, 1) # creating dummies
cohort161$sdummy <- ifelse(is.na(cohort161$saledate), 0, 1)

write.csv(cohort161, file = "cohort161.csv")
write.csv(cohort16, file = "cohort16.csv")
write.csv(sales16,file = "16.csv")
#save(cohort16, file = "cohort16.RData")
#save(cohort161, file = "cohort161.RData")

####survival functions ####

#cohort: 2014 
cohort14 = read.csv("cohort14.csv")

help("stargazer-package")
#save(cohort14, file = "cohort14.RData")
attach(cohort14)

co14 = cbind(saleval, hap)

### use sales value instead .. 
# Kaplan-Meier non-parametric analysis
kmsurvival14a <- survfit(Surv(spell,fdummy) ~ 1)
summary(kmsurvival14a)
plot(kmsurvival14a, xlab="Months", ylab="Survival Probability")
 
# Kaplan-Meier non-parametric analysis by group
kmsurvival14b <- survfit(Surv(spell, fdummy) ~hap) #using hap
summary(kmsurvival14b)
plot(kmsurvival14b, xlab="Months", ylab="Survival Probability")

# Cox proportional hazard model - coefficients and hazard rates
coxph14 <- coxph(Surv(spell,fdummy) ~ co14, method="breslow")
summary(coxph14)

#### more covars: log(sqft) and log(age), 
#### new cohort: 20141
cohort141 = read.csv("cohort141.csv")
#save(cohort141, file = "cohort141.RData")
attach(cohort141)

co141 = cbind(saleval, hap, age, sqft)

# Kaplan-Meier non-parametric analysis
kmsurvival141a <- survfit(Surv(spell,fdummy) ~ 1)
summary(kmsurvival141a)
plot(kmsurvival141a, xlab="Months", ylab="Survival Probability")

# Kaplan-Meier non-parametric analysis by group
kmsurvival141b <- survfit(Surv(spell, fdummy) ~hap) #using hap
summary(kmsurvival14b)
plot(kmsurvival14b, xlab="Months", ylab="Survival Probability")


# Cox proportional hazard model - coefficients and hazard rates
coxph141 <- coxph(Surv(spell,fdummy) ~ co141, method="breslow")
summary(coxph141)

ggforest(coxph151, data = cohort151)


#cohort: 2015
cohort15 = read.csv("cohort15.csv")
#save(cohort15, file = "cohort15.RData")
attach(cohort15)

co15 = cbind(saleval, hap)
# Kaplan-Meier non-parametric analysis
kmsurvival15a <- survfit(Surv(spell,fdummy) ~ 1)
summary(kmsurvival15a)
plot(kmsurvival15a, xlab="Months", ylab="Survival Probability")

# Kaplan-Meier non-parametric analysis by group
kmsurvival15b <- survfit(Surv(spell, fdummy) ~hap) #using hap
summary(kmsurvival15b)
plot(kmsurvival15b, xlab="Months", ylab="Survival Probability")


# Cox proportional hazard model - coefficients and hazard rates
coxph15 <- coxph(Surv(spell,fdummy) ~ co15, method="breslow")
summary(coxph15)

#### more covars: log(sqft) and log(age)
#### new cohort: 20151

cohort151 = read.csv("cohort151.csv")
attach(cohort151)
#save(cohort151, file = "cohort151.RData")

co151 = cbind(saleval, hap, age, sqft)
shap = (saleval*hap)
co151 = cbind(shap, age, sqft)

View(cohort161)
sum(cohort161$fdummy)
# Kaplan-Meier non-parametric analysis
kmsurvival151a <- survfit(Surv(spell,fdummy) ~ 1)
summary(kmsurvival151s)
plot(kmsurvival151a, xlab="Months", ylab="Survival Probability")

# Kaplan-Meier non-parametric analysis by group
kmsurvival151b <- survfit(Surv(spell, fdummy) ~hap) #using hap
summary(kmsurvival151b)
plot(kmsurvival151b, xlab="Months", ylab="Survival Probability")

# Cox proportional hazard model - coefficients and hazard rates
coxph151 <- coxph(Surv(spell,fdummy) ~ co151, method="breslow")
summary(coxph151)

#cohort: 2016
cohort16 = read.csv("cohort16.csv")
attach(cohort16)
#save(cohort16, file = "cohort16.RData")

co16 = cbind(saleval, hap)
# Kaplan-Meier non-parametric analysis
kmsurvival16a <- survfit(Surv(spell,fdummy) ~ 1)
summary(kmsurvival16a)
plot(kmsurvival16a, xlab="Months", ylab="Survival Probability")

# Kaplan-Meier non-parametric analysis by group
kmsurvival16b <- survfit(Surv(spell, fdummy) ~hap) #using hap
summary(kmsurvival16b)
plot(kmsurvival16b, xlab="Months", ylab="Survival Probability")


# Cox proportional hazard model - coefficients and hazard rates
coxph16 <- coxph(Surv(spell,fdummy) ~ hap, method="breslow")
summary(coxph16)

#### more covars: log(sqft) and log(age)
#### new cohort: 20161

cohort161 = read.csv("cohort161.csv")
#save(cohort161, file = "cohort161.RData")
attach(cohort161)

co161 = cbind(saleval, hap, age, sqft)

# Kaplan-Meier non-parametric analysis
kmsurvival161a <- survfit(Surv(spell,fdummy) ~ 1)
summary(kmsurvival161a)
plot(kmsurvival161a, xlab="Months", ylab="Survival Probability")

# Kaplan-Meier non-parametric analysis by group
kmsurvival161b <- survfit(Surv(spell, fdummy) ~hap) #using hap
summary(kmsurvival161b)
plot(kmsurvival161b, xlab="Months", ylab="Survival Probability")


# Cox proportional hazard model - coefficients and hazard rates
coxph161 <- coxph(Surv(spell,fdummy) ~ co161, method="breslow")
summary(coxph161)


#####aesthetics ####

# Drawing survival curves

#generating hazard values

ggforest(coxph161, data = cohort161)


#cohort 2014

ggsurvplot(kmsurvival14a, data = cohort14, title = "2014 Foreclosure Survival Rates", palette = "#E7B800", xlab = "Months", 
           break.time.by =6, risk.table = TRUE)

ggsurvplot(kmsurvival14b, data = cohort14, title = "2014 Foreclosure Survival Rates", palette = c("#E7B800", "#2E9FDF"), xlab = "Months", 
           break.time.by =6, risk.table = TRUE)

# Combine on the same plot

fit14 <- list(Combined = kmsurvival14a, HAPvsNonHAP = kmsurvival14b)
ggsurvplot_combine(fit14, cohort14, title = "2014 Foreclosure Survival Rates", palette = c("#E7B800","#2E9FDF","#D16103"), xlab = "Months", 
                   break.time.by =6, risk.table = TRUE)

#cohort 2015

ggsurvplot(kmsurvival15a, data = cohort15, title = "2015 Foreclosure Survival Rates", palette = "#E7B800", xlab = "Months", 
           break.time.by =6, risk.table = TRUE)

ggsurvplot(kmsurvival15b, data = cohort15, title = "2015 Foreclosure Survival Rates", palette = c("#E7B800", "#2E9FDF"), xlab = "Months", 
           break.time.by =6, risk.table = TRUE)

# Combine on the same plot
fit15 <- list(Combined = kmsurvival15a, HAPvsNonHAP = kmsurvival15b)
ggsurvplot_combine(fit15, cohort15, title = "2015 Foreclosure Survival Rates", palette = c("#E7B800","#2E9FDF","#D16103"), xlab = "Months", 
                   break.time.by =6, risk.table = TRUE)

#cohort 2016 

ggsurvplot(kmsurvival16a, data = cohort16, title = "2016 Foreclosure Survival Rates",palette = "#E7B800", xlab = "Months", 
           break.time.by =6, risk.table = TRUE)

ggsurvplot(kmsurvival16b, data = cohort16, title = "2016 Foreclosure Survival Rates", palette = c("#E7B800", "#2E9FDF"),  xlab = "Months", 
           break.time.by =6, risk.table = TRUE)

# Combine on the same plot
fit16 <- list(Combined = kmsurvival16a, HAPvsNonHAP = kmsurvival16b)
ggsurvplot_combine(fit, cohort16, title = "2016 Foreclosure Survival Rates", palette = c("#E7B800","#2E9FDF","#D16103"), xlab = "Months", 
                   break.time.by =6, risk.table = TRUE)

#cohort 20141

ggsurvplot(kmsurvival141a, data = cohort141, title = "2014 Foreclosure Survival Rates",palette = "#E7B800", xlab = "Months", 
           break.time.by =6, risk.table = TRUE)

ggsurvplot(kmsurvival141b, data = cohort16, title = "2014 Foreclosure Survival Rates", palette = c("#E7B800", "#2E9FDF"), xlab = "Months", 
           break.time.by =6, risk.table = TRUE)

# Combine on the same plot
fit141 <- list(Combined = kmsurvival141a, HAPvsNonHAP = kmsurvival141b)
ggsurvplot_combine(fit, cohort141, title = "2014 Foreclosure Survival Rates", palette = c("#E7B800","#2E9FDF","#D16103"), xlab = "Months", 
                   break.time.by =6,, risk.table = TRUE)


sum(cohort15$fdummy)
#cohort 20151

ggsurvplot(kmsurvival151a, data = cohort151, title = "2015 Foreclosure Survival Rates",palette = "#E7B800", xlab = "Months", 
           break.time.by =6, risk.table = TRUE)

ggsurvplot(kmsurvival151b, data = cohort151, title = "2015 Foreclosure Survival Rates",palette = c("#E7B800", "#2E9FDF"), xlab = "Months", 
           break.time.by =6, risk.table = TRUE)

# Combine on the same plot
fit151 <- list(Combined = kmsurvival151a, HAPvsNonHAP = kmsurvival151b)
ggsurvplot_combine(fit, cohort151, title = "2015 Foreclosure Survival Rates", palette = c("#E7B800","#2E9FDF","#D16103"), xlab = "Months", 
                   break.time.by =6, risk.table = TRUE)

#cohort 20161

ggsurvplot(kmsurvival161a, data = cohort161, title = "2016 Foreclosure Survival Rates",palette = "#E7B800", xlab = "Months", 
           break.time.by =6, risk.table = TRUE)

ggsurvplot(kmsurvival161b, data = cohort161, title = "2016 Foreclosure Survival Rates",palette = c("#E7B800", "#2E9FDF"), xlab = "Months", 
           break.time.by =6,conf.int = TRUE, risk.table = TRUE)

# Combine on the same plot
fit161 <- list(Combined = kmsurvival161a, HAPvsNonHAP = kmsurvival161b)
ggsurvplot_combine(fit, cohort161, title = "2016 Foreclosure Survival Rates", palette = c("#E7B800","#2E9FDF","#D16103"), xlab = "Months", 
                   break.time.by =6, risk.table = TRUE)
