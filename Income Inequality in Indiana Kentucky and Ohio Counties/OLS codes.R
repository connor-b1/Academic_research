install.packages("GGally")
install.packages("clubSandwich")
library("clubSandwich")
library("dplyr")
library("lmtest")
library("olsrr")
library("car")
library("stargazer")
data <- data %>% arrange(CNTY_NUM)                   

date1 = seq(1:900)
plot(data$GINI, date1)

data2 <- final_data_set_vert2_0 %>% arrange(CNTY_NUM)

PERCAPIN

attach(data2)
class(data$D1990)
logpop = log(POP)
newdata <- factor(STATE)
loginc = log(PERCAPIN)
final1 <- data2 %>% mutate(logpop = (log(POP)), loginc = (log(PERCAPIN)), unemp = (UNEMP/100))
final2 <- final1[ ,-18]

#interaction terms

??datavar
educit <- EDU - mean(EDU)
poverit <- POVERTY - mean(POVERTY)
interaction <- educit*poverit

#first regressions without diff()
attach(final1)
ols1 <- lm(GINI~logpop+EDU+POVERTY+loginc+unemp+factor(YEAR)+factor(STATE)+EDU:POVERTY)
ols3 <- lm(GINI~logpop+EDU+POVERTY+factor(YEAR)+factor(STATE))
summary(ols1)
summary(ols1, cluster=c("YEAR"))
par(mfrow=c(3,3)) 
plot(ols1)

stargazer(ols1, ols5, type = "text", title="Descriptive statistics", digits=1, out="ols2.txt")
AIC(ols1, ols5)
BIC(ols1, ols5)
#testing Farrar ??? Glauber test (F ??? test) poverty edu and unemp ?????? 
#non differenced: logpop loginc and factors 
getwd()
AIC(ols1)
BIC(ols1)
vif(ols1)
plot(ols1)
serialcorr(ols1)
vif(ols1)
#differencing

date = seq(1:900)
par(mar = rep(2,4))

plot(date, final2$unemp, type='l',col=4)
plot(date, final2$GINI, type='l',col=4)
plot(date, final2$, type='l',col=4)

attach(final2)
dpvty <- diff(POVERTY)
dedu <- diff(EDU)
dunemp <- diff(unemp)

date1 = seq(1:899)
plot(date, GINI, type='l',col=4)

dgini <- diff(GINI)
dfctyr <- factor(YEAR)
dfctst <- factor(STATE)
dfctyr1 <- dfctyr[2:900]
dfctst1 <- dfctst[2:900]
logpop899 <- logpop[2:900]
loginc899 <- loginc[2:900]

#interaction terms


educIT <- dedu - mean(dedu)
poverIT <- dlogpvty - mean(dlogpvty)

ols4 <- lm(dgini~logpop899+dedu+dpvty+dunemp+loginc899+dfctyr1+dfctst1)
ols5 <- lm(dgini~logpop899+dedu+dpvty+dunemp+loginc899+dfctst1+dedu:dpvty)
anova(ols4, ols5)
serialcorr(ols5)
summary(ols4)
AIC(ols4)
BIC(ols4)
vif(ols4)
plot(ols4)
bgtest(ols4)
serialcorr(ols4)