if (!require("pacman")) install.packages("pacman")
pacman::p_load(wooldridge, stargazer, dplyr, ggplot2)

# This is not intended to be homework solutions to the exercises
# the intent of this script is to provide the code required to 
# answer some of the homework questions with the textbook.
# Because of this, you will not find solutions to problems
# that do not require a computer program/software to answer.

########## Chapter 3########## 

#### C1 ####

df <- bwght

# iii #

fit1 <- lm(bwght ~ cigs, data = df)
fit2 <- lm(bwght ~ cigs + faminc, data = df)

stargazer(fit1,fit2, type = 'text')
cor(df$cigs, df$faminc)

#### C2 ####

df <- hprice1

# i #
summary(fit <- lm(price ~ sqrft + bdrms, data = df))

# ii #
fit$coefficients['bdrms']

# iii #
fit$coefficients['bdrms']*1 + fit$coefficients['sqrft']*140

# iv #
fit.rsq <- summary(fit)$r.squared
paste('About', round(fit.rsq,3)*100, '%')

# v #
fit$coefficients['(Intercept)'] + fit$coefficients['bdrms']*4 + fit$coefficients['sqrft']*2438

# vi #
df$prediction <- fit$fitted.values
df$prediction[1] - df$price[1]

#### C3 ####

df <- ceosal2

# i #

summary(fit1 <- lm(lsalary ~ lsales + lmktval, data = df)) #constant elasticity, use logs of variables

# ii #

summary(fit2 <- lm(lsalary ~ lsales + lmktval + profits, data = df))
sum(df$profits<0) # nine observations are negative

# iii #

summary(fit3 <- lm(lsalary ~ lsales + lmktval + profits + ceoten, data = df))
paste('one more year as CEO increases salary by about', round(fit3$coefficients['ceoten']*100,1), '%')
# holding lsales, lmktval, and profits constant

# iv #

cor(df$lmktval, df$profits)

#### C4 ####

df <- attend

# i #

min(df$atndrte)
max(df$atndrte)
mean(df$atndrte)

min(df$priGPA)
max(df$priGPA)
mean(df$priGPA)

min(df$ACT)
max(df$ACT)
mean(df$ACT)

#or

df.1 <- df[,c('atndrte','priGPA','ACT')]

data.frame(mean = apply(df.1,2,mean),
           min = apply(df.1,2,min),
           max = apply(df.1,2,max))


# ii #

summary(fit <- lm(atndrte ~ priGPA + ACT, data = df))

# iv #

fit$coefficients[1] + fit$coefficients['priGPA']*3.65 + fit$coefficients['ACT']*20

df[(df$priGPA == 3.65 | df$ACT==20),]
df[(df$priGPA == 3.65 & df$ACT==20),]

#or 

df %>% filter(priGPA == 3.65 | ACT == 20)
df %>% filter(priGPA == 3.65 , ACT == 20)

# v #

student.A <- fit$coefficients[1] + fit$coefficients['priGPA']*3.1 + fit$coefficients['ACT']*21
student.B <- fit$coefficients[1] + fit$coefficients['priGPA']*2.1 + fit$coefficients['ACT']*26
student.A - student.B

#or

fit$coefficients['priGPA']*(3.1-2.1) + fit$coefficients['ACT']*(21-26)

#### C5 ####

df <- wage1

summary(fit1 <- lm(educ ~ exper + tenure, data = df))
df$resid1 <- fit1$residuals

summary(fit2 <- lm(lwage ~ resid1, data = df))

summary(fit3 <- lm(lwage ~ educ + exper + tenure, data = df))

fit2$coefficients['resid1']
fit3$coefficients['educ']

#### C6 ####

df <- wage2

# i #

summary(fit1 <- lm(IQ ~ educ, data = df))
gamma.1 <- as.numeric(fit1$coefficients['educ'])

# ii #

summary(fit2 <- lm(lwage ~ educ, data = df))
beta.tilde.1 <- as.numeric(fit2$coefficients['educ'])

# iii #

summary(fit3 <- lm(lwage ~ educ + IQ, data = df))
beta.hat.1 <- as.numeric(fit3$coefficients['educ'])
beta.hat.2 <- as.numeric(fit3$coefficients['IQ'])

# iv #

all.equal(beta.tilde.1, beta.hat.1 + beta.hat.2*gamma.1)

#### C7 ####

df <- meap93

# i #

summary(fit1 <- lm(math10 ~ lexpend + lnchprg, data = df))

# iii #

summary(fit2 <- lm(math10 ~ lexpend, data = df))

stargazer(fit1, fit2, type = "text")

# iv #

cor(df$lexpend, df$lnchprg)

#### C 8 ####

df <- discrim

# i #

mean(df$prpblck, na.rm = T)
sd(df$prpblck, na.rm = T)

mean(df$income, na.rm = T)
sd(df$income, na.rm = T)

# or

df.1 <- df[,c('prpblck','income')]

options(scipen = 999)
data.frame(mean = apply(df.1, 2, mean, na.rm = T),
           sd = apply(df.1, 2, sd, na.rm = T))
options(scipen = 0)

# ii #

options(scipen = 999)
summary(fit1 <- lm(psoda ~ prpblck + income, data = df))

options(scipen = 0)

# iii #

summary(fit2 <- lm(psoda ~ prpblck, data = df))
stargazer(fit1, fit2, type = 'text')

# iv #

summary(fit3 <- lm(lpsoda ~ prpblck + lincome, data = df))
fit3$coefficients['prpblck']*20

# v #

summary(fit4 <- lm(lpsoda ~ prpblck + lincome + prppov, data = df))
stargazer(fit3, fit4, type = 'text')

# vi #

cor(df$lincome, df$prppov)

#### C9 ####

df <- charity

# i #

summary(fit1 <- lm(gift ~ mailsyear + giftlast + propresp, data = df))
summary(fit2 <- lm(gift ~ mailsyear, data = df))
stargazer(fit1, fit2, type = 'text')

# ii #

stargazer(fit1, fit2, type = 'text')

# iii #

head(df$propresp,10)

# iv #

summary(fit3 <- lm(gift ~ mailsyear + giftlast + propresp + avggift, data = df))
stargazer(fit1, fit3, type = 'text', keep = 'mailsyear')

# v #

stargazer(fit1, fit3, type = 'text', keep = 'gift')

#### C10 ####

df <- htv

# i #

max(df$educ) - min(df$educ)

sum(df$educ==12)/nrow(df)

apply(df[,c('educ','motheduc','fatheduc')], 2, mean)

# ii #

summary(fit1 <- lm(educ ~ motheduc + fatheduc, data = df))

# iii #

summary(fit2 <- lm(educ ~ motheduc + fatheduc + abil, data = df))
stargazer(fit1, fit2, type = 'text')

# iv #

df$abilsq <- df$abil^2

summary(fit3 <- lm(educ ~ motheduc + fatheduc + abil + abilsq, data = df))

abil.star = fit3$coefficients['abil']*(-1)/(2*fit3$coefficients['abilsq'])

# v #

sum(df$abil<abil.star)/nrow(df)

# vi #

df.new <- data.frame(abil = df$abil, abilsq = df$abilsq, motheduc = 12.18, fatheduc = 12.45)

df.new$predicted.educ <- predict(fit3, newdata = df.new)

plot(df.new$abil, df.new$predicted.educ, xlab = 'Ability', ylab = 'Predicted Education')

#### C11 ####

df <- meapsingle

# i #

summary(fit1 <- lm(math4 ~ pctsgle, data = df))

# ii #

summary(fit2 <- lm(math4 ~ pctsgle + lmedinc + free, data = df))
stargazer(fit1, fit2, type = 'text')

# iii #

cor(df$lmedinc, df$free)

# v #

1/(1-summary(lm(pctsgle ~ lmedinc + free, data = df))$r.squared)
1/(1-summary(lm(lmedinc ~ pctsgle + free, data = df))$r.squared)
1/(1-summary(lm(free ~ lmedinc + pctsgle, data = df))$r.squared)

install.packages('car')
library(car)
vif(fit2)

#### C12 ####

df <- econmath

# i #

sum(df$score==100)
mean(df$score)

data.frame(mean = apply(df[,c('actmth','acteng')], 2, mean, na.rm = T),
           sd = apply(df[,c('actmth','acteng')], 2, sd, na.rm = T))

# ii #

summary(fit1 <- lm(score ~ colgpa + actmth + acteng, data = df))
