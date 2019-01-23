if (!require("pacman")) install.packages("pacman")
pacman::p_load(wooldridge, stargazer, ggplot2, car)

# wooldridge package used for datasets
# stargazer package used to compare regression results
# ggplot2 package used optionally for graphing
# car package used for joint hypothesis testing of regrerssion coefficients

########## Chapter 4 ########## 

#### C1 ####

df <- vote1

# iii #

summary(fit1 <- lm(voteA ~ lexpendA + lexpendB + prtystrA, data = df))

# iv #

df$lexpendB.minus.A <- df$lexpendB - df$lexpendA
summary(fit1b <- lm(voteA ~ lexpendA + lexpendB.minus.A + prtystrA, data = df))

summary(fit1b)$coef['lexpendA','t value']

#### C2 ####

df <- lawsch85

# i #

summary(fit1 <- lm(lsalary ~ LSAT + GPA + llibvol + lcost + rank, data = df))

# ii #

linearHypothesis(fit1, c('LSAT = 0', 'GPA = 0'))

# iii #

summary(fit2 <- lm(lsalary ~ LSAT + GPA + llibvol + lcost + rank + clsize + faculty, data = df))
linearHypothesis(fit2, c('clsize = 0', 'faculty = 0'))

#### C3 ####

df <- hprice1

# i #

summary(fit1 <- lm(lprice ~ sqrft + bdrms, data = df))
delta.hat.1 <- 150*fit1$coefficients['sqrft'] + fit1$coefficients['bdrms'] 

# ii #

df$new.var <- df$sqrft - 150*df$bdrms
summary(fit2 <- lm(lprice ~ new.var + bdrms, data = df))

# iii #

confint(fit2, 'bdrms', level = .95)

#### C4 ####

df <- bwght

summary(fit <- lm(bwght ~ cigs + parity + faminc, data = df))
summary(fit.restricted <- lm(bwght ~ cigs + parity + faminc + motheduc + fatheduc, data = df))

stargazer(fit, fit.restricted, type = 'text')

#### C5 ####

df <- mlb1

# i #

summary(fit1 <- lm(lsalary ~ years + gamesyr + bavg + hrunsyr + rbisyr, data = df))
summary(fit2 <- lm(lsalary ~ years + gamesyr + bavg + hrunsyr, data = df))

stargazer(fit1, fit2, type = 'text')

# ii #

summary(fit3 <- lm(lsalary ~ years + gamesyr + bavg + hrunsyr + runsyr + fldperc + sbasesyr, data = df))

# iii #

linearHypothesis(fit3, c('bavg = 0','fldperc = 0','sbasesyr = 0'))

#### C6 ####

df <- wage2

# i #

summary(fit1 <- lm(lwage ~ educ + exper + tenure, data = df))
linearHypothesis(fit1, 'exper = tenure')

# ii #

df$exper.plus.tenure <- df$exper + df$tenure
summary(fit2 <- lm(lwage ~ educ + exper + exper.plus.tenure, data = df))

confint(fit2, 'exper', level = .95)

#### C7 ####

df <- twoyear

# i #

min(df$phsrank)
max(df$phsrank)
mean(df$phsrank)

# ii #

summary(fit1 <- lm(lwage ~ jc + totcoll + exper + phsrank, data = df))

# iii #

summary(fit.4.26 <- lm(lwage ~ jc + totcoll + exper, data = df))

# iv #

summary(fit3 <- lm(lwage ~ jc + totcoll + exper + id, data = df))

#### C8 ####

df <- k401ksubs

# i #

sum(df$fsize==1)

# ii #

summary(fit1 <- lm(nettfa ~ inc + age, data = df, subset = fsize == 1))

# iv #

t.stat <- (fit1$coefficients['age'] - 1)/summary(fit1)$coef['age','Std. Error']
pnorm(-abs(test))

# v #

summary(fit2 <- lm(nettfa ~ inc, data = df, subset = fsize == 1))
stargazer(fit1, fit2, type = 'text')

#### C9 ####

df <- discrim

# i #

summary(fit1 <- lm(lpsoda ~ prpblck + lincome + prppov, data = df))

# ii #

df.temp <- df[complete.cases(df[,c('lincome','prppov')]),] #keep only rows that have non-null values for both lincome and prppov
cor(df.temp$lincome, df.temp$prppov,)

summary(fit1)$coef[c('lincome','prppov'),'Pr(>|t|)']

# iii #

summary(fit2 <- lm(lpsoda ~ prpblck + lincome + prppov + lhseval, data = df))
summary(fit2)$coef['lhseval','Pr(>|t|)']

# iv #

stargazer(fit1, fit2, type = 'text')

linearHypothesis(fit2, c('lincome = 0','prppov = 0'))

#### C10 ####

df <- elem94_95

# still in progress of making...


