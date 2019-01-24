if (!require("pacman")) install.packages("pacman")
pacman::p_load(wooldridge, stargazer, ggplot2, car, moments)

# wooldridge package used for datasets
# stargazer package used to compare regression results
# ggplot2 package used optionally for graphing
# car package used for joint hypothesis testing of regrerssion coefficients
# momnents package used for checking skewness

########## Chapter 5 ########## 

#### C1 ####

df <- wage1

# i #

summary(fit1 <- lm(wage ~ educ + exper + expersq, data = df))

df$fit1.resid <- fit1$residuals

hist(df$fit1.resid)

# or 

ggplot(df, aes(fit1.resid)) +
  geom_histogram(aes(y = ..density..), alpha = .5) + 
  geom_line(aes(y = ..density.., colour = 'red'), stat = 'density', show.legend = F)
  
# ii #

summary(fit2 <- lm(lwage ~ educ + exper + expersq, data = df))

df$fit2.resid <- fit2$residuals

hist(df$fit2.resid)

# or 

ggplot(df, aes(fit2.resid)) +
  geom_histogram(aes(y = ..density..), alpha = .5) + 
  geom_line(aes(y = ..density.., colour = 'red'), stat = 'density', show.legend = F)

#### C2 ####

df <- gpa2

# i #

summary(fit1 <- lm(colgpa ~ hsperc + sat, data = df))

# ii #

summary(fit2 <- lm(colgpa ~ hsperc + sat, data = df[1:2070,]))

# iii #

summary(fit2)$coef['hsperc','Std. Error']/summary(fit1)$coef['hsperc','Std. Error']
sqrt(nrow(df)/2070)

#### C3 ####

df <- bwght

df.test <- df[complete.cases(df[,c('bwght','cigs','parity','faminc','motheduc','fatheduc')]),]

summary(fit.restricted <- lm(bwght ~ cigs + parity + faminc, data = df.test))

df.test$resid.restricted <- fit.restricted$residuals 

R.squared.u <- summary(fit1 <- lm(resid.restricted ~ cigs + parity + faminc + motheduc + fatheduc, data = df.test))$r.squared

LM.stat = nrow(df.test)*R.squared.u

pchisq(LM.stat, 2, lower.tail = F)

# F stat comparison

summary(fit.unrestricted <- lm(bwght ~ cigs + parity + faminc + motheduc + fatheduc, data = df.test))
lht(fit.unrestricted, c('motheduc = 0','fatheduc = 0'))

#### C4 ####

df <- k401ksubs

# i #

df.fsize <- df[df$fsize==1,]

# using formula in exercise
mu.hat.y <- mean(df.fsize$inc)
sigma.hat.y <- sd(df.fsize$inc)
z.i <- (df.fsize$inc - mu.hat.y)/sigma.hat.y

sum(z.i^3)/(nrow(df.fsize))

# skewness from moments package
skewness(df.fsize$inc)

df.fsize$linc <- log(df.fsize$inc)
skewness(df.fsize$linc)

# ii #

df <- bwght2

skewness(df$bwght)
skewness(df$lbwght)

#### C5 ####

df <- htv

# i #

nlevels(as.factor(df$educ))

# ii #

ggplot(df, aes(educ)) +
  geom_histogram(aes(y = stat(density))) +
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(df$educ), sd = sd(df$educ)), 
    lwd = 2, 
    col = 'red'
  )

#### C6 ####

df <- econmath

# i #

min(econmath$score)
max(econmath$score)

# iii #

summary(fit1 <- lm(score ~ colgpa + actmth + acteng, data = df))
