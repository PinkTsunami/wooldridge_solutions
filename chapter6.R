if (!require("pacman")) install.packages("pacman")
pacman::p_load(wooldridge, stargazer, ggplot2, car, moments, dplyr)

# wooldridge package used for datasets
# stargazer package used to compare regression results
# ggplot2 package used optionally for graphing
# car package used for joint hypothesis testing of regrerssion coefficients
# momnents package used for checking skewness
# dplyr used for filtering of dataset; not required 

########## Chapter 6 ########## 

#### C1 ####

df <- kielmc
df.subset <- df %>% filter(year == 1981) # dplyr filter subset
df.subset <- df[df$year == 1981,] # base R subset

# i #

summary(fit1 <- lm(lprice ~ ldist, data = df.subset))

# ii #

summary(fit2 <- lm(lprice ~ ldist + lintst + larea + lland + rooms + baths + age, data = df.subset))

# iii #

summary(fit3 <- lm(lprice ~ ldist + lintst + larea + lland + rooms + baths + age + lintstsq, data = df.subset))

# iv #

summary(fit4 <- lm(lprice ~ ldist + lintst + larea + lland + rooms + baths + age + lintstsq + I(ldist^2), data = df.subset))

stargazer(fit1,fit2,fit3,fit4, type = 'text')

#### C2 ####

df <- wage1

# i #

summary(fit1 <- lm(lwage ~ educ + exper + expersq, data = df))

# ii #

summary(fit1)$coefficients['expersq',4] <= 0.01

# iii #

beta.hat.2 <- summary(fit1)$coefficients['exper',1]
beta.hat.3 <- summary(fit1)$coefficients['expersq',1]

100*(beta.hat.2 + 2*beta.hat.3*4)*(5-4)

100*(beta.hat.2 + 2*beta.hat.3*19)*(20-19)

# iv #

# first derivative w.r.t. exper and set equal to 0

exper.star <- beta.hat.2/(-2*beta.hat.3)

# e.g.
100*(beta.hat.2 + 2*beta.hat.3*29)*(29-28)
100*(beta.hat.2 + 2*beta.hat.3*28)*(28-27)

#### C3 ####

# i #

# first derivative w.r.t to educ equal to beta_1 + beta_3*exper

# ii #

# Null: beta_3 = 0

# iii #

df <- wage2

summary(fit1 <- lm(lwage ~ educ + exper + educ*exper, data = df))

# iv #

df$new_var <- df$educ*(df$exper-10)

summary(fit2 <- lm(lwage ~ educ + exper + new_var, data = df))

theta.hat.1 <- summary(fit2)$coefficients['educ',1]
theta.hat.1.sd <- summary(fit2)$coefficients['educ',2]

c(theta.hat.1-1.96*theta.hat.1.sd,theta.hat.1+1.96*theta.hat.1.sd)

#### C4 ####

# i #

# ii #

# iii #

# iv #






















