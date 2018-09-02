##### Learning about the gamlss - package #####

#### Chapter 1 ####

# Linear model 

# Loading library
library(gamlss)
data(rent)

head(rent)

PPP <- par(mfrow=c(2,2))
plot(R~Fl, data=rent, col=gray(0.7), pch=15, cex=0.5) 
plot(R~A, data=rent, col=gray(0.7), pch=15, cex=0.5) 
plot(R~H, data=rent, col=gray(0.7), pch=15, cex=0.5) 
plot(R~loc, data=rent, col=gray(0.7), pch=15, cex=0.5) 
par(PPP)


r1 <- gamlss(R ~ Fl+A+H+loc, family=NO, data=rent, trace=FALSE) 
l1 <- lm(R ~ Fl+A+H+loc,data=rent)
coef(r1)
coef(l1)

# Note that the gamlss object residuals are the normalized (randomized) quantile residuals as # explained in Section 12.2, and not the simple residuals ˆ that might be expected.

fitted(r1, "sigma")[1]
summary(r1)
Rsq(r1)
plot(r1)


# Generalized linear model

r2 <- gamlss(R ~ Fl+A+H+loc, family=GA, data=rent)

coef(r2)
coef(r2, "sigma")
deviance(r2)

l2 <- glm(R ~ Fl+A+H+loc, family=Gamma(link="log"), data=rent) 
coef(l2)
deviance(l2)


# Generalized additive model 

r3 <- gamlss(R ~ pb(Fl)+pb(A)+H+loc, family=GA, data=rent, trace = FALSE) 
summary(r3)       

AIC(r2,r3)

term.plot(r3, pages=1, ask=FALSE)
wp(r3, ylim.all=.6)

# Modeling sigma

r4 <- gamlss(R ~ pb(Fl)+pb(A)+H+loc, 
             sigma.fo=~pb(Fl)+pb(A)+H+loc, family=GA, data=rent, trace=FALSE)
AIC(r3,r4)
term.plot(r4, pages=1, what="sigma", ask=FALSE)
drop1(r4, what="sigma")
wp(r4, ylim.all=.6)


# Generalized additive model for location, scale and shape

r5 <- gamlss(R ~ pb(Fl)+pb(A)+H+loc, 
             sigma.fo=~pb(Fl)+pb(A)+H+loc, 
             nu.fo=~pb(Fl)+pb(A)+H+loc, 
             family=BCCGo, data=rent, trace=FALSE)
AIC(r4, r5)
