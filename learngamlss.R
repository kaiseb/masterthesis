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
