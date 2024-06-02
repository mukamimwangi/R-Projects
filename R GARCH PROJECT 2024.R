#FINANCIAL TIME SERIES
# MODELS ARE Garch, Egarch, GJR Garch
# transfer data from excel to R
ACT=read.csv("ACTUARIAL.CSV");ACT
# choosing data for analysis
myd=ACT$INFLATION;myd
#install fGarch package
library(fGarch)
log.INT=diff(myd);log.INT
ts.plot(log.INT)
summary(log.INT)
#GARCH
#garch(1,1)
F1=garchFit(formula=~garch(1,1),data=log.INT,cond.dist="norm",include.mean = FALSE,trace = FALSE);F1
summary(F1)
plot(F1)
F2=garchFit(formula=~garch(1,1),data=log.INT,cond.dist="std",include.mean = FALSE,trace = FALSE);F2
summary(F2)
plot(F2)
F3=garchFit(formula=~garch(1,1),data=log.INT,cond.dist="ged",include.mean = FALSE,trace = FALSE);F3
summary(F3)
plot(F3)
par(mfrow=c(2,2))

predict(F1,n.head=60,plot=TRUE)

predict(F2,n.head=60,plot=TRUE)

predict(F3,n.head=60,plot=TRUE)

# garch(1,2)
F4=garchFit(formula=~garch(1,2),data=log.INT,cond.dist="norm",include.mean = FALSE,trace = FALSE);F4
summary(F4)
plot(F4)
F5=garchFit(formula=~garch(1,2),data=log.INT,cond.dist="std",include.mean = FALSE,trace = FALSE);F5
summary(F5)
plot(F5)
F6=garchFit(formula=~garch(1,2),data=log.INT,cond.dist="ged",include.mean = FALSE,trace = FALSE);F6
summary(F6)
plot(F6)
par(mfrow=c(2,2))

predict(F4,n.head=60,plot=TRUE)

predict(F5,n.head=60,plot=TRUE)

predict(F6,n.head=60,plot=TRUE)

#Garch (2,1)
F7=garchFit(formula=~garch(2,1),data=log.INT,cond.dist="norm",include.mean = FALSE,trace = FALSE);F4
summary(F7)
plot(F7)
F8=garchFit(formula=~garch(2,1),data=log.INT,cond.dist="std",include.mean = FALSE,trace = FALSE);F5
summary(F8)
plot(F8)
F9=garchFit(formula=~garch(2,1),data=log.INT,cond.dist="ged",include.mean = FALSE,trace = FALSE);F6
summary(F9)
plot(F9)
par(mfrow=c(2,2))

predict(F7,n.head=60,plot=TRUE)

predict(F8,n.head=60,plot=TRUE)

predict(F9,n.head=60,plot=TRUE)

#Garch (2,2)
F10=garchFit(formula=~garch(2,1),data=log.INT,cond.dist="norm",include.mean = FALSE,trace = FALSE);F4
summary(F10)
plot(F10)
F11=garchFit(formula=~garch(2,1),data=log.INT,cond.dist="std",include.mean = FALSE,trace = FALSE);F5
summary(F11)
plot(F11)
F12=garchFit(formula=~garch(2,1),data=log.INT,cond.dist="ged",include.mean = FALSE,trace = FALSE);F6
summary(F12)
plot(F12)
par(mfrow=c(2,2))

predict(F10,n.head=60,plot=TRUE)

predict(F11,n.head=60,plot=TRUE)

predict(F12,n.head=60,plot=TRUE)

library(fBasics)




