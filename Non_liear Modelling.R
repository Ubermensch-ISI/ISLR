require(ISLR)
data(Wage)
data=Wage
str(data)
attach(data)

#----------working with age
mod=lm(wage~poly(age,3,raw=T),data=data)
mod2=lm(wage~poly(age,3),data=data)
agelim=range(age)
pr=seq(agelim[1],agelim[2],length=length(age))
pred=predict(mod,data.frame("age"=pr),se=T)

interval=cbind(pred$fit+2*pred$se.fit,pred$fit-2*pred$se.fit)


plot(age,wage,xlim=agelim,cex=0.5)
title("degree 4 polynomial for wage vs age")
lines(pr,pred$fit,xlim=agelim,col="blue",lwd=1.5)
matlines(pr,interval)


mod3=lm(wage~poly(age,6),data=data)
coef(summary(mod3))#--------looking at the p-value it is clear that 3rd degree
#-polynomial is good and more degrees dont impove much


#-----------logistic for predicting wage >250
mod_log=glm(I(wage>250)~poly(age,4),data=data,family="binomial")
pred_log=predict(mod_log,data.frame("age"=pr),se=T,type="response")


interval_log=cbind(pred_log$fit+2*pred_log$se.fit,
pred_log$fit-2*pred_log$se.fit)


plot(age,I(wage>250),xlim=agelim,cex=0.5,ylim=c(-0.01,0.2))
title("degree 4 logistic for wage>250 vs age")
lines(pr,pred_log$fit,xlim=agelim,col="blue",lwd=1.5)
matlines(pr,interval_log)
#----it is not sensible as we would get negative probabilities

#-----hence we should calculate manually
pred_logit=predict(mod_log,newdata=data.frame("age"=pr),se=T)

p.log=exp(pred_logit$fit)/(1+exp(pred_logit$fit))
se.band=cbind(pred_logit$fit+2*pred_logit$se.fit,
pred_logit$fit-2*pred_logit$se.fit)
inter_logit=exp(se.band)/(1+exp(se.band))

plot(age,I(wage>250),xlim=agelim,cex=0.5,type="n",ylim=c(0,0.2),labels=T)
title("degree 4 logistic for wage>250 vs age")
lines(pr,p.log,xlim=agelim,col="blue",lwd=1.5)
matlines(pr,inter_logit)



#---------------step functions
mod_ste=lm(wage~cut(age,4),data=data)
summary(mod_ste)



pr=seq(agelim[1],agelim[2],length=length(age))
pred.s=predict(mod_ste,data.frame("age"=pr),se=T)

interval.s=cbind(pred.s$fit+2*pred.s$se.fit,pred.s$fit-2*pred.s$se.fit)


plot(age,wage,xlim=agelim,cex=0.5,main="degree 4 polynomial for wage vs age",
sub="nothing serious")
lines(pr,pred.s$fit,xlim=agelim,col="blue",lwd=2)
matlines(pr,interval.s,lwd=2)


#-------------------splines
#-----------basis spline
require(splines)

i=4

mod=lm(wage~bs(age,i),data=data)
mod2=lm(wage~poly(age,3),data=data)
agelim=range(age)
pr=seq(agelim[1],agelim[2],length=length(age))
pred=predict(mod,data.frame("age"=pr),se=T)
interval=cbind(pred$fit+2*pred$se.fit,pred$fit-2*pred$se.fit)

summary(mod)

plot(age,wage,xlim=agelim,cex=0.5)
title(paste(i,"th degree " ))
lines(pr,pred$fit,xlim=agelim,col="blue",lwd=1.5)
matlines(pr,interval)


#---------------natural spline
mod=lm(wage~ns(age,df=2),data=data)
mod2=lm(wage~ns(age,3),data=data)
mod3=lm(wage~ns(age,4),data=data)
agelim=range(age)
pr=seq(agelim[1],agelim[2],length=length(age))
pred=predict(mod,data.frame("age"=pr),se=T)
interval=cbind(pred$fit+2*pred$se.fit,pred$fit-2*pred$se.fit)

summary(mod)

plot(age,wage,xlim=agelim,cex=0.5)
title(paste(i,"th degree " ))
lines(pr,pred$fit,xlim=agelim,col="blue",lwd=1.5)
matlines(pr,interval)

anova(mod,mod2,mod3)#----------df 3 is good enough



#----------smooth spline
mod.s=smooth.spline(age,wage,cv=T)
mod.s2=smooth.spline(age,wage,lambda=1)
plot(age,wage,xlim=agelim,cex=0.5)
lines(mod.s,col="red",lwd=2)
lines(mod.s2,col="blue",lwd=2)


#----------local regression 
mod=loess(wage~age,span=0.2,data=data)
mod2=loess(wage~age,span=0.3,data=data)
mod3=loess(wage~age,span=0.4,data=data)
agelim=range(age)
pr=seq(agelim[1],agelim[2],length=length(age))
pred.1=predict(mod,data.frame("age"=pr),se=T)
pred.2=predict(mod2,data.frame("age"=pr),se=T)
pred.3=predict(mod3,data.frame("age"=pr),se=T)

interval=cbind(pred$fit+2*pred$se.fit,pred$fit-2*pred$se.fit)

summary(mod)

plot(age,wage,xlim=agelim,cex=0.5)
title("local regression")
lines(pr,pred.1$fit,xlim=agelim,col="blue",lwd=2)
lines(pr,pred.2$fit,xlim=agelim,col="red",lwd=2)
lines(pr,pred.3$fit,xlim=agelim,col="green",lwd=2)
matlines(pr,interval)


#-----------GAM
gam1=lm(wage~ns(year,4)+ns(age,5)+education,data=data)
summary(gam1)

plot.gam(gam1)
library(gam)

gam=gam(wage~s(age,5)+s(year,4)+education,data=data)
par(mfrow=c(1,3))
plot(gam,se=T,col="blue")












