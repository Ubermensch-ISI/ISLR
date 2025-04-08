library(ISLR)
data(Smarket)
data=Smarket
attach(data)
str(data)

#----------Logistic regression
glm.fit=glm(Direction~.-Year-Today,family="binomial",data=data)
summary(glm.fit)
pred=predict(glm.fit,type="response")
result=rep("Down",length(Direction))
result[pred>0.5]="Up"
table(result,Direction)
mean(result!=Direction)#--------error in prediction 

#-----now we will use 2005 as test data and rest years as training data

data1=data[Year<2005,]
data.2005=data[Year==2005,]
str(data1)
glm.fit1=glm(Direction~.-Year-Today,family="binomial",data=data1)
pred1=predict(glm.fit,data.2005,type="response")

result1=rep("Down",length(data.2005$Direction))
result1[pred1>0.5]="Up"
table(result1,data.2005$Direction)
mean(result1!=data.2005$Direction) #------------error in preidction is high


#----drawing curves for two
levels(Direction)=c(1,0)
d=as.numeric(levels(Direction))[Direction]
plot(d~Lag1)
dr.g=glm(d~data$Lag1,family=binomial)
pre.d=predict(dr.g,type="response")
resu=rep(0,length(Lag1))
resu[pre.d>0.5]=1
lines(pre.d~Lag1)



#--------------Linear Discriminant Analysis
require(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=data1)
lda.fit

plot(lda.fit)