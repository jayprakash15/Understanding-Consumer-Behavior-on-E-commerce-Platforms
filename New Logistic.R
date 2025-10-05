mdata=read.csv("Downloads/Inspiring Factors.csv")
mdata

names(mdata)
  
Amazon=mdata$Amazon
Amazon
A=mdata$Discount
A
B=mdata$Cashback
B
C=mdata$Safety.Security
C
D=mdata$Delivery_Speed
D
E=mdata$Return_policy
E


model=glm(formula=Amazon~A+B+C+D+E,family=binomial(link="logit"),data=mdata)
model
summary(model)

###
mdata=read.csv(file.choose(),header = TRUE)
mdata

head(mdata)

attach(mdata)

library(ltm)

cronbach.alpha(mdata,na.rm=TRUE)


####

###Goodness of fit
with(mdata,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail=F))
