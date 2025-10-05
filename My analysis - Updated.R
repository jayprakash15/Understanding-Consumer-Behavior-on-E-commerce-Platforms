data = read.csv("D:\\College\\BSc (S P college)\\Project\\Book1.csv")
data = data[-c(8,9)]
#View(data)
summary(data)
sum(is.na(data))
dim(data)

# overall rating of E-markets are Amazon 4; flipkart 4; myntra 3;
# snap 3; ajio 3; paytm 2; other 2 but there is very less data for
# last 3 as thoses apps have limited user

# s = subset(data,other==5)
# s
# summary(s)
# length(s$other)

str(data[1:4])
summary(data[1:4]) 
plot(data[c(1:4)])
 
# data[1] <- as.factor(data[1])
# str(data[1])
# dim(data)
# for(x in 1:9) {
#   data[x] <- as.factor(data[x])
# }

data$Amazon <- as.factor(data$Amazon)
data$flipkart <- as.factor(data$flipkart)
data$Myntr <- as.factor(data$Myntr)
data$snap <- as.factor(data$snap)
data$ajio <- as.factor(data$ajio)
data$paytm <- as.factor(data$paytm)
data$other <- as.factor(data$other)
data$How.confident.are.you.that.your.payment.information <- as.factor(data$How.confident.are.you.that.your.payment.information)
data$How.confident.are.you.that.your.personal.information <- as.factor(data$How.confident.are.you.that.your.personal.information)

str(data)
summary(data)
plot(data[1:3])

data$How.confident.are.you.that.your.payment.information <- as.numeric(data$How.confident.are.you.that.your.payment.information)
data$How.confident.are.you.that.your.personal.information <- as.numeric(data$How.confident.are.you.that.your.personal.information)
#data.numeric <- data.x[sapply(data.x,is.numeric)]

library(corrplot)
co_relation = cor(data[c(8,9)])
#heatmap(data[c(8,9)])
corrplot.mixed(co_relation)

A = data$How.confident.are.you.that.your.payment.information
B = data$How.confident.are.you.that.your.personal.information
class(A)

S=stack(list("A"=A,"B"=B))   ### One way to create data frame
class(S)
#View(S)
names(S)

ANOVA = oneway.test(values~ind, data=S, var.equal=TRUE)        
ANOVA

# if the p-value >α (usually 0.05), then our data are not considered to
# be “surprising enough” when H0 is true, and we say that our data do not
# provide enough evidence to reject H0(or, equivalently, that the data
# do not provide enough evidence to accept Ha)

####################

# Read in data
mdata <- read.csv("/home/jayprakash/Videos/College/Project/Book3.csv")


# Format categorical variables
mdata$Amazon <- factor(mdata$Amazon)
mdata$Flipkart <- factor(mdata$Flipkart)
mdata$Myntra <- factor(mdata$Myntra)

install.packages("nnet")
# Load the package
library(nnet)
# Run the model
model <- multinom(y ~ Amazon + Flipkart + Myntra, data=mdata)


##############

data <- read.csv("/home/jayprakash/Videos/College/Project/Project_data.csv") 
View(data)

data1 <- data[c(18,19,20,21,22)]
View(data1)

colnames(data1) <- c('Discount','Cashback','Security','Delivery_Speed','Return_Policy')
View(data1)
summary(data1)

Discount <- c(6,105,102,23,6,3,5)
Cashback  <- c(3,113,78,25,8,15,8)
Security  <- c(9,120,81,29,2,3,6)
Apps <- c('Ajio','Amazon','Filpkart','Myntra','Other','paytmMall','Snapdeal')   

Delivery_Speed <- c(5,124,88,24,4,5)
Return_Policy <- c(2,114,75,40,7,12)
App <- c('Ajio','Amazon','Filpkart','Myntra','Other','Snapdeal')   
#names(data1$Discount)

#pie(Discount1,labels = label1 )
#pie(Discount1,main="Pie Chart",col = 3:6,labels = "")
#legend(locator(1),legend = label1, fill = 3:6)

pie1 = data.frame(Apps,Discount,Cashback,Security) 
View(pie1)

pie2 = data.frame(App,Delivery_Speed,Return_Policy)
View(pie2)

write.csv(pie1,file="/home/jayprakash/Videos/College/Project/pie1.csv",row.names=FALSE)
write.csv(pie2,file="/home/jayprakash/Videos/College/Project/pie2.csv",row.names=FALSE)


###################

data = read.csv("/home/jayprakash/Videos/College/Project/Project_data.csv")
dim(data)    #To see dimension of data
#View(data)  # To indentfiy index of colume we want


data1 = data[c(24,25)]   #selecting data we want
summary(data1)
colnames(data1) = c("Security_Of_Payment_Info","Security_Of_Personal_Info")
#View(data1)  #changing colume names and Viewing data

#data1$Security_Of_Payment_Info = as.factor(data1$Security_Of_Payment_Info)
#data1$Security_Of_Personal_Info = as.factor(data1$Security_Of_Personal_Info)
#summary(data1)
#View(data1)

#data1$Security_Of_Payment_Info = as.numeric(data1$Security_Of_Payment_Info)
#data1$Security_Of_Personal_Info = as.numeric(data1$Security_Of_Personal_Info)
#is.numeric(data1[1])
#is.numeric(data1$Security_Of_Payment_Info)


shapiro.test(data1$Security_Of_Payment_Info)
# p-value = 0.00003343 < alpha = 0.05 
# we reject H0, data is non-normal
#e = 2.718 ; 1.479*(e^(-13))

shapiro.test(data1$Security_Of_Personal_Info)
# p-value = 0.0000280913 < alpha = 0.05
# we reject H0, data is non-normal
# Also we don't know the population variance and if variance of those
# two variable is equal. 

# Mann Whitney U Test (Wilcoxon Rank Sum Test)
# This module presents hypothesis testing techniques for situations 
# with small sample sizes and outcomes that are ordinal, ranked or 
# continuous and cannot be assumed to be normally distributed.
# Null Hypothesis: H 0 : Two populations are equal

#wilcox.test(new, placebo, alternative="less")
wilcox.test(data1$Security_Of_Payment_Info, data1$Security_Of_Personal_Info)
# P-value = 0.1002 > alpha(0.05), we accept H0.
# We have statistically significant evidence at α =0.05, to show 
# that the two populations of security of Payment information and 
# Security of Personal information are equal

# res <- wilcox.test(BULB_PRICE~ BULB_TYPE, data = DATASET, exact = FALSE)
# wilcox.test(Security_Of_Payment_Info ~ Security_Of_Personal_Info ,data= data1 ,exact= FALSE)
