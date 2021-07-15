library(DataExplorer)
library(pastecs)
library(readxl)
library(ggplot2)
library(fBasics)
library(moments)
library(caTools)
incn<-read_xlsx("C:/Users/User/Desktop/Outlook/Incinerator.xlsx",sheet=1)
str(incn)
dim(incn)
summary(incn)
create_report(incn)

stat.desc(incn)
#correlation b/w price and area
cor.test(incn$price,incn$area)
# for 95% cases cor will be 57-70%
#in this case we reject null hypothesis as p value is less than 0.05
# Visualization using ggplot2
ggplot(incn,aes(price,area))+geom_point()
ggplot(incn,aes(price,area))+geom_smooth()

cor.test(incn$price,incn$rooms)

# Visualization using ggplot2
ggplot(incn,aes(price,rooms))+geom_point()
ggplot(incn,aes(price,rooms))+geom_smooth()

# Testing whether average age of house is 18 or not
t.test(incn$age,mu=18.0)
t.test(incn$age,mu=18.0,alternative =c("less"))
t.test(incn$age,mu=18.0,alternative =c("greater"))
#mean of x  18.00935  and p-value = 0.498 
# as p value is greater than 0.05 we reject our null hypothesis
 
ggplot(data = incn, aes(incn$price, incn$area))+geom_point()
ggplot(data = incn, aes(incn$price, incn$area))+geom_smooth()

qqnorm(incn$price)
qqline(incn$price)
qqnorm(incn$lrprice)
qqline(incn$lrprice)
#this data is skewed and not normally distributed
boxplot(incn$price)
shapiro.test(incn$price)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
# box plot for Price
boxplot(incn$price, main="Price", sub=paste("Outlier rows: ", boxplot.stats(incn$price)$out))  
# box plot for Area
boxplot(incn$area, main="Area", sub=paste("Outlier rows: ", boxplot.stats(incn$area)$out))  
# scatterplot
scatter.smooth(x=incn$price, y=incn$area, main="Price ~ Area") 
# scatterplot 
scatter.smooth(x=incn$lrprice, y=incn$area, main="lrprice ~ Area") 
 # build linear regression model on full data

regmod<-lm(price~area,data=incn)
print(regmod)
summary(regmod)
regmod2<-lm(incn$lrprice~area,data=incn)
print(regmod2)
summary(regmod2)

AIC(regmod)  
#=> 7595.852
BIC(regmod)  
#=> 7607.167
#training and testing dataset

# divide data on the basis of sl no into two parts
set.seed(100)
?set.seed()
#randomly distributed data where 70 percent is truse rem is false
split1<-sample.split(incn$price, SplitRatio = 0.70)
split1
#training dataset
data2train<-subset(incn, split1 == TRUE)
data2test<-subset(incn,split1==FALSE)
dim(data2train)   #70%
dim(data2test)    #30%

# Build the model on training data
# build the model
lmmod<-lm(price~area,data=data2train)
summary(lmmod)
# predict distance
pricepred<-predict(lmmod,data2test)
summary(pricepred)
# make actuals_predicteds dataframe.
actuals_preds <- data.frame(cbind(actuals=data2test$price, predicteds=pricepred)) 
cor(actuals_preds)
correlation_accuracy <- cor(actuals_preds) 
summary(correlation_accuracy)
# 62.1% accuracy
head(actuals_preds)

