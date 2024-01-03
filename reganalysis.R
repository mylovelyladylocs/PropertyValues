#simple linear regression with housing data

#import data
house<-read.csv(file.choose(),header=TRUE)
#Remove region 1
n<-which(house$Region ==1)
house<-house[-n,]

model<-lm(Sales.Price ~  sqft, data = house)
summary(model)
par(mfrow = c(2, 2))
plot(model)
#Remove 143, 367, and 127
house1<-house[-c(143,367,127),]
model.o<-lm(Sales.Price ~  sqft, data = house1)
summary(model.o)
par(mfrow = c(2, 2))
plot(model.o)

#Region 2
a<-which(house1$Region == 2)
house.region.2<-house1[a,]
model.region2<-lm(Sales.Price ~  sqft, data = house.region.2)
summary(model.region2)

#Region 3
b<-which(house1$Region == 3)
house.region.3<-house1[b,]
model.region3<-lm(Sales.Price ~  sqft, data = house.region.3)
summary(model.region3)

#Region 4
c<-which(house1$Region == 4)
house.region.4<-house1[c,]
model.region4<-lm(Sales.Price ~  sqft, data = house.region.4)
summary(model.region4)

#Region 5
d<-which(house1$Region == 5)
house.region.5<-house1[d,]
model.region5<-lm(Sales.Price ~  sqft, data = house.region.5)
summary(model.region5)

model1<-lm(Sales.Price ~  bath, data = house)
summary(model1)

model2<-lm(Sales.Price ~  bed, data = house)
summary(model2)

model3<-lm(Sales.Price ~ as.factor(Region), data = house)
summary(model3)

model4<-lm(Sales.Price ~ bed + bath + sqft, data = house)
summary(model4)

model5<-lm(Sales.Price ~ as.factor(Region) + bed + bath + sqft, data = house)
summary(model5)


#linearity

plot(house$sqft,house$Sales.Price)

# This finds the correlation coefficient between the TV and Sales columns of the data frame named advertising.
sqcor <- cor.test(house$Sales.Price, house$sqft)
bedcor <- cor.test(house$Sales.Price, house$bed)
bathcor <- cor.test(house$Sales.Price, house$bath)

coefficient$estimate

#outliers

