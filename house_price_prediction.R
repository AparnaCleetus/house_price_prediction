#loading packages
require(MASS)
require(ISLR)
require(corrplot)
library(tidyverse)
install.packages("Metrics")
library(Metrics)

#load data
house_data = read.csv("https://personal.utdallas.edu/~axc190011/Melbourne_housing_FULL.csv", stringsAsFactors = FALSE, quote = "")
names(house_data)
head(house_data)
dim(house_data)

#data preparation
#elimite records which donot have house price
which(is.na(house_data))
sum(is.na(house_data))
new_house_data = na.omit(house_data)
dim(new_house_data)
new_house_data$Rooms =  as.numeric(new_house_data$Rooms)
new_house_data$Price =  as.numeric(new_house_data$Price)
new_house_data$Distance = as.numeric(new_house_data$Distance)
new_house_data$Propertycount = as.numeric(new_house_data$Propertycount)
new_house_data$Bathroom = as.numeric(new_house_data$Bathroom)
new_house_data$Car = as.numeric(new_house_data$Car)
new_house_data$Landsize = as.numeric(new_house_data$Landsize)
new_house_data$Longtitude = as.numeric(new_house_data$Longtitude)
new_house_data$BuildingArea = as.numeric(new_house_data$BuildingArea)

#exploratory data analysis
summary(new_house_data)
ggplot(data = new_house_data, aes(x = Regionname, y = Price, color = Type)) + geom_point() + theme(text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1))
ggplot(data = new_house_data, aes(x = CouncilArea, y = Distance, color = Type)) + geom_point() + theme(text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1))
ggplot(data = new_house_data, aes(x=YearBuilt)) + geom_histogram(binwidth = 5) + xlim(1850,2020)
ggplot(data = new_house_data, aes(x = Regionname, y = Price)) + geom_boxplot() + theme(text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1))
ggplot(data = new_house_data, aes(x = CouncilArea)) + geom_bar() + theme(text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1))

#The target variable is heavily right skewed implying that the mean of the housing prices > median price. Mean of Melbourne housing price is $1,050,172 (Australian Dollars).
ggplot(data=new_house_data ,aes(x=Price))+geom_histogram(bins = 50,color = "white", fill = "Green")+scale_x_continuous(breaks = c(1000000,2000000,3000000,4000000),labels = c("$1M","$2M","$3M","$4M"))+ggtitle("House Price Distribution in Melbourne")+theme_bw()


#corelation between the numeric data
head(new_house_data)
house_data_numeric = new_house_data[c(3,5,9,11,12:16,18,19)] 
house_data_numeric <- na.omit(house_data_numeric)
head(house_data_numeric)
M = cor(house_data_numeric)
corrplot(M,method = "number")
corrplot(M,method = "pie")
#Bedrooms are highly correlated with rooms and bathrooms, so they are not considered for the furhter analysis. Adding bedrooms would give a biased result in most modelling.
house_data_numeric = as.data.frame(house_data_numeric[-c(3)])
head(house_data_numeric)

#split the training and test data
set.seed(100)
sample_size = ceiling(nrow(house_data_numeric) * 0.8)
train_index = sample(nrow(house_data_numeric), sample_size)

training_data = house_data_numeric[train_index, ]
test_data = house_data_numeric[-train_index, ]

#Train the model
model = lm(Price ~ Landsize + Bedroom2 +  Distance + Car + Bathroom + BuildingArea + YearBuilt + Lattitude + Longtitude , data = training_data)
summary(model)

#Validate the model
predicted_price = data.frame(predict(model, test_data))


#MSE of the model
head(predicted_price)
mse(test_data$Price, predicted_price$predict.model..test_data.)

plot(model)
