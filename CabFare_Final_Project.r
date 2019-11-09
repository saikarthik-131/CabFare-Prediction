rm (list = ls()) # Cleaing the evironment
#setting working directory
setwd('D:\CabFarePrediction\InputFiles') 
getwd()

#Loading Libraries in R
#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#Reading the training and test data
cabfare_train = read.csv('train_cab.csv')
cabfare_test = read.csv('test.csv')

#Get the names of the Variables
names('cabfare_train')


#knowing structure of our data
str(cabfare_train) 
#We have 16067 obs. of  7 variables

#Changing the data types of fareamount and passengercount variables
cabfare_train$fare_amount = as.numeric(as.character(cabfare_train$fare_amount))
cabfare_train$passenger_count = as.integer(as.character(cabfare_train$passenger_count))


summary(cabfare_train)
############################DATA PRE-POCESSING########################
######################################################################
 #
  # 3.1 Missing Value Analysis
  # ============================== #
missing_values = function(data_frame){sapply(data_frame, function(X) sum(is.na(X)))}
missing_values(cabfare_train)

# ============================== #
# 3.2 Converting types of data
# ============================== #
cabfare_train$fare_amount = as.numeric(levels(cabfare_train$fare_amount))[cabfare_train$fare_amount]
cabfare_train$passenger_count = as.integer(cabfare_train$passenger_count)
cabfare_train$pickup_datetime = as.POSIXlt(strptime(cabfare_train$pickup_datetime, "%Y-%m-%d %H:%M:%S"))

missing_values(cabfare_train)
str(cabfare_train)
sapply(cabfare_train, class)
summary(cabfare_train)

# ============================== #
# 3.2 Removing 0s & NAs rows
# ============================== #
remove_na_rows = function(data_frame){
  na_rows = apply(data_frame, 1, function(df_row) any(is.na(df_row)))
  data_frame = data_frame[!na_rows, ]
  return(data_frame)
}
clean_training_data = remove_na_rows(cabfare_train)

remove_0s_rows = function(data_frame){
  rows_0s = apply(data_frame, 1, function(df_row) any(df_row == 0))
  data_frame = data_frame[!rows_0s, ]
  return(data_frame)
}
training_data = remove_0s_rows(clean_training_data)

# ============================== #
# 3.3 Outliers
# ============================== #
# boxplot function to plot boxplot of columns
boxplot(training_data$fare_amount, data = training_data)

library('ggplot2')
#Box Plot using ggplot2 library
box_plot = function(column, data_frame){
  ggplot(data_frame, aes_string(y=column))+
    geom_boxplot(outlier.color = 'seagreen', outlier.shape = 8, outlier.size = 2)
}
box_plot(training_data$pickup_longitude, training_data)

#Box Plot using ggplot2 library
hist_plot = function(column, data_frame, binsize){
  ggplot(data = data_frame, aes_string(column)) + geom_histogram(binwidth = binsize)
}
hist_plot('passenger_count', training_data, 1000)

find_outlier_range = function(column, data_frame){
  #column - string
  q1 = quantile(data_frame[ ,column], 0.25)
  q3 = quantile(data_frame[ ,column], 0.75)
  iqr = q3 - q1
  lower_range = q1 - 1.5*iqr
  upper_range = q3 + 1.5*iqr
  print(lower_range)
  print(upper_range)
  return(c(lower_range, upper_range))
}
manual_outlier_range = data.frame(
  fare_amount = c(1, 100),
  pickup_longitude = c(-74.8, -72.8),
  pickup_latitude = c(39.45, 41.45),
  dropoff_longitude = c(-74.8, -72.8),
  dropoff_latitude = c(39.45, 41.45),
  passenger_count = c(1, 6)
)

iqr_outlier_range = data.frame(
  fare_amount = c(0, 22.25),
  passenger_count = c(0, 3.5),
  onroad_distance = c(0, 10380),
  onroad_time = c(0, 1961),
  amnt_per_km = c(0.05, 5.8),
  amnt_per_hr = c(8.15, 87)
)

is_in_range = function(num, column){
  #column - string
  lr = manual_outlier_range[1,column]
  ur = manual_outlier_range[2,column]
  if(num >= lr && num <= ur){
    return(TRUE)
  }
  else return(FALSE)
}

# ============== Removing Outliers ================
remove_outliers = function(df){
  df = df[df$fare_amount >= manual_outlier_range[1,1] & df$fare_amount <= manual_outlier_range[2,1], ]
  df = df[df$pickup_longitude >= manual_outlier_range[1,2] & df$pickup_longitude <= manual_outlier_range[2,2], ]
  df = df[df$pickup_latitude >= manual_outlier_range[1,3] & df$pickup_latitude <= manual_outlier_range[2,3], ]
  df = df[df$dropoff_longitude >= manual_outlier_range[1,4] & df$dropoff_longitude <= manual_outlier_range[2,4], ]
  df = df[df$dropoff_latitude >= manual_outlier_range[1,5] & df$dropoff_latitude <= manual_outlier_range[2,5], ]
  df = df[df$passenger_count >= manual_outlier_range[1,6] & df$passenger_count <= manual_outlier_range[2,6], ]
  return(df)
}


cabfare_train = remove_outliers(training_data)

hist_plot('fare_amount', cabfare_train, binsize = 10)
box_plot(column = 'passenger_count', cabfare_train)

#for cauculating distance from logi/lati
library(geosphere)
cabfare_train$pickup_longitude = abs(cabfare_train$pickup_longitude)
cabfare_train$pickup_latitude = abs(cabfare_train$pickup_latitude)
cabfare_train$dropoff_longitude = abs(cabfare_train$dropoff_longitude)
cabfare_train$dropoff_latitude = abs(cabfare_train$dropoff_latitude)

#cabfare_train$long = abs(cabfare_train$pickup_longitude - cabfare_train$dropoff_longitude)
#cabfare_train$lati = abs(cabfare_train$pickup_latitude - cabfare_train$dropoff_latitude)

cabfare_train = cabfare_train %>% 
  mutate(distance = by(cabfare_train, 1:nrow(cabfare_train), function(row) { 
    distHaversine(c(row$pickup_longitude, row$pickup_latitude), c(row$dropoff_longitude,row$dropoff_latitude))/1000}))
summary(cabfare_train$distance)

cabfare_train$distance[cabfare_train$distance > 500 ] <- 0
cabfare_train$distance[cabfare_train$distance < 0 ] <- 0




#***************************Visualisation ***************************
library(ggplot2)
library(scales)

par(mar=c(3,3,1,1))
par(mfrow=c(3,2))

#A)Univariate
#variable quantity
#par("mar") - set it to par(mar= c(1,1,1,1)) to avoid margin error for plots
#par("mar")
hist(cabfare_train$fare_amount, main="ScatterPlot of fare_amount")
hist(cabfare_train$passenger_count, main="ScatterPlot of passenger_count")
hist(cabfare_train$month, main="ScatterPlot of month")
hist(cabfare_train$year, main="Hist of year")
hist(cabfare_train$dayOfWeek, main="Hist of dayOfWeek")
hist(cabfare_train$hour, main="Hist of hour")
plot(cabfare_train$distance, main="ScatterPlot of distance")


#check skwness of the target variable
library(PerformanceAnalytics)
skew_xts <-  skewness(cabfare_train$fare_amount)
skew_xts
#unlike python where i kept my fare under 100 and it gave me skew of 2.9 , R gives me 10.85835 when the fare is kept under 500.



#B)bi-variant 
#Continuous variables vs target variable 
#PLoting the graph for hour and Fare
ggplot(cabfare_train,aes(x = hour, y = fare_amount))+
  geom_line()+
  labs(x= "hour of the day")+
  scale_x_discrete(limits = c(0:23))+
  scale_y_continuous(limits=c(0,180))
#From the above graph we can see that the timeing is not affecting too much. Maximin dots are below 100. 

#PLoting the year for distance and Fare
ggplot(cabfare_train,aes(x = year, y = fare_amount))+geom_point()+
  geom_line()+
  labs(x= "year")+
  scale_x_discrete(limits = c(0:23))+
  scale_y_continuous(limits=c(0,180))

#PLoting the graph for passanger_count and Fare
gplot_p <- ggplot(data=cabfare_train, aes(x=passenger_count, y=fare_amount)) + geom_point()+ geom_line()+ 
  ggtitle("Time and Fare Plot") +
  xlab("Passenger Count ") + 
  ylab("Fare")
gplot_p
# From the Graph it seems passenger count is not affecting the fare and frequency of 1 pssenges are high

#PLoting the graph for day and Fare
gplot_d <- ggplot(data=cabfare_train, aes(x=dayOfWeek, y=fare_amount)) + geom_point()+ geom_line()+ 
  ggtitle("Day count and Fare Plot") +
  xlab("Day Count ") + 
  ylab("Fare")
gplot_d

#PLoting the graph for distance and Fare
gplot <- ggplot(data=cabfare_train, aes(x=cabfare_train$distance, y=cabfare_train$fare_amount)) + geom_point()+ geom_line()+ 
  ggtitle("Distance and Fare Plot") +
  xlab("Distance in KM ") + 
  ylab("Fare")
gplot

# creating copy of preprocessed data 
mydata = cabfare_train

#5.*************************** Feature selection ***************************
#install.packages("corrgram") # for correlation graph
library(corrgram)

#A. Correlation check on continuous variable
round(cor(numeric_data),2) #Correlation table column wise
corrgram(cabfare_train[, numeric_index], order = F, upper.panel = panel.pie, text.panel = panel.txt, main = "correlation plot") 


#Multicollinearity test
library(usdm)

vifcor(cabfare_train[,c(0,1,2,3)])

############################MODELING########################
############################################################

#Sampling
#1. Non scaled data(scalling is not required as i am using scaled data for my model)
set.seed(101)
train_index = sample(1:nrow(cabfare_train), 0.8*nrow(cabfare_train))
data_train = cabfare_train[train_index,] 
data_test = cabfare_train[-train_index,]

train_cab_final = subset(cabfare_train, select = c(1,7:12))
summary(train_cab_final)
str(train_cab_final)



#function error matrics
mape = function(actual, predict){
  mean(abs((actual-predict)/actual))*100}
rmse = function(actual, predict){
  sqrt(sum((predict - actual)^2) / nrow(train_cab_final))}
rmpe = function(actual, predict){
  sqrt(sum((predict - actual)^2) / nrow(train_cab_final)) / mean(actual)}

-------------------------------------------------------------
#1. Linear regression
#fit the Model with fare amount and all independant variables.
linear_Model <- lm(fare_amount~., data = train_cab_final)
summary(linear_Model)
#predicting the Fare amount for Test data
linear_prediction<-predict(linear_Model, newdata = train_cab_final)
View(train_cab_final)
View(linear_prediction)

mape(train_cab_final$fare_amount, linear_prediction) #23.71183
rmse(train_cab_final$fare_amount, linear_prediction) #`2.213257`
rmpe(train_cab_final$fare_amount, linear_prediction) #0.26
#Multiple R-squared:  0.6847,	Adjusted R-squared:  0.6846 

#2. DecisionTrees
#fit the Model with fare amount and all independant variables.
library(rpart)
library(rpart.plot)
Decision_Model <- rpart(fare_amount~., data = train_cab_final)
summary(Decision_Model)
#predicting the Fare amount for Test data
Decision_prediction<-predict(Decision_Model, newdata = train_cab_final)
View(train_cab_final)
View(Decision_prediction)

mape(train_cab_final$fare_amount, Decision_prediction) #20.71183
rmse(train_cab_final$fare_amount, Decision_prediction) #`4.513257`
rmpe(train_cab_final$fare_amount, Decision_prediction) #0.26
#Multiple R-squared:  0.7747,	Adjusted R-squared:  0.93 


##2. *******************Random forest*************************
#*************************************************************
library(randomForest)
library(inTrees)

#model
model_RF = randomForest(fare_amount ~. , train_cab_final, importance = TRUE, ntree = 500)

#Error plotting
plot(model_RF) #my error i decreasing with higher number of trees

# Checking model by predicting on out of sample data
RandomForest_prediction <- predict(model_RF, train_cab_final)
print(model_RF)

mape(train_cab_final$fare_amount, RandomForest_prediction) #10.52
rmse(train_cab_final$fare_amount, RandomForest_prediction) #0.95
rmpe(train_cab_final$fare_amount, RandomForest_prediction) #0.11
#Mean of squared residuals: 4.049305
#% Var explained: 71.36

varImpPlot(model_RF)
-------------------------------------
  model_RF1 = randomForest(fare_amount ~ distance , train_cab_final, importance = TRUE, ntree = 100)

#Error plotting
plot(model_RF1) #my error i decreasing with higher number of trees

# Checking model by predicting on out of sample data
predictRF1 <- predict(model_RF, train_cab_final)
print(model_RF1)

mape(train_cab_final$fare_amount, RandomForest_prediction) #10.52
rmse(train_cab_final$fare_amount, RandomForest_prediction) #0.95
rmpe(train_cab_final$fare_amount, RandomForest_prediction) #0.11
#Mean of squared residuals: 4.049305
#% Var explained: 71.36

plot(train_cab_final$fare_amount, predict_fare_simple, xlab = 'Actual values', ylab = 'Predicted values', main = 'simple liner')
plot(train_cab_final$fare_amount, predict_fare_lm, xlab = 'Actual values', ylab = 'Predicted values', main = 'Multilinear')
plot(train_cab_final$fare_amount, RandomForest_prediction, xlab = 'Actual values', ylab = 'Predicted values', main = 'Random forest') 
#choosing random forest for my test dataset

#===========Predicting for Test Data========================#
apply(cabfare_test, 2, function(x){sum(is.na(x))})#no missing value
str(cabfare_test)
cabfare_test <- mutate(cabfare_test,
                       pickup_datetime = ymd_hms(`pickup_datetime`),
                       month = as.integer(month(pickup_datetime)),
                       year = as.integer(year(pickup_datetime)),
                       dayOfWeek = as.integer(wday(pickup_datetime)),
                       hour = hour(pickup_datetime),
                       hour = as.integer(hour(pickup_datetime))
)
summary(cabfare_test)

cabfare_test = cabfare_test %>% 
  mutate(distance = by(cabfare_test, 1:nrow(cabfare_test), function(row) { 
    distHaversine(c(row$pickup_longitude, row$pickup_latitude), c(row$dropoff_longitude,row$dropoff_latitude))/1000}))
summary(cabfare_test$distance)
str(cabfare_test)
View(cabfare_test)

test_cab_final = subset(cabfare_test, select = c(6:11))
View(test_cab_final)
str(test_cab_final)

RandomForest_prediction_test = predict(model_RF, test_cab_final)


#================Comparing the result==============#
train_cab_final$predict_fare_lm = predict_fare_lm
train_cab_final$predictRF = predictRF
view(test_cab_final)

test_cab_final$fare_amount = predictRF_test

#PLoting the graph for distance and Fare
gplot <- ggplot(data=test_cab_final, aes(x=distance, y=fare_amount)) + geom_point()+ geom_line()+ 
  ggtitle("Distance and Fare Plot") +
  xlab("Distance in KM ") + 
  ylab("Fare")
gplot



#=============END of the Script===========#



