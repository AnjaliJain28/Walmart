#Project 1:- Predict the analysis of sales and demand accurately.
#Dataset:- Walmart Retail store, US (45 store data)
setwd("~/Project")
getwd()

# Step-1 Importing the "Walmart_Store_sales" dataset.
#library(readr) can be used if u re using "read_csv()"
walmart<- read.csv("Walmart_Store_sales.csv")

# Step-2 Data Exploration.
# 2(a) See the imported data carefully and obsever the value.
View(walmart)

# 2(b) no of obs and variables stored in the walmart dataset
dim(walmart)
# Walmart_Store_sales has 6435 observations (rows) and 8 variables(columns). 
# It takes 338.8 KB memory in the system.

# 2(c) opt. check the top and last six observations 
# top 6 rows of obs
head(walmart)
# bottom 6 rows of obs
tail(walmart)

# 2(d) Types of data stored in a walmart dataset. 
str(walmart)

# 2(e) summary of the dataset with mean, median, max, min, quartile and other values.
summary(walmart)

# Step -3 Data Manuplation.
# 3(a) From the structure command we can see date was not in right format. 
# It needed to converted into a date format.
walmart<- read.csv("Walmart_Store_sales.csv", stringsAsFactors = FALSE)
#check the data again
str(walmart)

# The factor string converted into the char as there is - in it. 
# There are many ways to convert date into date format.
# But here, I am using lubridate package for it.

#install.packages("lubridate")
library('lubridate')
walmart$Date <- dmy(walmart$Date)
View(walmart)
str(walmart)
# As we see date is now in date format. We can also convert store into a factor but there is no need for it.
# So i will skip it. Otherwise run 
# walmart$Store <- as.factor(walmart$Store) 
# This will converts the store from numeric to factor data type.


# Task:-  Basic Statistics.
# Task1:- Which store has maximum sales?

# use the dplyr package for further manpulation and analysis.
# install.packages("dplyr")
library(dplyr)

# group by store 
# Find the sum, mean, std devn cof of varaience.
# Summarize it and store in other variable called walmart2
walmart2 <- summarize(group_by(walmart,Store), total_sale= sum(Weekly_Sales),
                     mean_sale = mean(Weekly_Sales),
                     stdev_sale = sd(Weekly_Sales),                         
                     cof_var_sale = (sd(Weekly_Sales)/mean(Weekly_Sales)*100))
summary(walmart2)

# Ans 1(a)
# Method1 :- filter it by gaining the max value
filter(walmart2, total_sale >= max(walmart2$total_sale))
# method 2: By Arrange method for task 1:-
walmart %>% group_by(Store) %>%  summarise(total_sales=sum(Weekly_Sales)) %>% arrange(desc(total_sales))

# Task2(a) Which store has maximum standard deviation i.e., the sales vary a lot. 
filter(walmart2, stdev_sale >= max(walmart2$stdev_sale))

# Task2(b) Also, find out the coefficient of mean to standard deviation.
filter(walmart2, cof_var_sale >= max(walmart2$cof_var_sale))


# Task3:- Which store/s has good quarterly growth rate in Q3’2012?
# Quarterly Rate Growth = (Q3'2012 - Q2' 2012) / Q2' 2012
# If we see the dataframe of walmart there is no quarter variable in the dataset.
# (a) Find the quarter value of date by using quarter function and then store in the table.
walmart$qutr <- quarter(walmart$Date, with_year = TRUE)

# (b) Group by store, quarter and find the sum of weekly_sales
df1 <- walmart %>% group_by(Store,qutr) %>% summarise(quarter_sales = sum(Weekly_Sales))
View(df1)

# (c) find the lag between the each quarter
df1$lag1 <- lag(df1$quarter_sales,1)
df1

# qtrly growth rate for 2012 q2
df1 %>%  filter(qutr=='2012.3') %>%  mutate(growth_rate=(quarter_sales-lag1)*100/lag1) %>% arrange(desc(growth_rate))


# Task4:- Some holidays have a negative impact on sales. 
# Find out holidays which have higher sales than the mean sales in non-holiday season for all stores together
# (a) Mean sales of Non-Holiday season, filter Holiday Flag = 0 values and find the mean of weekly_sales.
walmart %>% filter(Holiday_Flag==0) %>% summarise(mean_sales = mean(Weekly_Sales))
# (b) filter holiday flag =1 and weekly_sales is greater than the mean_sale of the diff dates.
walmart %>% filter(Holiday_Flag == 1 & Weekly_Sales > 1041256) %>% distinct(Date)

# Task5:- Provide a monthly and semester view of sales in units and give insights
# 5(a) Semenster View:- 

# New Variable in datafarme which has name Semenster is generated and store the sem values of the dates.
walmart$Semester <- semester(walmart$Date, with_year = TRUE)
View(walmart)

# Find the semester wise weekly sales of each store and store in other variable. 
sem <- walmart %>% group_by(Semester) %>% summarise(Sem_sales = sum(Weekly_Sales))
sem

# Use ggplot2 library for plotting the graph between Semester and Sem_sales.
# install.package("ggplot2")
library(ggplot2)
ggplot(data = sem, mapping = aes(x = factor(Semester), y = Sem_sales))+geom_bar(stat = 'identity')

# 5(b) Month View:- 

# New Variable in datafarme which has name Month is generated and store the month values of the dates.
walmart$Month <- month(walmart$Date)
View(walmart)

# Find the Month wise weekly sales of each store and store in other variable. 
month <- walmart %>% group_by(Month) %>% summarise(Month_sales = sum(Weekly_Sales))
month

# Use ggplot2 library for plotting the graph between Month and Month_sales.
ggplot(data = month, mapping = aes(x = factor(Month), y = Month_sales))+geom_bar(stat = 'identity')

# Task:- Statistical Model For Store 1 – Build  prediction models to forecast demand
# Task (a):- Linear Regression – Utilize variables like date and 
# restructure dates as 1 for 5 Feb 2010 (starting from the earliest date in order). 
# Hypothesize H(0) = CPI, unemployment, and fuel price have any impact on sales.

store_1 <- walmart %>% filter(Store==1) %>% arrange(Date)
View(store_1)
rownames(store_1)
colnames(store_1)
# restructuring date
store_1$ord_date <- rownames(store_1)

# linear model using sales as the dependent var
# With all parameters.
ln_mod <- lm(Weekly_Sales ~ . ,data=store_1)
summary(ln_mod)
# R-sq values is 22.07 %. It means model only explain 22.07 data variablity. So, it is not a good model without order date.
#Temperature and CPI is the only variable that can be used according to it.

# How to check which variable is not affected the weekly sales?
# step 1 - to look at the P value for F statistic
# since p <0.05 , so we fail to reject that all the coefficients are together=0
# step 2 - to look at individual varaibles 
# h0: the coeff for the variable in the model =0
# since for holiday p>0.05, so we fail to reject the h0

# Model 2 with only selected values.
ln_mod_2 <- lm(Weekly_Sales ~ Holiday_Flag+
                  Temperature+
                  Fuel_Price+
                  CPI+
                  Unemployment+
                  Date,data=store_1)
summary(ln_mod_2)
# Model2 has only 14% of data variability. not a good model.
ln_mod_3 <- lm(Weekly_Sales ~ Holiday_Flag+
                 Temperature+
                 Fuel_Price+
                 CPI+
                 Unemployment, data=store_1)
summary(ln_mod_3)
# we can reject fuel price and unemployment variable as there value is greater than 0.05
# only temperature and CPI has effects on the weekly sales of the stores
# model 4 has only 13% data variablity.
ln_mod_4 <- lm(Weekly_Sales ~ Holiday_Flag+Temperature+CPI,data=store_1)
summary(ln_mod_4)

#Next step train and test the data
# Spilting into Train and test data
library('caTools')
#set.seed(30)
mod <- sample.split(store_1$Weekly_Sales,SplitRatio = 0.7)
mod
#creating train and test data set
st1_train <- subset(store_1, mod == TRUE)
st1_test  <- subset(store_1, mod == FALSE)
#train the model
model <- lm(Weekly_Sales ~ Temperature+CPI,data=st1_train)
model
summary(model)

# Make a prediction
# predict your model using test data
st1_test$pre_sales <- predict(model, newdata = st1_test)
View(st1_test)

# Error between the predicted and original value
st1_test$error <-  st1_test$pre_sales - st1_test$Weekly_Sales
View(st1_test)

library('car')
vif(model)

# Task (b):- Change dates into days by creating new variable.
store_1$Day <- day(store_1$Date)
View(store_1) 
colnames(store_1)

lm <- lm(Weekly_Sales ~ Holiday_Flag+
                 Temperature+
                 Fuel_Price+
                 CPI+
                 Unemployment+
                 Date+ Day + Month + qutr + Semester
               ,data=store_1)
summary(lm)
# 28 % variability 
# temp, date, day and month effects the model

lm_2 <- lm(Weekly_Sales ~ Temperature+ 
           Date+ 
           Day + 
           Month+
           Semester,data=store_1)
summary(lm_2)
# has 26 % variablity

mod_2 <- sample.split(store_1$Weekly_Sales,SplitRatio = 0.7)
mod_2
#creating train and test data set
st2_train <- subset(store_1, mod_2 == TRUE)
st2_test  <- subset(store_1, mod_2 == FALSE)
#train the model
model_2 <- lm(Weekly_Sales ~ Temperature+ 
             Date+ 
             Day + 
             Month+
             Semester,data=st2_train)
summary(model_2)

# Make a prediction
# predict your model using test data
st2_test$pre_sales <- predict(model_2, newdata = st2_test)
View(st2_test)

# Error between the predicted and original value
st2_test$error <-  st2_test$pre_sales - st2_test$Weekly_Sales
View(st2_test)

#based on the variables and condn we can select model_1 and model_2.

