# Import data 


data <- read.csv("C:/Users/shubhamv/Downloads/Airfares.csv")
library(psych)
head(data,10)
str(data)
dim(data)
summary(data)


summary(data$S_INCOME)
sum(is.na(data)) # There is no missing values.
#____________________________________________________________________________ 

# Data manipulation 
data$NEW <- as.factor(data$NEW)
data$NEW <- as.numeric(data$NEW)
table(data$NEW)
data$VACATION <- as.factor(data$VACATION)
data$VACATION <- as.numeric(data$VACATION)
table(data$VACATION)
class(data$VACATION)
data$SW <- as.factor(data$SW)
data$SW <- as.numeric(data$SW) # label encoding
table(data$SW)

#================================================================================
#class(data$S_INCOME)
#data$S_INCOME <- as.numeric(data$S_INCOME)
#data$S_INCOME <- as.numeric(substr(gsub(",","",data$S_INCOME,2,6)))
#class(data$E_INCOME)
#data$E_INCOME <-  as.numeric(substr(gsub(",","",data$E_INCOME,2,6)))

#=====================================================================

#Many of our numeric fields contain special characters so we will need to clean 
#them before we can run any analysis.

class(data$FARE)
data$FARE <- as.numeric(substring(as.character(data$FARE),2))

toberemovedChars <- c("$",",")

for(c in toberemovedChars)
{
  data$S_INCOME <- sub(x = as.character(data$S_INCOME), 
                           pattern = c,replacement = "",fixed = TRUE)
  data$E_INCOME <-  sub(x = as.character(data$E_INCOME), 
                            pattern = c,replacement = "",fixed = TRUE)
}

data$S_INCOME <- as.numeric(data$S_INCOME)
data$E_INCOME <- as.numeric(data$E_INCOME)

#===============================================================

#data$FARE <- as.numeric(data$FARE)
#FARE_1 <- gsub(",","",data$FARE)
#as.numeric(FARE_1)
#data$FARE <- as.numeric(substr(gsub(",","",data$FARE,2,6)))
#class(data$FARE)

data$SLOT <- as.factor(data$SLOT)
class(data$SLOT)
data$SLOT<- as.numeric(data$SLOT)
table(data$SLOT)
data$GATE <- as.factor(data$GATE)
data$GATE <- as.numeric(data$GATE)
table(data$GATE)
#__________________________________________________________________________________

#install.packages('funModeling') # OR by using ggplot we can plot hist on same page
library(funModeling)

plot_num(data) # Plotting distributions for numerical variables

# Getting frequency distributions for categoric variables

#library(dplyr)
#factorVar <- data %>% select(SLOT,GATE,NEW,VACATION,SW) 

# Frequency distribution

#freq(factorVar)

#______________________________________________________________
# Box plots 
str(data)

par(mfrow = c(3,3)) # plotting window of 2 rows 2 columns
boxplot(data$COUPON,col = 'red',main = 'COUPON',horizontal = TRUE)
boxplot(data$HI,col = 'blue',main = 'HI',horizontal = TRUE)
boxplot(data$S_INCOME,col = 'purple',main = 'S_INCOME',horizontal = TRUE)
boxplot(data$E_INCOME,col = 'pink',main = 'E_INCOME',horizontal = TRUE)
boxplot(data$S_POP,col = 'green',main = 'S_POP',horizontal = TRUE)
boxplot(data$E_POP,col = 'yellow',main = 'E_POP',horizontal = TRUE)
boxplot(data$DISTANCE,col = 'orange',main = 'DISTANCE',horizontal = TRUE)
boxplot(data$PAX,col = 'brown',main = 'PAX',horizontal = TRUE)
boxplot(data$FARE,col = 'sky blue',main = 'FARE',horizontal = TRUE)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Scatter plot

# The basic syntax of ggplot2 is:
  
#  ggplot(data, mapping=aes()) +
#  geometric object 

# arguments: 
#  data: Dataset used to plot the graph
# mapping: Control the x and y-axis 
# geometric object: The type of plot you want to show. The most common object are:
  
#  - Point: `geom_point()` 
# - Bar: `geom_bar()`
# - Line: `geom_line()` 
# - Histogram: `geom_histogram()`

# Basic scatter plot
library(ggplot2)

# To arrange multiple ggplots on one single page, we'll use the function ggarrange()[in ggpubr]

#install.packages('ggpubr')
library(ggpubr)

plot_1 <- ggplot(data, aes(x = COUPON, y = FARE)) +
           geom_point()

plot_2 <-ggplot(data,aes(x = HI, y = FARE )) +
           geom_point()

plot_3 <-ggplot(data,aes(x = S_INCOME, y = FARE )) +
  geom_point()

plot_4 <- ggplot(data, aes(x = E_INCOME, y = FARE)) +
  geom_point()

plot_5 <- ggplot(data,aes(x = S_POP, y = FARE)) +
  geom_point()

plot_6 <- ggplot(data,aes(x=DISTANCE,y = FARE)) +
  geom_point()

plot_7 <- ggplot(data,aes(x=PAX, y = FARE)) +
  geom_point()

ggarrange(plot_1,plot_2,plot_3,plot_4,plot_5,plot_6,plot_7,
          labels = c("A","B","C","D","E","F","G"),
          ncol = 3 , nrow = 3) 

# Scatter plot with groups

str(data)

ggplot(data,aes(x = (DISTANCE), y = (FARE))) +
  geom_point(aes(color = factor(VACATION)))

# The aes() inside the geom_point() controls the color of the group. 
# The group should be a factor variable. Thus, you convert the variable in a factor.

#_______________________________________________________________________________
# Correlation matrix 

str(data)
library(corrplot)

nums <- sapply(data, is.numeric) 
mcor <- round(cor(data[nums]),2)
corrplot(mcor,method = 'number')

#____________________________________________________________________________

str(data)
nums <- data[c(1,5:9,12,13)]
names(nums)
pairs.panels(nums,density = TRUE,scale = TRUE,method = 'pearson',hist.col = 'cyan',
             show.points = FALSE)


# Data splitting

set.seed(123)
sample_data <- sample(2,nrow(data),replace = TRUE, prob = c(.7,.3))
table(sample_data)

train_data <- data[sample_data==1,]
dim(train_data)
test_data <- data[sample_data==2,]
dim(test_data)


write.csv(train_data,"train.csv")

# Model building 
# Model 1

model1 <- lm(FARE~. ,data = train_data)
summary(model1)

# Multiple R-squared:  0.7759,	Adjusted R-squared:  0.7692 

# Multicolienearity

library(car)
vif(model1)

#_____________________________________________________________________

# PREDICTION

predicted_data <- predict(model1)
predicted_data
summary(predicted_data)

#install.packages('Metrics')
library(Metrics)

result <- rmse(predicted_data,original_data)

original_data <- data$FARE
original_data
predicted_data
error <- original_data - predicted_data  # 64.11-67.004807        
-2.894807*-2.894807
error_sq <- error^2  # -2.894807*-2.894807  # SSE
error_sq_mean <- mean(error_sq) # MSE
rmse_data <- sqrt(error_sq_mean) # RMSE
rmse_data #  98.3956 

#==============================================================================

model2 <- lm(FARE~COUPON+VACATION+SW+HI+S_INCOME+E_INCOME+S_POP+E_POP+SLOT+GATE+DISTANCE+PAX,
             data = train_data) # New var removed
summary(model2)

# Multiple R-squared:  0.7754,	Adjusted R-squared:  0.7692 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

model3 <- lm(FARE~VACATION+SW+HI+S_INCOME+E_INCOME+S_POP+E_POP+SLOT+GATE+DISTANCE+PAX,
             data = train_data) # COUPON removed

summary(model3)

# Multiple R-squared:  0.7735,	Adjusted R-squared:  0.7678

vif(model3) # Multicolinearity

# VACATION       SW       HI S_INCOME E_INCOME    S_POP    E_POP     SLOT     GATE DISTANCE      PAX 
# 1.366286 1.525150 1.389508 1.851060 1.483013 1.994007 2.158478 1.535883 1.251834 1.407511 1.637204 

#==============================================================================

# PREDICTION

pred <- predict(model3)
pred
original_data <- data$FARE
original_data

error1 <- original_data-pred
result1 <- rmse(original_data,pred)
# 98.57613
#=========================================================================

# Assumptions _____________________________________________

# Normality

hist(error1,col = 'blue')

# Linearity
# First, linear regression needs the relationship between the independent and dependent 
# variables to be linear.  It is also important to check for outliers since linear regression 
# is sensitive to outlier effects.  The linearity assumption can best be tested with scatter plots

str(data)
par(mfrow=c(4,4))
plot(data$VACATION,error1)
plot(data$SW,error1)
plot(data$HI,error1)
plot(data$S_INCOME,error1)
plot(data$E_INCOME,error1)

plot(data$S_POP,error1)
plot(data$E_POP,error1)
plot(data$SLOT ,error1)
plot(data$GATE,error1)
plot(data$DISTANCE,error1)
plot(data$PAX,error1)

# Linearity Exist.
# we didn't find any specified pattern in plot , var have linear relationship  

# No Autocorrelation (no dependency of residuals) 

# Fourth, linear regression analysis requires that there is little or no autocorrelation in the data.  
# Autocorrelation occurs when the residuals are not independent from each other.  
# For instance, this typically occurs in stock prices, where the price is not independent 
# from the previous price

# While a scatterplot allows you to check for autocorrelations, you can test the 
# linear regression model for autocorrelation with the Durbin-Watson test.  
# Durbin-Watson's d tests the null hypothesis that the residuals are not linearly auto-correlated.  
# While d can assume values between 0 and 4, values around 2 indicate no autocorrelation.  
# As a rule of thumb values of 1.5 < d < 2.5 show that there is no auto-correlation in the data. 
# However, the Durbin-Watson test only analyses linear autocorrelation and only 
# between direct neighbors, which are first order effects.

library(lmtest)
dwtest(model3)
# H0 : the null hypothesis is the residuals are not linearly auto-correlated.

# Test statistics lies bw 0-2 means +ve autocorr,if its is exactly 2 mean no autocorr
# if its >2 & <4 its -ve autocorr.


# Heteroscedasticity # Constant error variance
# The last assumption of the linear regression analysis is homoscedasticity.  
# The scatter plot is good way to check whether the data are homoscedastic 
# (meaning the residuals are equal across the regression line).  
# The following scatter plots show examples of data that are not homoscedastic (i.e., heteroscedastic):

bptest(model3)
# Ho: hetroscadecity not present 

# studentized Breusch-Pagan test

# data:  model3
# BP = 46.674, df = 11, p-value = 2.457e-06

# p value is less than 0.05 ie we reject null hypo and except alternate
# ie there is hetroscadacity (constant error variance)


# Independance of error


plot(data$HI,error1, col='purple', main="Independence of error")

#the errors are independent (all the variables are independent )

#==============================================================================================

# VALIDATION 

#View(test_data)
dim(test_data)
write.csv(test_data,"test.csv")

predict(model3,data = test_data)
predict(model3,data = test_data,interval = "confidence") 

# fit        lwr       upr
# 67.004807  57.644891  76.36472

library(car) # residual plot 
residualPlot(model3,id.n=3)
qqPlot(model1, main="QQ Plot of residuals: model1") # qqplot

