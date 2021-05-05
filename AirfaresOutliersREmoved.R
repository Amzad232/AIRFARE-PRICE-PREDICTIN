# Import data 

data <- read.csv("C:/Users/shubhamv/Downloads/Airfares.csv")
library(psych)
head(data,10)
str(data)
dim(data)
summary(data)
describe(data)


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


# Outliers Removed 

summary(data$COUPON)
COUPONIQR = 1.298-1.040
lower = 1.040 - (1.5*COUPONIQR)
upper = 1.298 + (1.5*COUPONIQR)

data <- data[data$COUPON>=lower & data$COUPON<=upper ,]
summary(data$COUPON) # max , mean , median changed after outliers remoevd

summary(data$HI)
HI_IQR = 5528-3199
lower1 = 3199 - (1.5*HI_IQR)
upper1 = 5528 + (1.5*HI_IQR)

data <- data[data$HI>=lower1 & data$HI<=upper1,]
summary(data$HI)

summary(data$S_INCOME)
S_INCOME_IQR = 29260-24706
lower2 = 24706 - (1.5*S_INCOME_IQR)
upper2 = 29260 + (1.5*S_INCOME_IQR)

data <- data[data$S_INCOME>=lower2 & data$S_INCOME<=upper2,]
summary(data$S_INCOME)

summary(data$DISTANCE)
DISTANCE_IQR = 1200-457 
lower3 = 457 - (1.5*DISTANCE_IQR) 
upper3 = 1200 + (1.5*DISTANCE_IQR)

data <- data[data$DISTANCE>=lower3 & data$DISTANCE <=upper3,]
summary(data$DISTANCE)

summary(data$PAX)
PAX_IQR = 14090-5328
lower4 = 5328 - (1.5*PAX_IQR)
upper4 = 14090 + (1.5*PAX_IQR)

data <- data[data$PAX>=lower4 & data$PAX<=upper4,]
summary(data$PAX)

summary(data$FARE)
FARE_IQR = 205-103.53
lower5 = 103.53 - (1.5*FARE_IQR)
upper5 = 205 + (1.5*FARE_IQR)

data <- data[data$FARE>=lower5 & data$FARE<=upper5,]
summary(data$FARE)

#======================================================================

# BOxplot after outliers removed

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

#================================================================================

# Feature Scaling or Normalizing data 

# One way to turn an average machine learning model into a good one is through the 
# statistical technique of normalizing of data. If we don't normalize the data, 
# the machine learning algorithm will be dominated by the variables that use a larger scale, 
# adversely affecting model performance. This makes it imperative to normalize the data.
# categorical target variable will not be normalized.

# Min-Max Normalization
# The formula for a min-max normalization is:
  
#  (X - min(X))/(max(X) - min(X))

# For each value of a variable, we simply find how far that value is from the minimum value, 
# then divide by the range. 

# To implement this in R, we can define a simple function and then use lapply to apply 
# that function to whichever columns in the dataset we would like:

# define Min-Max normalization function

  min_max_norm <- function(x) {
      
    (x - min(x)) / (max(x) - min(x))
  }

# apply Min-Max normalization 

data_norm <- as.data.frame(lapply(data[c(1,5:9,12:14)],min_max_norm))
head(data_norm)

data_norm$NEW <- data$NEW
data_norm$SW <- data$SW
data_norm$VACATION <- data$VACATION
data_norm$SLOT <- data$SLOT
data_norm$GATE <- data$GATE

str(data)

#_______________________________________________________________________________________

# MORE RELIABLE PREDICTION ARE MADE IF THE PREDICTORS & TARGET VAR ARE NORMALLY DISTRIBUTED.

# Transformation 
# Normal distribution (skewwed data)

data_norm$COUPON <- log(data_norm$COUPON)
data_norm$DISTANCE <- log(data_norm$DISTANCE)
data_norm$PAX <- log(data_norm$PAX)
data_norm$E_POP <- log(data_norm$E_POP)

#install.packages('rcompanion')
library(rcompanion)
PAX_log <- log(data$PAX)
plotNormalHistogram(data$PAX)
plotNormalHistogram(PAX_log) # see the comparison 

plot_num(data_norm) # see the comparison
plot_num(data) # before after.
#______________________________________________________________________________


# Data splitting

set.seed(123)
sample_data <- sample(2,nrow(data_norm),replace = TRUE, prob = c(.7,.3))
table(sample_data)

train_data <- data_norm[sample_data==1,]
dim(train_data)
test_data <- data_norm[sample_data==2,]
dim(test_data)


write.csv(train_data,"train.csv")

# Model building after outliers rem , feature scaling , skewd(logtransform) 
# Model 1

model1 <- lm(FARE~. ,data = train_data)
summary(model1)

#ggplot(model1,aes(x=PAX,y=FARE)) + geom_point() + geom_smooth(method = lm)
  

# Multiple R-squared:  0.7765,	Adjusted R-squared:  0.7682
# (Intercept)  9.070e+01

# MULTICOLLINEARITY

# In the presence of multicollinearity, the solution of the regression model becomes unstable.
# For a given predictor (p), multicollinearity can assessed by computing a score 
# called the variance inflation factor (or VIF), which measures how much the variance of a regression 
# coefficient is inflated due to multicollinearity in the model.

# The smallest possible value of VIF is one (absence of multicollinearity). As a rule of thumb,
# a VIF value that exceeds 5 or 10 indicates a problematic amount of collinearity (James et al. 2014).

# When faced to multicollinearity, the concerned variables should be removed, since the 
# presence of multicollinearityimplies that the information that this variable
# provides about the response is redundant in the presence of the other variables 

# If multicollinearity is found in the data, centering the data 
# (that is deducting the mean of the variable from each score) might help 
# to solve the problem.  However, the simplest way to address the problem is 
# to remove independent variables with high VIF values.


library(car)
vif(model1)

#_____________________________________________________________________

# PREDICTION

predicted_data <- predict(model1)
predicted_data
summary(predicted_data)

#install.packages('Metrics')
library(Metrics)

original_data <- data$FARE
result <- rmse(predicted_data,original_data) # 94.08085 earliar its was 98.3956

original_data
predicted_data
error <- original_data - predicted_data          
-2.894807*-2.894807
error_sq <- error^2    # SSE
error_sq_mean <- mean(error_sq) # MSE
rmse_data <- sqrt(error_sq_mean) # RMSE
rmse_data #  94.08085 

#==============================================================================

model2 <- lm(FARE~VACATION+SW+HI+S_INCOME+E_INCOME+S_POP+E_POP+SLOT+GATE+DISTANCE+PAX,
             data = train_data) # New  & COUPON var removed
summary(model2)

# Multiple R-squared:  0.7753,	Adjusted R-squared:  0.7683  
# (Intercept)  6.021e+01

# PREDICTION

pred2 <- predict(model2)
pred2
original_data <- data$FARE
original_data

result1 <- rmse(original_data,pred2) # 94.19751

error1 <- original_data-pred2
result1 <- rmse(original_data,pred2)
# 94.19751
#=========================================================================

model3 <- lm(FARE~VACATION+SW+HI+S_POP+E_POP+S_INCOME+SLOT+DISTANCE+PAX,
                       data = train_data) # E_income removed
summary(model3)

# Multiple R-squared:  0.7733,	Adjusted R-squared:  0.7669
# (Intercept)  9.003e+01

pred3 <- predict(model3)
result3 <- rmse(original_data,pred3) #  93.9718
error2 <- original_data-pred3

# Assumptions _____________________________________________

# Normality

hist(error2,col = 'pink')

# Linearity
# First, linear regression needs the relationship between the independent and dependent 
# variables to be linear.  It is also important to check for outliers since linear regression 
# is sensitive to outlier effects.  The linearity assumption can best be tested with scatter plots

str(data)
par(mfrow=c(4,4))
plot(data$VACATION,error2)
plot(data$SW,error2)
plot(data$HI,error2)
plot(data$S_INCOME,error2)


plot(data$S_POP,error2)
plot(data$E_POP,error2)
plot(data$SLOT ,error2)
plot(data$GATE,error2)
plot(data$DISTANCE,error2)
plot(data$PAX,error2)

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
# BP = 46.674, df = 11, p-value = 1.432e-06

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

set.seed(123)
sample_data <- sample(2,nrow(data),replace = TRUE, prob = c(.7,.3))
table(sample_data)

train_data <- data[sample_data==1,]
dim(train_data)
test_data <- data[sample_data==2,]
dim(test_data)

test_pred <- data.frame(VACATION=1,SW=1,HI=5419.16,S_INCOME=26993,S_POP=3532657,E_POP=7145897,SLOT=2,
                       DISTANCE=576,PAX=8820)

predict(model3,newdata = test_pred)
predict(model3,newdata = test_pred,interval = "confidence") 


library(car) # residual plot 
residualPlot(model3,id.n=3)
qqPlot(model1, main="QQ Plot of residuals: model1") # qqplot

