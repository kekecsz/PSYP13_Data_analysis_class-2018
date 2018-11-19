###########################################################
###########################################################
#   Exercise 11 - The basics of linear regression         #
###########################################################
###########################################################

###########################################################
#                                                         #
#                 Loading packages                        #
#                                                         #
###########################################################

library(psych) # for describe
library(dplyr) # for data management
library(gsheet) # to read data from google sheets
library(ggplot2) # for ggplot

###########################################################
#                                                         #
#                 Custom functions                        #
#                                                         #
###########################################################

# The following custom functions will come in handy during the visualization of the error of the predictions of the regressions line.
# This is not essential for the analysis and you don't need to learn how this custom function works if you don't want to. 

error_plotter <- function(mod, col = "black", x_var = NULL){
  mod_vars = as.character(mod$call[2])
  data = eval(parse(text = as.character(mod$call[3])))
  y = substr(mod_vars, 1, as.numeric(gregexpr(pattern ='~',mod_vars))-2)
  x = substr(mod_vars, as.numeric(gregexpr(pattern ='~',mod_vars))+2, nchar(mod_vars))
  
  data$pred = predict(mod)
  
  if(x == "1" & is.null(x_var)){x = "response_ID"
  data$response_ID = 1:nrow(data)} else if(x == "1"){x = x_var}
  
  plot(data[,y] ~ data[,x], ylab = y, xlab = x)
  abline(mod)
  
  for(i in 1:nrow(data)){
    clip(min(data[,x]), max(data[,x]), min(data[i,c(y,"pred")]), max(data[i,c(y,"pred")]))
    abline(v = data[i,x], lty = 2, col = col)
  }
  
}


###########################################################
#                                                         #
#     Data management and descriptive statistics          #
#                                                         #
###########################################################

# Lets say we are a complany that sells shows, and we would like to be able to tell people's shoe size just by knowing their height.
# We collect some data using this simple survey:

# Data collection form: https://goo.gl/forms/nNXWwCPTbdHxGrBO2

# You can load the data we just collected with the following code
mydata = as.data.frame(gsheet2tbl("https://docs.google.com/spreadsheets/d/1C07SRvPJzftZaFApiI5YgptyymvyYQpIUi9v6gFbLHw/edit?usp=sharing"))
# or you can use this permanent link from github, that contains an older version of the same data table
# mydata = read.csv("https://bit.ly/2Q0qZgP")

# You should always check the dataset for coding errors or data that does not make sense.
# View data in the data viewer tool 
View(mydata)

# or display simple descriptive statistics and plots
describe(mydata$height)
describe(mydata$shoe_size)

# histograms
hist(mydata$height, breaks = 20)
hist(mydata$shoe_size, breaks = 10)

# scatterplot
plot(shoe_size ~ height, data = mydata)

# use the table() function for categorical data
table(mydata$gender)

# you should exclude or correct data that is not valid
mydata_cleaned = mydata # create a copy of the data where which will be cleaned


mydata_cleaned[mydata_cleaned[,"height"] == "168cm", "height"] = 168
mydata_cleaned[,"height"] = as.numeric(as.character(mydata_cleaned[,"height"]))
mydata_cleaned[mydata_cleaned[,"height"] == 64, "height"] = 164

describe(mydata_cleaned[,"height"])

mydata_cleaned = mydata_cleaned[-which(mydata_cleaned[,"gender"] == "etc."),] # exclude invalid value
# this code might be required to correct the dataframe as well if you have a cell where "female" has a space after it.
# mydata_cleaned[,"gender"][mydata_cleaned[,"gender"] == "female "] = "female" # unify different coding variations

# check that the variable is clean now
table(mydata_cleaned$gender)



###########################################################
#                                                         #
#            Prediction with linear regressioin           #
#                                                         #
###########################################################

## how can I predict the outcome?

# find a pattern in the data instead of focusing on the individual datapoints

# run a regression analysis
mod1 <- lm(shoe_size ~ height, data = mydata_cleaned)

# In simple regression, we identifies the underlying pattern of the data, by fitting a single straight line that is closest to all data points.
plot(shoe_size ~ height, data = mydata_cleaned)
abline(mod1)

# This returns a matematical equation (called the regression equation) with which you can predict the outcome by knowing the value of the predictors.
# Y = b0 + b1*X

mod1

# You don't have to do the calculations by hand, you can get the predicted values by using the predict() function.
# The predictors have to have the same variable name as in the regression formula, and they need to be in a dataframe
height = c(150, 160, 170, 180, 190)
height_df = as.data.frame(height)
predictions = predict(mod1, newdata = height_df)
height_df_with_predicted = cbind(height_df, predictions)
height_df_with_predicted


# predicted values all fall on the regression line
plot(shoe_size ~ height, data = mydata_cleaned)
abline(mod1)
points(height_df_with_predicted, col = "red", pch = 19, cex = 2)


## How to measure model performance? (Is this model prediction any good?)
# You can measure how effective your model is by measureing the difference between the actual outcome values and the predicted values. We call this residual error in regression.
# The residual error for each observed shoe size can be seen on this plot 
# (this will only work if you ran the code in the top of the script containing the error_plotter() custom function). This code is only for visualization purposes.
error_plotter(mod1, col = "blue")

# You can simply add up all the residual error (the length of these lines), and 
# get a good measure of the overall efficiency of your model. 
# This is called the residual absolute difference (RAD). However, this value is 
# rarely used. More common is the residual sum of squared differences (RSS).
RAD = sum(abs(mydata_cleaned$shoe_size - predict(mod1)))
RAD

RSS = sum((mydata_cleaned$shoe_size - predict(mod1))^2)
RSS



## Is the predictor useful?
# To establish how much benefit did we get by taking into account the predictor, we can compare the residual error when using out best guess (mean) without taking into account the predictor, with the residual error when the predictor is taken into account.

# Below you can find regression model where we only use the mean of the 
# outcome variable to predict the outcome value.
# We can calculate the sum of squared differences the same way as before, 
# but for the model without any predictors, we call this the total sum of 
# squared differences (TSS).
mod_mean <- lm(shoe_size ~ 1, data = mydata_cleaned)
# plot the residual error
error_plotter(mod_mean, col = "red", x_var = "height")
# sum of squared differences between actual value and prediction
TSS = sum((mydata_cleaned$shoe_size - predict(mod_mean))^2)
TSS




# The total amount of information gained about the variability of the outcome is shown by the R squared statistic (R^2).
R2 = 1-(RSS/TSS)
R2
# This means that by using the regression model, we are able to explain R2 of the variability in the outcome.

# R^2 = 1 means all variablility of the outcome is perfectly predicted by the predictor(s)
plot(mydata_cleaned$shoe_size, mydata_cleaned$shoe_size)

# R^2 = 0 means no variablility  of the outcome is predicted by the predictor(s)
plot(sample(1:nrow(mydata_cleaned)), mydata_cleaned$shoe_size)



# Is the model with the predictor significantly better than a model without the predictor?
# do an anova to find out, comparing the amount of variance explained by the two models
anova(mod_mean, mod1)



# Or, you can get all this information from the model summary
summary(mod1)

# confidence interval of the regression coefficients
confint(mod1)

# plot the confidence intervals

ggplot(mydata_cleaned, aes(x = height, y = shoe_size))+
  geom_point()+
  geom_smooth(method='lm',formula=y~x)
