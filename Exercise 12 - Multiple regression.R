###########################################################
###########################################################
#            Exercise 12 - Multiple regression            #
###########################################################
###########################################################

# This exercise will show you how multiple predictors can be used in the same regression model to achieve better prediction efficiency.

###########################################################
#                                                         #
#                 Loading packages                        #
#                                                         #
###########################################################

library(psych) # for describe
library(lm.beta) # for lm.beta
library(car) # for scatter3d
library(ggplot2) # for ggplot
library(rgl) # for scatter3d



###########################################################
#                                                         #
#                 Data management and descriptives        #
#                                                         #
###########################################################

#In this exercise we will predict the price of apartments and houses. 
#We use a dataset from Kaggle containing data about housing prices and variables that may be used to predict housing prices. The data is about accomodations in King County, USA (Seattle and sorrounding area).
#We only use a portion of the full dataset now containing information about N = 200 accomodations.
#You can load the data with the following code

data_house = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/data_house_small_sub.csv")

### Check the dataset

View(data_house)

# Display simple descriptive statistics and plots.
# We are going to predict price of the apartment using the variables sqft_living (the square footage of the living area), and grade (overall grade given to the housing unit, based on King County grading system), so lets focus on these variables.
# Later we are also going to use a categorical variable, has_basement (whether the apartment has a basement or not) as well.

describe(data_house)

# histograms
hist(data_house$price, breaks = 30)
hist(data_house$sqft_living, breaks = 30)
hist(data_house$grade, breaks = 30)

# scatterplots
plot(price ~ sqft_living, data = data_house)
plot(price ~ grade, data = data_house)

table(data_house$has_basement)
plot(data_house$price ~ data_house$has_basement)


###########################################################
#                                                         #
#                   Multiple regression                   #
#                                                         #
###########################################################

# We fit a regression model with multiple predictors: sqft_living and grade. In the formula, the predictors are separated by a + sign.
mod_house1 = lm(price ~ sqft_living + grade, data = data_house)

# tilde

# The regression equation is displayed just like in the case of simple regression

mod_house1

# It is not trivial to visualize the regression equation in multiple regression. You can plot every simple regression separately, but that is not an accurate depiction of the prediction using the model.

plot(price ~ sqft_living, data = data_house)
abline(lm(price ~ sqft_living, data = data_house))
plot(price ~ grade, data = data_house)
abline(lm(price ~ grade, data = data_house))

# Alternatively, you can use a 3 dimensional plot to visualize the regression plane.
scatter3d(price ~ sqft_living + grade, data = data_house)

# the regression equation: Y = b0 + b1*X1 + b2*X2



# Again, we can ask for predictions for specific values of predictors, but we need to specify all predictor values (in this case, both sqft_living and grade of the apartment) to get a prediction.
# Remember that you need to provide the predictors in a dataframe with the predictors having the same variable name as in the model formula
sqft_living = c(600, 600, 1100, 1100)
grade = c(6, 9, 6, 9)
newdata_to_predict = as.data.frame(cbind(sqft_living, grade))
predicted_price = predict(mod_house1, newdata = newdata_to_predict)

cbind(newdata_to_predict, predicted_price)



### What to report in a publication and home assignment

# In a publication (and in the home assignment) you will need to report the following information:
# First of all, you will have to specify the regression model you built. For example: 
# "In a linar regression model we predicted housing price (in USD) with square footage of living area (in ft) and King County housing grade as predictors."
# Next you will have to indicate the effectiveness of the model. You can do this by after a text summary of the results, giving information about the F-test of the whole model, specifically, the F value, the degrees of freedom (note that there are two degrees of freedom for the F test), and the p-value. You can find all this information in the model summary. Also provide information about the model fit using the adjusted R squared from the model summary and the AIC values provided by the AIC() function.
# Don't forget to use APA guidelines when determing how to report these statistics and how many decimal places to report (2 decimals for every number except for p values, which should be reported up to 3 decimals).

# Furthermore, you will have to provide information about the regression equation and the predictors' added value to the model. You can do this by creating a table with the following information:
# Regression coefficients with confidence intervals, and standardized beta values for each predictor, together with the p-values of the t-test.
# The regression coefficients and p-values can be found in the model summary, and the confidence interavlas and std. betas can be computed by applying the confint() and lm.beta() functions on the model object. (the lm.beta package is needed for the lm.beta() function)

sm = summary(mod_house1)
sm

AIC(mod_house1)

confint(mod_house1)
lm.beta(mod_house1)



### categorical variables can be predictors

# Categorical variables can be included in models just like continuous variables. Here, we include the variable has_basement as a predictor, which is a categorical variable that has two levels: 'has basement' and 'no basement'. In this case, the intercept can be interpreted as the predicted value for all continuous predictor values as 0, and the has_basement variable at its default level: 'has basement'. The regression coefficient for has_basement indicates how much price is predicted to change if the apartment has no basement compared to if it has basement.

mod_cat = lm(price ~ sqft_living + grade + has_basement, data = data_house)

mod_cat

summary(mod_cat)

# The default level (reference level) of categorical variables is the level earliest in the alphabet. For this reason, the reference level of the variable has_basement is "has basement".
# For more intuitive interpretation, it would make sense to change the reference level to "no basement", so that the model coefficient for this variable would be positive, and it would indicate how much price increase would a basement mean for the apartment sales.
# This can be done with the relevel() function
# This can be done with the relevel() function. We have to re-run the model for this change to take effect in the model object.

data_house$has_basement = relevel(data_house$has_basement, ref = "no basement")

mod_cat = lm(price ~ sqft_living + grade + has_basement, data = data_house)
summary(mod_cat)


### higher order terms can be added as predictors

# If you suspect that there is non-linear relationship between the outcome and some predictor, you can try to include a second or third order term.
# For example, here we can see that the relationship of price and grade is not entirely linear.
plot(price ~ grade, data = data_house)

# So we build a model including the second order term of grade, to account for a quadratic relationsip.
# Unless you know what you are doing, always add the first order term in the model as well, like here:

mod_house_quad <- lm(price ~ grade + I(grade^2), data = data_house)
summary(mod_house_quad)

ggplot(data_house, aes(x = grade, y = price))+
  geom_point()+
  geom_smooth(method='lm',formula=y~x+I(x^2))



### interactions can be predictors

# A relationship of different predictors can also be modelled, if you suspect that the association of a predictor and the outcome might depend on the value of another predictor.
# For example here we first build a model where we include the effect of geographic location (longitude and latitude) in the model (mod_house_geolocation), and next, we include the interaction of longitude and latitude in the model, because we suspect that these parameters might influence each others association with price.

mod_house_geolocation = lm(price ~ sqft_living + grade + long + lat, data = data_house)
summary(mod_house_geolocation)

mod_house_geolocation_inter2 = lm(price ~ sqft_living + grade + long * lat, data = data_house)
summary(mod_house_geolocation_inter2)

# Note that the adjusted R squared did not increase substancially due to the inclusion of the interaction term, so it might not be so useful to take into account the interaction, it might be enoug to take into account the main effects of longitude and latitude. This needs to be further evaluated with model comparison. See the exercise related to that.


