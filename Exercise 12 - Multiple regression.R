###########################################################
###########################################################
#            Exercise 12 - Multiple regression            #
###########################################################
###########################################################


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



###### Multiple regression
# using multiple predictors in linear regression models

# Housing sales dataset from Kaggel
data_house = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/data_house_small_sub.csv")

### Predicting apartment sales prices using sqfootage and house grade

### check data set for invalid data (e.g. coding errors)
# descriptive statistics
describe(data_house)

# histograms
hist(data_house$price, breaks = 30)
hist(data_house$sqft_living, breaks = 30)
hist(data_house$grade, breaks = 30)

# scatterplots
plot(price ~ sqft_living, data = data_house)
plot(price ~ grade, data = data_house)

# fit the regression model
mod_house1 = lm(price ~ sqft_living + grade, data = data_house)

# plotting the scatterplots for each predictor separately
# with the simple regression regression lines
plot(price ~ sqft_living, data = data_house)
abline(lm(price ~ sqft_living, data = data_house))
plot(price ~ grade, data = data_house)
abline(lm(price ~ grade, data = data_house))

# plot the regression plane (3D scatterplot with regression plane)
scatter3d(price ~ sqft_living + grade, data = data_house)

# the regression equation: Y = b0 + b1*X1 + b2*X2


mod_house1


sqft_living = c(600, 600, 1100, 1100)
grade = c(6, 9, 6, 9)
newdata_to_predict = as.data.frame(cbind(sqft_living, grade))
prediction = predict(mod_house1, newdata = newdata_to_predict)

cbind(newdata_to_predict, prediction)

### What to report in a publication or and home assignment
# interpreting the summary
summary(mod_house1)

AIC(mod_house1)

confint(mod_house1)

lm.beta(mod_house1)




### Which predictor had the most unique information added to the model?
# we need to calculate the standardized coefficents to be able to compare them
lm.beta(mod_house1)



### categorical variables can be predictors

mod_cat = lm(price ~ sqft_living + grade + has_basement, data = data_house)


mod_cat

summary(mod_cat)
# plot the relationship of price and basement
plot(data_house$price ~ data_house$has_basement)




### higher order terms can be added as predictors
# but unless you know what you are doing, always
# add the first order term in the model as well, like here:
mod_house_quad <- lm(price ~ grade + I(grade^2), data = data_house)
summary(mod_house_quad)
plot(price ~ grade, data = data_house)

ggplot(data_house, aes(x = grade, y = price))+
  geom_point()+
  geom_smooth(method='lm',formula=y~x+I(x^2))



### interactions can be predictors

# the effect of goegraphic location added to the model
mod_house_geolocation = lm(price ~ sqft_living + grade + long + lat, data = data_house)
summary(mod_house_geolocation)

# the effect of the interaction of latitude and longitude added to the model
mod_house_geolocation_inter1 = lm(price ~ sqft_living + grade + long + lat + I(long * lat), data = data_house)
summary(mod_house_geolocation_inter1)
# this will result in the same output:
mod_house_geolocation_inter2 = lm(price ~ sqft_living + grade + long * lat, data = data_house)
summary(mod_house_geolocation_inter2)




