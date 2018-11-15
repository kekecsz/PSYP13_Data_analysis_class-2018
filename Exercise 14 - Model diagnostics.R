###########################################################
###########################################################
#          Exercise 14 - Model diagnostics                #
###########################################################
###########################################################


###########################################################
#                                                         #
#                 Loading packages                        #
#                                                         #
###########################################################

library(psych) # for describe
library(car) # for residualPlots, vif, pairs.panels, ncvTest



###########################################################
#                                                         #
#                     Data management                     #
#                                                         #
###########################################################

# Housing sales dataset from Kaggel
data_house = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/data_house_small_sub.csv")

###########################################################
#                                                         #
#                 Model diagnostics                       #
#                                                         #
###########################################################

## assumptions
# normality of residuals
# linear relationship of outcome and predictors
# homoscedasticity (homogeneity of variance)
# no multicollinearity



# Fit the final model (containing all predictors)
mod_house2 = lm(price ~ sqft_living + grade, data = data_house)

# extreme values
plot(data_house$price ~ data_house$sqft_living)
# assess leverage
abline(lm(price ~ sqft_living, data = data_house))
plot(mod_house2, which = 5)

# checking for influential outliers = extreme values with high leverage
# Cook's distance > 1, or Cook's distance > 4/N
4/nrow(data_house)
plot(mod_house2, which = 4)

## checking assumptions

# normality assumption
# QQ plot
plot(mod_house2, which = 2)
# skew and kurtosis
describe(residuals(mod_house2))
# histogram
hist(residuals(mod_house2), breaks = 20)

mod_house3 = lm(price ~ sqft_living + grade, data = data_house[-c(186, 113),])

# recheck the assumption of normality of residuals
describe(residuals(mod_house3))
plot(mod_house3, which = 2)
hist(residuals(mod_house3), breaks = 20)



# linearity assumption
# predicted values against actual values
pred <- predict( object = mod_house3 )
plot( x = pred, y = data_house[-c(186, 113),]$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")
# predicted values against residuals
plot(mod_house3, which = 1)
# residual plot for each predictor from the car package, returning the result of linearity tests
residualPlots(mod_house3)

# homoscedasticty assumption (homogeneity of variance)
plot(mod_house3, which = 3)
ncvTest(mod_house3)

# multicollinearity (VIF above 5), or a VIF threshold of 3 is recommended in this paper: http://onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2009.00001.x/full
# some info about VIF: 
# https://statisticalhorizons.com/multicollinearity
# http://blog.minitab.com/blog/understanding-statistics/handling-multicollinearity-in-regression-analysis
vif(mod_house3)
pairs.panels(data_house[,c("price", "sqft_living", "grade")], col = "red", lm = T)



# note that high vif is not a problem when it is only due to the inclusion of the interaction term

# model with interaction term
mod_house_geolocation_inter2 = lm(price ~ sqft_living + grade + long * lat, data = data_house[-c(186, 113),])
# multicollinearity
vif(mod_house_geolocation_inter2)

# model with interaction term
mod_house_geolocation_inter2_noint = lm(price ~ sqft_living + grade + long, data = data_house[-c(186, 113),])
# multicollinearity
vif(mod_house_geolocation_inter2_noint)

# pairwise correlations
pairs.panels(data_house[,c("price", "sqft_living", "grade", "long", "lat")], col = "red", lm = T)


