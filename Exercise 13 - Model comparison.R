###########################################################
###########################################################
#  Exercise 13 - Model comparison and model selection     #
###########################################################
###########################################################


###########################################################
#                                                         #
#                 Loading packages                        #
#                                                         #
###########################################################

# no specific package is needed for this exercise


###########################################################
#                                                         #
#                     Data management                     #
#                                                         #
###########################################################

# Housing sales dataset from Kaggel
data_house = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/data_house_small_sub.csv")

###########################################################
#                                                         #
#                 Hierarchical regression                 #
#                                                         #
###########################################################



# Using hierarchical regression, you can quantify the amount of information gained by adding a new predictor or a set of predictors to a previous model. To do this, you will build two models, the predictors in one is the subset of the predictors in the other model.

# Here we first build a model to predict the price of the apartment by using only sqft_living and grade as predictors.  
mod_house2 <- lm(price ~ sqft_living + grade, data = data_house)

# Next, we want to see whether we can improve the effeffctiveness of our prediction by taking into account geographic location in our model, in addition to living space and grade
mod_house_geolocation = lm(price ~ sqft_living + grade + long + lat, data = data_house)

# We can look at the adj. R squared statistic to see how much variance is explained by the new and the old model.

summary(mod_house2)$adj.r.squared
summary(mod_house_geolocation)$adj.r.squared

# It seems that the variance explained has increased substantially by adding information about geographic location to the model.

# Now, we should compare residual error and model fit thought the anova() function and the AIC() function.
# The anova() function can only be used for comparing models if the two models are "nested", that is, predictors in one of the models are a subset of predictors of the other model.
# If the anova F test is significant, it means that the models are significantly different in terms of their residual errors.
# If the difference in AIC of the two models is larger than 2, the two models are significantly different in their model fit. Smaller AIC means less error and better model fit, so in this case we accept the model with the smaller AIC. However, if the difference in AIC does not reach 2, we can retain either of the two models. Ususally we stick with the less complicated model in this case, but theoretical considerations and previous results should also be considered when doing model selection.

anova(mod_house2, mod_house_geolocation)

AIC(mod_house2)
AIC(mod_house_geolocation)

# The same procedure can be repeated if we have more than two steps/blocks in the hierarchical regression.

# Here we build a third model, which adds even more predictors to the formula. This time, we add information about the condition of the apartment.

mod_house_geolocation_cond = lm(price ~ sqft_living + grade + long + lat + condition, data = data_house)

# We can compare the three models now.
# R^2
summary(mod_house2)$adj.r.squared
summary(mod_house_geolocation)$adj.r.squared
summary(mod_house_geolocation_cond)$adj.r.squared

# anova
anova(mod_house2, mod_house_geolocation, mod_house_geolocation_cond)

# AIC
AIC(mod_house2)
AIC(mod_house_geolocation)
AIC(mod_house_geolocation_cond)

# Did we gain substantial information about housing price by adding information about the condition of the apartment to the model?


###########################################################
#                                                         #
#                      Model selection                    #
#                                                         #
###########################################################


# first rule of model selection:
# always go with the model that is grounded in theory and prior research
# because automatic model selection can lead to overfitting


# "Predicting" variability of the outcome in your original data is easy
# If you fit a model that is too flexible, you will get perfect fit on your intitial data.
# For example you can fit a line that would cover your data perfectly, reaching 100% model fit... to a dataset where you already knew the outcome.
# However, when you try to apply the same model to new data, it will produce bad model fit. In most cases, worse, than a simple regression.
# In this context, data on which the model was built is called the training set, and the new data where we test the true prediction efficiency of a model is called the test set. The test set can be truely newly collected data, or it can be a set aside portion of our old data which was not used to fit the model.
# Linear regression is very inflexible, so it is less prone to overfitting. This is one of its advantages compared to more flexible prediction approaches.

## Comparing model performance on the training set and the test set

# In the next part of the exercise we will demonstrate that the more predictors you have, the higher your R^2 will be, even if the predictors have nothing to do with your outcome variable.
# First, we will generate some random variables for demonstration purposes. These will be used as predictors in some of our models in this exercise. It is important to realize that these variables are randomly generated, and have no true relationship to the sales price of the apartments. Using these random numbers we can demonstrate well how people can be mislead by good prediction performance of models containing many predictors.

rand_vars = as.data.frame(matrix(rnorm(mean = 0, sd = 1, n = 50*nrow(data_house)), ncol = 50))
data_house_withrandomvars = cbind(data_house, rand_vars)

# We create a new data object from the first half of the data (N = 100). We will use this to fit our models on. This is our training set. We set aside the other half of the dataset so that we will be able to test prediction performance on it later. This is called the test set.

training_set = data_house_withrandomvars[1:100,] # this is the training set, here we only use half of the data
test_set = data_house_withrandomvars[101:200,] # this is the test set, the other half of the dataset

# Now we will perform a hierarchical regression where first we fit our usual model predicting price with sqft_living and grade on the training set. Next, we fit a model containing sqft_living and grade and the 50 randomly generated variables that we just created. 
# (the names of the random variables are V1, V2, V3, ...)
mod_house_train <- lm(price ~ sqft_living + grade, data = training_set)
mod_house_rand_train  <- lm(price ~ sqft_living + grade+ V1 + V2 + V3 + V4 + V5 + V6 + V7 + 
                              V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + 
                              V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                              V28 + V29 + V30 + V31 + V32 + V33 + V34 + V35 + V36 + V37 + 
                              V38 + V39 + V40 + V41 + V42 + V43 + V44 + V45 + V46 + V47 + 
                              V48 + V49 + V50,
                            data = training_set)


# Now we can compare the model performance.
# First, if we look at the normal R^2 indexes of the models or the RSS, we will find that the model using the random variables (mod_house_rand_train) was much better at predicting the training data. The error was smaller in this model, and the overall variance explained is bigger. You can even notice that some of the random predictors were identified as having significant added prediction value in this model, even though they are not supposed to be related to price at all, since we just created them randomly. This is because some of these variables are alligned with the outcome to some extend by random chance.

summary(mod_house_train)
summary(mod_house_rand_train)

pred_train <- predict(mod_house_train)
pred_train_rand <- predict(mod_house_rand_train)
RSS_train = sum((training_set[,"price"] - pred_train)^2)
RSS_train_rand = sum((training_set[,"price"] - pred_train_rand)^2)
RSS_train
RSS_train_rand

# That is why we need to use model fit idnexes that are more sensitive to the number of variables we included as redictors, to account for the likelyhood that some variables will show a correlation by chance. Such as adjusted R^2, or the AIC. The anova() test is also sensitive to the number of predictors in the models, so it is not easy to fool by adding a bunch of random data either.

summary(mod_house_train)$adj.r.squared
summary(mod_house_rand_train)$adj.r.squared

# you can compare AIC
# it will usually choose the smaller model
AIC(mod_house_train)
AIC(mod_house_rand_train)

# models can be compared with anova, which is sensitive to the number of predictors
anova(mod_house_train, mod_house_rand_train)



## Result-based models selection

# (don't do this at home)
# After seeing the performance of mod_house_rand_train, and not knowing that it contains random variables, one might be tempted to build a model with only the predictors that were identified as having a significant added predictive value, to improve the model fit indices (e.g. adjusted R^2 or AIC). And that would acieve exactly that: it would result in the indcrease of the indexes, but not the actual prediction efficiency, so the better indexes would be just an illusion resulting from the fact that we have "hidden" from the statistical tests, that we have tried to use a lot of predictors in a previous model.
# Excluding variables that seem "useless" based on the results will blind the otherwise sensitive measures of model fit. This is what happens when using automated model selection procedures, such as backward regression.
# In the example below we use backward regression. This method first fits a complete model with all of the specified predictors, and then determins which predictor has the smallest amount of unique added explanatory value to the model, and excludes it from the list of predictors, refitting the model without this predictor. This procedure is iterated until until there is no more predictor that can be excluded without significantly reducing model fit, at which point the process stops. 

# backward regression
mod_back_train = step(mod_house_rand_train, direction = "backward")


anova(mod_house_train, mod_back_train)

summary(mod_house_train)$adj.r.squared
summary(mod_back_train)$adj.r.squared

AIC(mod_house_train)
AIC(mod_back_train)



# All of the above model comparison methods indicate that the backward regression model (mod_back_train) performs better. We know that this model can't be too much better than the smaller model, since it only contains a number of randomly generated variables in addition to the two predictors in the smaller model. So if we would only rely on these numbers, we would be fooled to think that the backward regression model is better.


### Testing performance on the test set

# A surefire way of determining actual model performance is to test it on new data, data that was not used in the "training" of the model. Here, we use the set aside test set to do this.
# Note that we did not re-fit the models on the test set, we use the models fitted on the training set to make our predictions using the predict() function on the test_set!!!
pred_test <- predict(mod_house_train, test_set)
pred_test_back <- predict(mod_back_train, test_set)

# now we calculate the sum of squared residuals 
RSS_test = sum((test_set[,"price"] - pred_test)^2)
RSS_test_back = sum((test_set[,"price"] - pred_test_back)^2)
RSS_test
RSS_test_back
# error is larger in the backward regression model


# the same happens if you pre-test correlation of predictors with the outcome,
# and only include predictors that are significantly correlated with the outcome.
# that way, you are overfitting your model to your training data


# BOTTOM LINE: model selection should be done pre-analysis, based on previous evidence
# post-hoc result-driven model/predictor selection can lead to overfitting
# the only good test of a model's true performance is to test
# the accuracy of its predictions on new data (or a set-asid test set)






