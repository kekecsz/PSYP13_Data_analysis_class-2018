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


# "predicting" variability of the outcome in your original data is easy

# if you fit a model that is too flexible, you will get perfect fit on your intitial data
plot(price ~ sqft_living, data = data_house[10:30,])
lines(price[order(sqft_living)] ~ sqft_living[order(sqft_living)], data = data_house[10:30,])

# but your model will not fit reality well
# this can be tested by collecting more data, and seeing how 
# well the model predicts the new data
# or, by designating a part of your data as a "training set", fitting the 
# model on that, and testing model performance on the other part, the "test set"

# training set with a felxible model
plot(price ~ sqft_living, data = data_house[10:30,], xlim = range(data_house[10:30,"sqft_living"]), ylim = range(data_house[10:30,"price"]))
lines(price[order(sqft_living)] ~ sqft_living[order(sqft_living)], data = data_house[10:30,])
# testing model performance on test set (fits poorly)
plot(price ~ sqft_living, data = data_house[30:50,], xlim = range(data_house[10:30,"sqft_living"]), ylim = range(data_house[10:30,"price"]))
lines(price[order(sqft_living)] ~ sqft_living[order(sqft_living)], data = data_house[10:30,])

# linear regression is very inflexible, so it is less prone to overfitting

# training set with a felxible model
mod_for_plot <- lm(price ~ sqft_living, data = data_house[10:30,])
pred <- predict(mod_for_plot)
plot(price ~ sqft_living, data = data_house[10:30,], xlim = range(data_house[10:30,"sqft_living"]), ylim = range(data_house[10:30,"price"]))
lines(pred[order(data_house[10:30,"sqft_living"])] ~ data_house[10:30,"sqft_living"][order(data_house[10:30,"sqft_living"])])
# testing model performance on test set (fits OK)
pred_test <- predict(mod_for_plot, newdata = data_house[30:50,])
plot(price ~ sqft_living, data = data_house[30:50,], xlim = range(data_house[10:30,"sqft_living"]), ylim = range(data_house[10:30,"price"]))
lines(pred_test[order(data_house[30:50,"sqft_living"])] ~ data_house[30:50,"sqft_living"][order(data_house[30:50,"sqft_living"])])

# comparing residual sum of squares of the two models on the test set
RSS_felx = sum((data_house[30:50,"price"] - data_house[10:30,"price"])^2)
RSS_linreg = sum((data_house[30:50,"price"] - pred_test)^2)
RSS_linreg/RSS_felx
# the linear regression RSS is much smaller


# but the more predictors you include in the model, the more flexible it gets
# thus, overfitting becomes a problem again

# a model with the obvious predictors of house price
# NOTE that we only use the first hundred observation (half) of the dataset
# here, as a training set, so later we can test the performance 
# of our model on the other half
mod_house2_train <- lm(price ~ sqft_living + grade, data = data_house[1:100,])
summary(mod_house2_train)

# add 50 random variables to the list of predictors
rand_vars = as.data.frame(matrix(rnorm(mean = 0, sd = 1, n = 50*nrow(data_house)), ncol = 50))
data_house_withrandomvars = cbind(data_house, rand_vars)
lm_formula = as.formula(paste("price ~ sqft_living + grade + ", paste(names(rand_vars), collapse = " + "), sep = ""))

mod_house2_rand_train = lm(lm_formula, data = data_house_withrandomvars[1:100,])

# R squared increased a lot
summary(mod_house2_rand_train)

pred_train <- predict(mod_house2_train)
pred_train_rand <- predict(mod_house2_rand_train)
RSS_train = sum((data_house_withrandomvars[1:100,"price"] - pred_train)^2)
RSS_train_rand = sum((data_house_withrandomvars[1:100,"price"] - pred_train_rand)^2)
RSS_train
RSS_train_rand

# so is this model better, than the one without the random variablers?

# lets check model performance on the test set (the other half of the dataset)
# we predict the outcome (price) in the test set with the models fitted on 
# the training set
# NOTE that we did not re-fit the models on the test set, we use the models fitted
# on the training set for the prediction
pred_test <- predict(mod_house2_train, data_house_withrandomvars[101:200,])
pred_test_rand <- predict(mod_house2_rand_train, data_house_withrandomvars[101:200,])
# now we calculate the sum of squared residuals 
RSS_test = sum((data_house_withrandomvars[101:200,"price"] - pred_test)^2)
RSS_test_rand = sum((data_house_withrandomvars[101:200,"price"] - pred_test_rand)^2)
RSS_test
RSS_test_rand
# there is more error in the prediction of the model containing the random predictors


## What if I don't have a test-set?
# models can be compared with anova, which is sensitive to the number of predictors
anova(mod_house2_train, mod_house2_rand_train)

# you can compare adjusted R squared in the summary of the two models
# adjusted R squared is also sensitive to the number of predictors,
# but it is still not perfect
summary(mod_house2_train)
summary(mod_house2_rand_train)

# you can compare AIC
# it will usually choose the smaller model
AIC(mod_house2_train)
AIC(mod_house2_rand_train)


# backward regression
mod_back_train = step(mod_house2_rand_train, direction = "backward")

anova(mod_house2_train, mod_back_train)

summary(mod_back_train)

AIC(mod_house2_train)
AIC(mod_house2_rand_train)
AIC(mod_back_train)

# test the performance of backward regression model on the test set
# NOTE that we did not re-fit the models on the test set, we use the models fitted
# on the training set for the prediction
pred_test <- predict(mod_house2_train, data_house_withrandomvars[101:200,])
pred_test_back <- predict(mod_back_train, data_house_withrandomvars[101:200,])
# now we calculate the sum of squared residuals 
RSS_test = sum((data_house_withrandomvars[101:200,"price"] - pred_test)^2)
RSS_test_back = sum((data_house_withrandomvars[101:200,"price"] - pred_test_back)^2)
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
