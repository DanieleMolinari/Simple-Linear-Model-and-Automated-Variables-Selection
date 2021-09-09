library(readr)
library(dplyr)
library(GGally)
library(olsrr)
library(ggfortify)
library(tidyverse)
library(splines)

Housing <- read.csv("housing.csv") #load data
Housing #checking data and type variables
glimpse(Housing) 
Housing$bath <- as.factor(Housing$bath) #bath can be a discrete variable
glimpse(Housing) #there are now 2 discrete varables
ggpairs(Housing) #checking the relationship beteewn response and potential potential predictors

ggplot(Housing, aes(x = price)) + #checking the distrubustion on response variable
  geom_histogram(bins=50,fill="blue",color="white") +
  labs(x = "Price", y = "Count")

Housing.lm <- lm(price~., data = Housing) #model with all data as they are
summary(Housing.lm)
autoplot(Housing.lm, which = c(1:4))

car::outlierTest(Housing.lm) #checking outliers

Housing <- Housing[-348,] #removing outliers

ggpairs(Housing) #checking data without outliers

ggplot(Housing, aes(x = price)) + #checking distribution of response varaible without outliers
  geom_histogram(bins=50,fill="blue",color="white") +
  labs(x = "Prce", y = "Count")

Housing.lm <- lm(price~., data = Housing) # model with all data minus the outliers
summary(Housing.lm)
autoplot(Housing.lm, which = c(1:4))

log.Housing.lm <- lm(log(price)~., data = Housing) #cheking model with logaitmic variables
summary(log.Housing.lm)
autoplot(log.Housing.lm, which = c(1:4))

ggplot(Housing, aes(x = log(price))) + #cheking the distribution of the logaritmic response variable
  geom_histogram(bins=50,fill="blue",color="white") +
  labs(x = "Prce", y = "Count")

Housing_test <- Housing[c(5, 97, 100, 222, 315, 453), ] #subtracting few observations for model checking
Housing <- Housing[-c(5, 97, 100, 222, 315, 453), ]

Housing.lm <- lm(price~., data = Housing) #final model with all the varaibles
summary(Housing.lm)
autoplot(Housing.lm, which = c(1:4))

#the following plots are for a better panoramic on the relationship between the response variable and all the single predictors
ggplot(Housing, aes(elevation, price)) +
  geom_point() +
  xlab("Elevation") +
  ylab("Price")

ggplot(Housing, aes(dist_am1, price)) +
  geom_point() +
  xlab("Distance to Amenity 1") +
  ylab("Price")

ggplot(Housing, aes(dist_am2, price)) +
  geom_point() +
  xlab("Distance to Amenity 2") +
  ylab("Price")

ggplot(Housing, aes(dist_am3, price)) +
  geom_point() +
  xlab("Distance to Amenity 3") +
  ylab("Price")

ggplot(Housing, aes(bath, price)) +
  geom_boxplot() +
  xlab("Number of baths") +
  ylab("Price")

ggplot(Housing, aes(sqft, price)) +
  geom_point() +
  xlab("Square footage of the house") +
  ylab("Price")

ggplot(Housing, aes(parking, price)) +
  geom_boxplot() +
  xlab("Parking type") +
  ylab("Price")

ggplot(Housing, aes(precip, price)) +
  geom_point() +
  xlab("Amount of precipitation") +
  ylab("Price")

#model selections
step(Housing.lm) #the lowest AIC value is for the model with only bath as predictor which I created further down
#but it is not so different from other models and actually R-Squared value is not the highest.

backwards.AIC <- ols_step_backward_aic(Housing.lm)
backwards.AIC
summary(backwards.AIC$model)

forward.AIC <- ols_step_forward_aic(Housing.lm)
forward.AIC
summary(forward.AIC$model)

#creating different models using varaibles based on results of model selections
model1 <- lm(price~sqft+parking+bath, data = Housing)
summary(model1)
confint(model1)
ggcoef(model1, vline_color = "red", vline_linetype = "solid", errorbar_color = "blue", errorbar_height = 0.25, exclude_intercept = TRUE)
#looking at the confidence intervals, bath is a very good predictor and parking should be taken under cosideration.
autoplot(model1, which = c(1:4)) #the errors has a constant variance and the mean is near zero. Normality is questionable though, but this is common for all the models considered.
anova(model1)

model2 <- lm(price~sqft*parking+bath, data = Housing)
summary(model2)
confint(model2)
ggcoef(model2, vline_color = "red", vline_linetype = "solid", errorbar_color = "blue", errorbar_height = 0.25, exclude_intercept = TRUE)
#iin this model, it seems that only bath is a good predictor because it is the only one which does not contain zero in the interval.
autoplot(model2, which = c(1:4))
anova(model2)

model3 <- lm(price~sqft+bath, data = Housing)
summary(model3)
autoplot(model3, which = 1:4)
anova(model3)

#from the single plots vs price I noticed some correlation with dist_am2 and dist_am3, but from the following analysis 
#they don't actually seem to be very good predictors
model4 <- lm(price~sqft+dist_am2+dist_am3+parking+bath, data = Housing)
summary(model4)
confint(model4)
ggcoef(model4, vline_color = "red", vline_linetype = "solid", errorbar_color = "blue", errorbar_height = 0.25, exclude_intercept = TRUE)
autoplot(model4, which = c(1:4))
anova(model4)

model5 <- lm(price~sqft+dist_am2+dist_am3*parking+bath, data = Housing)
summary(model5)
confint(model5)
ggcoef(model5, vline_color = "red", vline_linetype = "solid", errorbar_color = "blue", errorbar_height = 0.25, exclude_intercept = TRUE)
autoplot(model5, which = c(1:4))
anova(model5)

model_bath <- lm(price~bath, data = Housing)
summary(model_bath)
autoplot(model_bath, which = c(1:4))
anova(model_bath)

model_no_factors <- lm(price~sqft, data = Housing)
summary(model_no_factors)
autoplot(model_no_factors)
anova(model_no_factors)

log_model1 <- lm(log(price)~log(sqft)+parking+bath, data = Housing)
summary(log_model1)
confint(log_model1)
ggcoef(log_model1, vline_color = "red", vline_linetype = "solid", errorbar_color = "blue", errorbar_height = 0.25, exclude_intercept = TRUE)
autoplot(log_model1, which = c(1:4))
anova(log_model1)

#cheking non linear model using polinomial regression
model7 <- lm(price~poly(sqft, 2)+bath+parking, data = Housing)
summary(model7)
autoplot(model7, which = 1:4)
anova(model7)

#cheking non linear models using splines
model8 <- lm(price~bs(sqft, df = 6)+bath+parking, data = Housing)
summary(model8)
autoplot(model8, which = 1:4)
anova(model8)

model9 <- lm(price~ns(sqft, df = 6)+bath+parking, data = Housing)
summary(model9)
autoplot(model9, which = 1:4)
anova(model9)

#cheking how well the prediction is of models chosen
model1_test <- lm(price~sqft+parking+bath, data = Housing_test)
fit_model1_test <- predict(model1_test, interval = "confidence")
model1_test_fit <- data.frame(Housing_test, fit_model1_test)

Price_prediction1 <- cbind(Observed = Housing_test$price, fit_model1_test)
Price_prediction1 <- as.data.frame(Price_prediction1)
cor.test(Price_prediction1$Observed, Price_prediction1$fit)

plot_fit <- ggplot(Price_prediction1, aes(x = Observed, y = fit)) +
  geom_point(colour = "blue") +
  geom_abline() +
  xlim(250000, 750000) +
  ylim(250000, 750000) +
  xlab("Observed Value") +
  ylab("Fitted Value")
plot_fit

model4_test <- lm(price~sqft+parking+bath, data = Housing_test)
fit_model4_test <- predict(model4_test, interval = "confidence")
model4_test_fit <- data.frame(Housing_test, fit_model4_test)

Price_prediction4 <- cbind(Observed = Housing_test$price, fit_model4_test)
Price_prediction4 <- as.data.frame(Price_prediction4)
cor.test(Price_prediction4$Observed, Price_prediction4$fit)

plot_fit <- plot_fit +
  geom_point(data = Price_prediction4, aes(x = Observed, y = fit), colour = "red")
plot_fit
#there is no difference between model 1 and model 4, they are both good predictor, but model 1
#has a slightly higher value of Adj. Rsquared.

#final model 
fit_model7 <- predict(model7, interval = "confidence")
model7_fit <- data.frame(Housing, fit_model7)

ggplot(data = model7_fit) +
  aes(x = sqft, y = price, colour = bath) +
  geom_point(aes(shape = parking)) +
  geom_smooth(method = "lm", formula = y~bs(x, 6))

fit_model8 <- predict(model8, interval = "confidence")
model8_fit <- data.frame(Housing, fit_model8)

ggplot(data = model8_fit) +
  aes(x = sqft, y = price, colour = bath) +
  geom_point(aes(shape = parking)) +
  geom_smooth(method = "lm", formula = y~ns(x, 6))

#moddl chosen
fit_model1 <- predict(model1, interval = "confidence")
model1_fit <- data.frame(Housing, fit_model1)

ggplot(data = model1_fit) +
  aes(x = sqft, y = price, colour = bath) +
  geom_point(aes(shape = parking)) +
  geom_smooth(method = "lm")





