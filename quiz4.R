require(caret)

# Question 1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)
rf_model <- train(y ~ ., data=vowel.train, method='rf')
gbm_model <- train(y ~ ., data=vowel.train, method='gbm', verbose = FALSE)
rf_pred <- predict(rf_model, vowel.test)
gbm_pred <- predict(gbm_model, vowel.test)

confusionMatrix(rf_pred, vowel.test$y)$overall['Accuracy']
confusionMatrix(gbm_pred, vowel.test$y)$overall['Accuracy']
results_agree <- rf_pred == gbm_pred
confusionMatrix(rf_pred[results_agree], 
                vowel.test$y[results_agree])$overall['Accuracy']

# Question 2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)
rf_model <- train(diagnosis ~ ., data=training, method='rf')
set.seed(62433)
gbm_model <- train(diagnosis ~ ., data=training, method='gbm', verbose=FALSE)
set.seed(62433)
lda_model <- train(diagnosis ~ ., data=training, method='lda')
rf_pred <- predict(rf_model, testing)
gbm_pred <- predict(gbm_model, testing)
lda_pred <- predict(lda_model, testing)
confusionMatrix(rf_pred, testing$diagnosis)$overall['Accuracy']
confusionMatrix(gbm_pred, testing$diagnosis)$overall['Accuracy']
confusionMatrix(lda_pred, testing$diagnosis)$overall['Accuracy']

stacked_df <- data.frame(rf = rf_pred, gbm=gbm_pred, lda=lda_pred, 
                         truth=testing$diagnosis)
set.seed(62433)
stacked_model <- train(truth ~ ., stacked_df, method='rf')
stacked_pred <- predict(stacked_model, stacked_df)
confusionMatrix(stacked_pred, stacked_df$truth)$overall['Accuracy']


# Question 3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)
lasso_model <- train(CompressiveStrength ~ ., data=training, method='lasso')
plot.enet(lasso_model$finalModel, xvar='penalty', use.color = TRUE)

# Question 4
library(lubridate)  # For year() function below
require(forecast)
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
testest <- ts(testing$visitsTumblr)
model <- bats(tstrain)
pred <- forecast(model, h=length(testest), level=95)
accuracy(pred, testing$visitsTumblr)
sum(testing$visitsTumblr <= pred$upper) / nrow(testing)


# Question 5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
require(e1071)
set.seed(325)
model <- svm(CompressiveStrength ~ ., data=training)
model
pred = predict(model, testing)
RMSE = sqrt(sum((pred - testing$CompressiveStrength)^2))


