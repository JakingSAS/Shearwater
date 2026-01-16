library(tidyverse)
library(dplyr)
library(reshape2)
library(lubridate)
library(fastDummies)
library(ggplot2)
library(zoo)
library(pastecs)
library(caret)

#
# 2. Code up functions to call LM, XGB and RF with different settings
# 3. Code up a function to evaluate a list of models and report on the best, based on a metric
#.   stability between training and validation
#


temp <- dives2 %>%
  select(c("Dive.Number","Depth","Average PPO2", "Water Temp", Time.s,
            `External O2 Sensor 1 (mV)`,`External O2 Sensor 2 (mV)`,`External O2 Sensor 3 (mV)`))

library(data.table)
dives3 <- melt(setDT(temp), id.vars = c("Dive.Number","Depth","Average PPO2", "Water Temp", "Time.s"), 
               variable.name = "sensor")

set.seed(987239)
train_ind <- createDataPartition(dives3$value,
                                 times = 1,
                                 p = 0.8,
                                 list = F)

#
# This section for LM
#
train_x <- dives3[train_ind, ]
#%>%
#  select(-c('value'))
test_x <- dives3[-train_ind, ]
#%>%
#  select(-c('value'))

train_y <- dives3[train_ind,] %>%
  select(c('value'))
table(train_y)
train_y

test_y <- dives3[-train_ind, ]
#%>%
#  select(c('value'))
table(test_y)
test_y

#LM.Model <- lm(value ~ Dive.Number + Depth + `Average PPO2` + `Water Temp` + Time.s +
#                 Depth*Time.s + Depth*`Water Temp` +
#                 Time.s*`Water Temp`,
#               data=train_x)
LM.Model <- lm(value ~ Dive.Number + Depth + `Average PPO2` + `Water Temp` +
                 Depth*`Water Temp`,
               data=train_x)
# Training MSE
summary(LM.Model)
mean(LM.Model$residuals^2) # 9.70414
# Test MSE
LM.Pred <- cbind(test_x, Predicted = predict(LM.Model, test_x)) %>%
  mutate(residuals = (value - Predicted))
mean(LM.Pred$residuals^2) # 10.45717


# Look at residual plots
plot(LM.Model, which=1, col=c("blue"))
plot(LM.Model, which=2, col=c("red"))
plot(LM.Model, which=3, col=c("blue"))

# basic stats on the residuals
LM.Res <- as.data.frame(LM.Model[["residuals"]]) %>%
  rename(., LM.Residuals = `LM.Model[["residuals"]]`)

stat.desc(LM.Res)

#
# This section for Random Forest
#
library(randomForest)
train_x <- dives3[train_ind, ] %>%
  select(-c(Time.s, sensor)) %>%
  rename(Average.PPO2 = `Average PPO2`,
         Water.Temp = `Water Temp`)
test_x <- dives3[-train_ind, ] %>%
  select(-c(Time.s, sensor)) %>%
  rename(Average.PPO2 = `Average PPO2`,
         Water.Temp = `Water Temp`)


RF.Model <- randomForest(value ~ ., data = train_x, ntree = 1000, mtry = 3, importance = TRUE)
#plot(RF.Model)

# Training MSE 
RF.pred <- predict(RF.Model, newdata = train_x)
RF.pred <- cbind(train_x, RF.pred) %>%
  mutate(residuals = (value - RF.pred))
mean(RF.pred$residuals^2) # 7.781605
# Test MSE 
RF.pred <- predict(RF.Model, newdata = test_x)
RF.pred <- cbind(test_x, RF.pred) %>%
  mutate(residuals = (value - RF.pred))
mean(RF.pred$residuals^2) # 16.25629


#
# This section for Gradient Boosting
#
library(xgboost)

temp <- train_x %>%
  select(-c(value))
ttest <- test_x %>%
  select(-c(value))

dtrain <- xgb.DMatrix(label = train_x$value, data = as.matrix(temp))
XGB.Model <- xgboost(data = dtrain, max.depth = 2, eta = 1, nthread = 2, nrounds = 2)
# Training MSE 
XGB.pred <- predict(XGB.Model, newdata = dtrain)
XGB.pred <- cbind(train_x, XGB.pred) %>%
  mutate(residuals = (value - XGB.pred))
mean(XGB.pred$residuals^2) # 13.94232
# Test MSE
dtest <- xgb.DMatrix(label = test_x$value, data = as.matrix(ttest))
XGB.pred <- predict(XGB.Model, newdata = dtest)
XGB.pred <- cbind(test_x, XGB.pred) %>%
  mutate(residuals = (value - XGB.pred))
mean(XGB.pred$residuals^2) # 14.50034

# Gradient Boosting with 5-fold cv
XGBcv.Model <- xgb.cv(data = dtrain, max.depth = 2, eta = 1, nthread = 2, nrounds = 10, nfold = 5, metrics = c('rmse'))
print(XGBcv.Model$best_iteration)
trained_model <- xgb.train(data = dtrain, max_depth = 2, eta = 1, nthread = 4, nrounds = 10)
# Training MSE 
XGBcv.pred <- predict(trained_model, newdata = dtrain)
XGBcv.pred <- cbind(train_x, XGBcv.pred) %>%
  mutate(residuals = (value - XGBcv.pred))
mean(XGBcv.pred$residuals^2) # 10.23121
# Test MSE
dtest <- xgb.DMatrix(label = test_x$value, data = as.matrix(ttest))
XGBcv.pred <- predict(trained_model, newdata = dtest)
XGBcv.pred <- cbind(test_x, XGBcv.pred) %>%
  mutate(residuals = (value - XGBcv.pred))
mean(XGBcv.pred$residuals^2) # 10.95912
