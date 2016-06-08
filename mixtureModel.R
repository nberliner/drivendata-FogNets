#
# Driven Data Competition: From Fog Nets to Neural Nets
#   (https://www.drivendata.org/competitions/9/)
#
# Author: Niklas Berliner
#
# Mixture Model - Fog Net Competition
#


mixture.model <- function(df.model_train, df.model_test) {

  ## Build a randomForest to predict zeros
  df.rf_train       <- df.model_train[,-c(1)]
  df.rf_test        <- df.model_test[,-c(1)]
  df.rf_train$class <- as.factor(ifelse(df.rf_train$yield == 0, 1, 0)) #randomForest need factor
  df.rf_test$class  <- as.factor(ifelse(df.rf_test$yield  == 0, 1, 0))
  df.rf_train$yield <- NULL
  df.rf_test$yield  <- NULL
  df.rf_train$time  <- NULL
  df.rf_test$time   <- NULL
  
  rf_model <- randomForest(class ~ ., data = df.rf_train, xtest=df.rf_test[,-c(14)],
                           ytest=df.rf_test$class, ntree=500, mtry=6, keep.forest=TRUE)
  rf_model
  
  rf_cutoff <- 0.9 # be pretty sure that predicted zero is true zero
  
  rf_test_pred  <- predict(rf_model, df.rf_test, type='prob')
  rf_train_pred <- predict(rf_model, df.rf_train, type='prob')
  
  df.model_test$class_pred  <- ifelse(rf_test_pred[,2] >= rf_cutoff, 1, 0)
  df.model_train$class_pred <- ifelse(rf_train_pred[,2] >= rf_cutoff, 1, 0)
  
  ## For GAM modelling
  df.gam_test  <- df.model_test[df.model_test$class_pred == 0, ]
  df.gam_train <- df.model_train[df.model_train$class_pred == 0, ]
  
  # Use equal weights for each day
  days         <- as.numeric(strftime(df.gam_train$time, format='%d', tz='UTC'))
  df.days      <- data.frame('c'=1, 'day'=days)
  day_weight   <- df.days %>% group_by(day) %>% summarise(n=length(c))
  day_weight$n <- day_weight$n / mean(day_weight$n)
  day_weight   <- left_join(df.days, day_weight, by='day')
  day_weight   <- day_weight$n
  
  gam_model <- mgcv::gam(yield ~ s(percip_mm) + s(humidity, k=8) +
                           s(temp) + s(leafwet450_min) + s(temp_Td_ratio) + 
                           s(leafwet_lwscnt, k=4) + s(gusts_ms) + s(wind_dir, bs='cc', k=4) + 
                           s(wind_ms, k=20) + s(week, bs='cc', k=4) +
                           s(Td, k=6) + s(lag_temp) + s(lag_humidity),
                         data = df.gam_train,
                         family = 'gaussian',
                         weights = day_weight,
                         method = 'REML')

  return(setNames(list(rf_model, gam_model), c('rf_model', 'gam_model')))
}


predict_fog_nets <- function(df, rf_model, gam_model, rmse=FALSE, print_rmse=FALSE) {
  #
  # Predict for df, if yield is in df, a rmse will be printed
  #
  
  rmse.compute <- function(yield_true, yield_pred) {
    r <- sqrt(sum((yield_true-yield_pred)**2, na.rm=TRUE)/length(yield_true))
    return(r)
  }
  
  if ('yield' %in% colnames(df)) {
    yield <- df$yield
    df$yield <- NULL
    # print_rmse <- TRUE
  } else {
    print_rmse <-FALSE
  }
  
  ## Get the random forest prediction
  rf_pred <- predict(rf_model, df, type='prob')
  idx     <- which(rf_pred[,2] >= 0.9) # use the cutoff of .9
  
  if (print_rmse) {
    yield_true <- yield[idx]
    yield_pred <- rep(0, length(yield_true))
    r <- rmse.compute(yield_true, yield_pred)
    print(sprintf('rmse rf_model: %.3f', r))
  }
  
  ## Get the gam prediction
  gam_pred <- predict(gam_model, df[-idx,])
  
  if (print_rmse) {
    yield_pred <- c(gam_pred)
    yield_true <- yield[-idx]
    r <- rmse.compute(yield_true, yield_pred)
    print(sprintf('rmse gam_model: %.2f', r))
  }
  
  ## Assemble the prediction
  yield_pred <- rep(0, nrow(df))
  
  yield_pred[idx]  <- 0
  yield_pred[-idx] <- gam_pred
  
  ## Manually set yield with a lot of percip to zero
  idx_percip <- which(df$percip_mm > 2)
  yield_pred[idx_percip] <- 0
  
  yield_pred[yield_pred < 0] <- 0
  
  if (rmse) {
    r <- rmse.compute(yield, yield_pred)
  }
  if (print_rmse) {
    print(sprintf('rmse combined: %.2f', r))
  }
  
  if (rmse) {
    return(r)
  } else {
    return(yield_pred)
  }
}







