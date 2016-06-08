#
# Driven Data Competition: From Fog Nets to Neural Nets
#   (https://www.drivendata.org/competitions/9/)
#
# Author: Niklas Berliner
#
# Cross-validation scheme
#

library(lazyeval)

rmse.compute <- function(yield_true, yield_pred) {
  r <- sqrt(sum((yield_true-yield_pred)**2, na.rm=TRUE)/length(yield_true))
  return(r)
}


cross.validate <- function(model.function, df, predict_label) {
  #
  # Do cross-validation
  #
  
  ## Generate the missing day chunks
  days <- as.numeric(strftime(df$time, format='%d', tz='UTC'))
  
  idx_1 <- which(days %in% c(1,2,3,4))
  idx_2 <- which(days %in% c(4,5,6,7))
  idx_3 <- which(days %in% c(7,8,9,10))
  idx_4 <- which(days %in% c(10,11,12,13))
  idx_5 <- which(days %in% c(13,14,15,16))
  idx_6 <- which(days %in% c(17,18,19,20))
  idx_7 <- which(days %in% c(20,21,22,23))
  idx_8 <- which(days %in% c(24,25,26,27))
  idx_9 <- which(days %in% c(28,29,30,31))
  
  cv_sets <- list(idx_1, idx_2, idx_3, idx_4, idx_5, idx_6, idx_7, idx_8, idx_9)
  
  df.cv <- data.frame(row.names = c('test', 'train'))
  
  for (i in 1:length(cv_sets)) {
    print(sprintf("Computing cv %d", i))
    
    idx      <- cv_sets[[i]]
    df_train <- df[-idx, ]
    df_test  <- df[idx, ]
    
    model <- model.function(df_train)
    
    pred_train <- predict(model, df_train)
    pred_test  <- predict(model, df_test)
    
    true_train <- dplyr::select_(df_train, interp(~matches(var), var=as.character(predict_label)))
    true_test  <- dplyr::select_(df_test,  interp(~matches(var), var=as.character(predict_label)))
    
    rmse_test  <- rmse.compute(true_train[,1], pred_train)
    rmse_train <- rmse.compute(true_test[,1], pred_test)
    
    df.cv[,i] <- c(rmse_test, rmse_train)
  }
  
  return(df.cv)
  
}