#
# Driven Data Competition: From Fog Nets to Neural Nets
#   (https://www.drivendata.org/competitions/9/)
#
# Author: Niklas Berliner
#
# Use only Microclimate data 2 hour intervall
#

library(dplyr)
library(forecast)
library(randomForest)

folder <- '/path/data/'

# Test set: Microclimate (2h intervals)
fname             <- paste(folder, 'fb38df29-e4b7-4331-862c-869fac984cfa.csv', sep='')
df.test           <- read.csv(fname, header=TRUE)
colnames(df.test) <- c('time', colnames(df.test)[-1])
df.test$time <- as.POSIXct(strptime(df.test$time, "%Y-%m-%d %H:%M:%S"), tz='UTC')

# Training set: Microclimate (2h intervals)
fname              <- paste(folder, 'eaa4fe4a-b85f-4088-85ee-42cabad25c81.csv', sep='')
df.train           <- read.csv(fname, header=TRUE) 
colnames(df.train) <- c('time', colnames(df.train)[-1])
df.train$time <- as.POSIXct(strptime(df.train$time, "%Y-%m-%d %H:%M:%S"), tz='UTC')
df.train <- df.train[-c(1:4),]

## Remove leafwet460_min, too many missing values
df.train$leafwet460_min <- NULL
df.test$leafwet460_min  <- NULL

## Add the submission format time to the test set to be sure we catch everything
fname <- paste(folder, 'submission_format.csv', sep='/')
df.submission_format <- read.csv(fname)
colnames(df.submission_format) <- c('time', 'submission')
df.submission_format$time      <- as.POSIXct(strptime(df.submission_format$time, "%Y-%m-%d %H:%M:%S"), tz='UTC')

df.test_extended <- full_join(df.test, df.submission_format, by='time')
df.test_extended$submission <- ifelse(is.na(df.test_extended$submission), FALSE, TRUE)


## Combine the data
df.train$submission <- FALSE
df <- rbind(df.test_extended, df.train)
df <- df %>% arrange(as.numeric(df$time))


## Split the data into "chunks" for ARIMA modelling
chunks     <- na.chunks(df.train, df.test, df.submission_format)
idx_na     <- chunks$idx_na
idx_blocks <- chunks$idx_blocks


## I noticed there are 11 temp values at zero which seem to be artifacts. They are set to NA and predicted
idx_temp     <- which(df$temp == 0)
idx_humidity <- which(df$humidity == 0)

all(idx_temp == idx_humidity) # this does seem as if the measurement was wrong

## More general, the temperature does in general not seem to drop below 2 degrees. It only does so near the 0 values. We'll remove them and will try to predict them
idx_set_na <- which(df$temp < 2)
df[idx_set_na, c('temp', 'humidity')] <- NA




# Now we use Sidi data to predict some missing values
df <- predict.sidi(df, df.sidi)



## Assemble the blocks and put them in a list
chunks <- apply(idx_blocks, 2, function(x) return(df[x[1][[1]]:x[2][[1]], ]))

## Use first block
for (i in 1:length(chunks)) {
  df.chunk <- chunks[[i]]
 
  # Make sure we're really dealing with equi distant time series 
  if (i == 4) {
    df.forecast <- df.chunk[-c(1:19), ]
    idx_offset <- 19
  } else if (i == 6) {
    df.forecast <- df.chunk[-c(1:474), ]
    idx_offset <- 474
  } else if (i == 8) {
    df.forecast <- df.chunk[-c(1:363), ]
    idx_offset <- 363
  } else {
    df.forecast <- df.chunk
    idx_offset  <- 0
  }
  
  stopifnot(all(diff(df.forecast$time) == 2)) # sanity check to ensure that there are no gaps
  
  h           <- nrow(df.forecast) - nrow(na.omit(df.forecast))
#   idx         <- which(is.na(df.chunk$temp)) # we can use any column since all are missing
#   df.forecast <- na.omit(df.forecast)
  
  if (h > 0) {
    for (j in 3:(ncol(df.chunk)-1)) { # time, pericp_mm and submissions do not need to be predicted
      x <- df.forecast[,j]

      fit <- auto.arima(x) # fit the model
      kr  <- KalmanRun(x, fit$model) # thanks to: http://stats.stackexchange.com/a/104606
      
      id.na <- which(is.na(x))
      pred  <- sapply(id.na, FUN = function(x, Z, alpha) Z %*% alpha[x,], 
             Z = fit$model$Z, alpha = kr$states)
      
      df.chunk[(id.na+idx_offset),j] <- pred
    }
  }
  
  ## Done, replace the chunk
  chunks[[i]] <- df.chunk
}



## Asseble the new data.frame, remember that this won't have data for the first
## entries which also need to be submitted. We will set them to zero at the end.
df <- rbind_all(chunks)

## For now I don't know why, there remain 7 rows with missing values
df$humidity <- na.interp(df$humidity)
df$temp <- na.interp(df$temp)
df$leafwet450_min <- na.interp(df$leafwet450_min)
df$leafwet_lwscnt <- na.interp(df$leafwet_lwscnt)

stopifnot(!any(is.na(df)))

## Add time variable to capture seasonality
df$week <- as.numeric(strftime(df$time, format='%U', tz='UTC'))

## Add lagged variables
df$lag_temp     <- c(rep(mean(df$temp), 12), diff(df$temp, lag=12)) # lag temperature 1 day, i.e. 24h
df$lag_humidity <- c(rep(mean(df$humidity), 12), diff(df$humidity, lag=12)) # lag humdidity 1 day

## Add the dew temperature
## Define function to compute the dew point temperature
gamma <- function(temp, RH, b, c) {
  x <- log(RH) + ((b*temp)/(c+temp))
  return(x)
}

dew.point <- function(temp, RH, a=6.112, b=17.67, c=243.5) {
  g <- gamma(temp, RH, b, c)
  temp <- (c*g) / (b-g)
  return(temp)
}

df$Td <- dew.point(df$temp, df$humidity)
# df$Td[is.na(df$Td)] <- 0.0001

## Add the ratio between temperature and dew point
df$temp_Td_ratio <- df$temp / df$Td



### Build a mixture model of randomForest and gam

## Add the target variable
fname               <- paste(folder, 'a0f785bc-e8c7-4253-8e8a-8a1cd0441f73.csv', sep='')
df.target           <- read.csv(fname)
colnames(df.target) <- c('time', 'yield')
df.target$time      <- as.POSIXct(strptime(df.target$time, "%Y-%m-%d %H:%M:%S"), tz='UTC')

df.model <- left_join(df, df.target, by='time')

## Split the data
df.model_submission <- df.model[df.model$submission == TRUE,-c(10,16)]
df.model_modelling  <- df.model[df.model$submission == FALSE,-c(10)]



## Do a 9-fold cross validation leaving out 4 subsequent days of a month
days <- as.numeric(strftime(df.model[df.model$submission == FALSE,]$time, format='%d', tz='UTC'))

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
  idx <- cv_sets[[i]]
  df.model_train <- df.model_modelling[-idx, ]
  df.model_test  <- df.model_modelling[idx, ]
  
  model <- mixture.model(df.model_train, df.model_test)
  
  rmse_test  <- predict_fog_nets(df.model_test,  model$rf_model, model$gam_model, rmse=TRUE)
  rmse_train <- predict_fog_nets(df.model_train, model$rf_model, model$gam_model, rmse=TRUE)
 
  df.cv[,i] <- c(rmse_test, rmse_train)
}

print(sprintf("rmse test:  %.2f", mean(unlist(df.cv['test',]))))
print(sprintf("rmse train: %.2f", mean(unlist(df.cv['train',]))))
print(df.cv)



## Build the final model
model <- mixture.model(df.model_modelling, df.model_modelling[1:10,])

## Check that the rmse is roughly the same as bevore
predict_fog_nets(df.model_modelling,  model$rf_model, model$gam_model, rmse=TRUE)


## Get predictions for the submission
pred_time       <- df.model[df.model$submission == TRUE,c(1)]
pred_submission <- predict_fog_nets(df.model_submission, model$rf_model, model$gam_model)

pred_submission <- data.frame('time'=pred_time,
                              'yield'=pred_submission)

## Add the non-predicted entries
pred_submission <- left_join(df.submission_format, pred_submission, by='time')
pred_submission$submission <- NULL
pred_submission$yield[is.na(pred_submission$yield)] <- 0

fname <- paste(folder, 'Submission_07.csv', sep='/')
write.table(pred_submission, fname, row.names=FALSE, sep=',')




