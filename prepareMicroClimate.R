#
# Driven Data Competition: From Fog Nets to Neural Nets
#   (https://www.drivendata.org/competitions/9/)
#
# Author: Niklas Berliner
#
# Prepare the micro-climate data (not yet comprehensible, needs better formating and explanation)
#


library(dplyr)
library(forecast)
library(randomForest)

folder <- 'path/data/'

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

 
  
  ## Fill missing values using weather data from Sidi
  df_t <- left_join(df, df.sidi, by='time')
  df_t$week <- as.numeric(strftime(df_t$time, format='%U', tz='UTC'))
  
  df.sidi_modelling <- na.omit(df_t)
  
  
  gam_temp <- function(df) {
    gam_model <- mgcv::gam(temp ~ sidi_T + s(sidi_P) + s(sidi_U) + 
                             s(sidi_Ff, k=3) + s(sidi_N, k=3) + s(sidi_H, k=3) + 
                             s(sidi_VV, k=4) + s(sidi_Td, k=3) +
                             s(sidi_DD_degree, bs='cc', k=4) +
                             s(week, bs='cc', k=5),
                           data = df,
                           family = 'gaussian',
                           method = 'REML')
    
    return(gam_model)
  }
  
  
  ## Check the rmse
  cross.validate(gam_temp, df.sidi_modelling, 'temp') # Looks good
  
  
  
  
  
  
  
  gam_humidity <- function(df) {
    gam_model <- mgcv::gam(humidity ~ sidi_T + s(sidi_P) + s(sidi_U) + 
                             s(sidi_Ff, k=3) + s(sidi_N, k=3) + s(sidi_H, k=3) + 
                             s(sidi_VV, k=4) + s(sidi_Td, k=3) +
                             s(sidi_DD_degree, bs='cc', k=4) +
                             s(week, bs='cc', k=5),
                           data = df,
                           family = 'gaussian',
                           method = 'REML')
    
    return(gam_model)
  }
  
  ## Check the rmse
  cross.validate(gam_humidity, df.sidi_modelling, 'humidity') # Looks good
  
  
  
  gam_leafwet_lwscnt <- function(df) {
    gam_model <- mgcv::gam(leafwet_lwscnt ~ sidi_T + s(sidi_P) + s(sidi_U) + 
                             s(sidi_Ff, k=3) + s(sidi_N, k=3) + s(sidi_H, k=3) + 
                             s(sidi_VV, k=4) + s(sidi_Td, k=3) +
                             s(sidi_DD_degree, bs='cc', k=4) +
                             s(week, bs='cc', k=5),
                           data = df,
                           family = 'gaussian',
                           method = 'REML')
    
    return(gam_model)
  }
  
  ## Check the rmse
  cross.validate(gam_leafwet_lwscnt, df.sidi_modelling, 'leafwet_lwscnt') # Not amazing but has to do for now
  
  
  
  
  
  gam_leafwet450_min <- function(df) {
    gam_model <- mgcv::gam(leafwet450_min ~ sidi_T + s(sidi_P) + s(sidi_U) + 
                             s(sidi_Ff, k=3) + s(sidi_N, k=3) + s(sidi_H, k=3) + 
                             s(sidi_VV, k=4) + s(sidi_Td, k=3) +
                             s(sidi_DD_degree, bs='cc', k=4) +
                             s(week, bs='cc', k=5),
                           data = df,
                           family = 'gaussian',
                           method = 'REML')
    
    return(gam_model)
  }
  
  ## Check the rmse
  cross.validate(gam_leafwet450_min, df.sidi_modelling, 'leafwet450_min')
  
  
  
  
  
  gam_wind_ms <- function(df) {
    gam_model <- mgcv::gam(wind_ms ~ sidi_T + s(sidi_P) + s(sidi_U) + 
                             s(sidi_Ff, k=3) + s(sidi_N, k=3) + s(sidi_H, k=3) + 
                             s(sidi_VV, k=4) + s(sidi_Td, k=3) +
                             s(sidi_DD_degree, bs='cc', k=4) +
                             s(week, bs='cc', k=5),
                           data = df,
                           family = 'gaussian',
                           method = 'REML')
    
    return(gam_model)
  }
  
  ## Check the rmse
  cross.validate(gam_wind_ms, df.sidi_modelling, 'wind_ms')
  
  
  
  
  
  
  
  gam_wind_dir <- function(df) {
    gam_model <- mgcv::gam(wind_dir ~ sidi_T + s(sidi_P) + s(sidi_U) + 
                             s(sidi_Ff, k=3) + s(sidi_N, k=3) + s(sidi_H, k=3) + 
                             s(sidi_VV, k=4) + s(sidi_Td, k=3) +
                             s(sidi_DD_degree, bs='cc', k=4) +
                             s(week, bs='cc', k=5),
                           data = df,
                           family = 'gaussian',
                           method = 'REML')
    
    return(gam_model)
  }
  
  ## Check the rmse
  cross.validate(gam_wind_dir, df.sidi_modelling, 'wind_dir') # looks bad..
  
  
  
  
  
  gam_gusts_ms <- function(df) {
    gam_model <- mgcv::gam(gusts_ms ~ sidi_T + s(sidi_P) + s(sidi_U) + 
                             s(sidi_Ff, k=3) + s(sidi_N, k=3) + s(sidi_H, k=3) + 
                             s(sidi_VV, k=4) + s(sidi_Td, k=3) +
                             s(sidi_DD_degree, bs='cc', k=4) +
                             s(week, bs='cc', k=5),
                           data = df,
                           family = 'gaussian',
                           method = 'REML')
    
    return(gam_model)
  }
  
  ## Check the rmse
  cross.validate(gam_gusts_ms, df.sidi_modelling, 'gusts_ms') # looks bad..
  
  
predict.sidi <- function(df, df.sidi) {
    
    
  ## Fill missing values using weather data from Sidi
  df_t <- left_join(df, df.sidi, by='time')
  df_t$week <- as.numeric(strftime(df_t$time, format='%U', tz='UTC'))
  
  ### Predict missing values in df_t
  
  # humidity
  idx   <- which(!is.na(df_t$sidi_T) & is.na(df_t$humidity))
  model <- gam_humidity(na.omit(df_t[ ,c(1,3,11:21)]))
  
  df_t[idx, ]$humidity <- predict(model, df_t[idx, ])
  
  # temp
  idx   <- which(!is.na(df_t$sidi_T) & is.na(df_t$temp))
  model <- gam_temp(na.omit(df_t[ ,c(1,4,11:21)]))
  
  df_t[idx, ]$temp <- predict(model, df_t[idx, ])
  
  # leafwet450_min
  idx   <- which(!is.na(df_t$sidi_T) & is.na(df_t$leafwet450_min))
  model <- gam_leafwet450_min(na.omit(df_t[ ,c(1,5,11:21)]))
  
  df_t[idx, ]$leafwet450_min <- predict(model, df_t[idx, ])
  
  # leafwet_lwscnt
  idx   <- which(!is.na(df_t$sidi_T) & is.na(df_t$leafwet_lwscnt))
  model <- gam_leafwet_lwscnt(na.omit(df_t[ ,c(1,6,11:21)]))
  
  df_t[idx, ]$leafwet_lwscnt <- predict(model, df_t[idx, ])
  
  # gusts_ms
  idx   <- which(!is.na(df_t$sidi_T) & is.na(df_t$gusts_ms))
  model <- gam_gusts_ms(na.omit(df_t[ ,c(1,7,11:21)]))
  
  df_t[idx, ]$gusts_ms <- predict(model, df_t[idx, ])
  
  # wind_dir
  idx   <- which(!is.na(df_t$sidi_T) & is.na(df_t$wind_dir))
  model <- gam_wind_dir(na.omit(df_t[ ,c(1,8,11:21)]))
  
  df_t[idx, ]$wind_dir <- predict(model, df_t[idx, ])
  
  # wind_ms
  idx   <- which(!is.na(df_t$sidi_T) & is.na(df_t$wind_ms))
  model <- gam_wind_ms(na.omit(df_t[ ,c(1,9,11:21)]))
  
  df_t[idx, ]$wind_ms <- predict(model, df_t[idx, ])
  
  # percip_mm
  df_t$percip_mm[is.na(df_t$percip_mm)] <- 0
  
  df <- as.data.frame(df_t[ ,1:10])

  return(df)
}

