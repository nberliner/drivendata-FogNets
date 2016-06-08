#
# Driven Data Competition: From Fog Nets to Neural Nets
#   (https://www.drivendata.org/competitions/9/)
#
# Author: Niklas Berliner
#
# Generate the chunks
#

library(forecast)


na.chunks <- function(df.train, df.test, df.submission_format) {
  
  ## For the rest, we will be very simplistic
  df.train$gusts_ms       <- na.interp(df.train$gusts_ms)
  df.train$wind_dir       <- na.interp(df.train$wind_dir)
  df.train$wind_ms        <- na.interp(df.train$wind_ms)
  df.train$percip_mm      <- na.interp(df.train$percip_mm)
  df.train$humidity       <- na.interp(df.train$humidity)
  df.train$temp           <- na.interp(df.train$temp)
  df.train$leafwet450_min <- na.interp(df.train$leafwet450_min)
  df.train$leafwet_lwscnt <- na.interp(df.train$leafwet_lwscnt)
  
  
  df.test_extended <- full_join(df.test, df.submission_format, by='time')
  df.test_extended$submission <- ifelse(is.na(df.test_extended$submission), FALSE, TRUE)
  
  
  ## Combine the data
  df.train$submission <- FALSE
  df <- rbind(df.test_extended, df.train)
  df <- df %>% arrange(as.numeric(df$time))
  
  ## Split the data into "chunks" for ARIMA modelling
  na_idx       <- ifelse(rowSums(is.na(df)) > 0, 1, 0)
  na_idx       <- diff(na_idx)
  idx_start_na <- which(na_idx == 1)
  idx_end_na   <- which(na_idx == -1)
  
  # Each column is one na block or one block with NA at the end
  idx_na     <- mapply(function(x, y) return(list(x, y)), c(1, idx_start_na+1), idx_end_na)
  idx_blocks <- mapply(function(x, y) return(list(x, y)), idx_end_na+1, c(idx_end_na[-1], nrow(df)))
  
  return(setNames(list(idx_na, idx_blocks), c('idx_na', 'idx_blocks')))
}