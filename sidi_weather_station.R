#
# Driven Data Competition: From Fog Nets to Neural Nets
#   (https://www.drivendata.org/competitions/9/)
#
# Author: Niklas Berliner
#
# Prepare the weather station data
#
library(forecast)
library(dplyr)

## Sidi airport
folder            <- '/path/data/'

prepare_sidi_data <- function(folder) {
  #
  # Simplify the Sidi Ifni weather station data.
  #
  # The full data set can't be used direcly for the predictions. This
  # function will remove some columns and simplifiy others. Furthermore
  # it will map the times to the nearest time corresponding time present
  # in the macro-climate dataset.
  #
  # Args:
  #   folder:  The path to the folder containing all datasets
  #
  # Returns:
  #   The simplified dataset
  #
  
  ## Load the dataset
  fname             <- paste(folder, 'b57b4f6f-8aae-4630-9a14-2d24902ddf30.csv', sep='')
  df.sidi           <- read.csv(fname, header=TRUE)
  colnames(df.sidi) <- c('time', colnames(df.sidi)[-1])
  df.sidi$time      <- as.POSIXct(strptime(df.sidi$time, "%Y-%m-%d %H:%M:%S"), tz='WET')
  attributes(df.sidi$time)$tzone <- "UTC"
  
  ## Remove some columns that will not be used
  df.sidi$WW  <- NULL
  df.sidi$W1  <- NULL
  df.sidi$W2  <- NULL
  df.sidi$Cl  <- NULL
  df.sidi$Cm  <- NULL
  df.sidi$Ch  <- NULL
  df.sidi$E.  <- NULL
  df.sidi$E   <- NULL
  df.sidi$sss <- NULL
  df.sidi$Tx  <- NULL
  df.sidi$Tn  <- NULL
  df.sidi$Po  <- NULL
  df.sidi$Pa  <- NULL
  df.sidi$tR  <- NULL
  df.sidi$Tg  <- NULL
  
  df.sidi$RRR <- NULL
  df.sidi$Nh  <- NULL
  
  ## Convert factors to numeric
  df.sidi$N <- as.character(df.sidi$N)
  df.sidi$N <- ifelse(df.sidi$N == '100%', 100, df.sidi$N)
  df.sidi$N <- ifelse(df.sidi$N == '70 - 80%', 75, df.sidi$N)
  df.sidi$N <- ifelse(df.sidi$N == '90  or more, but not 100%', 95, df.sidi$N)
  df.sidi$N <- ifelse(df.sidi$N == '50%', 50, df.sidi$N)
  df.sidi$N <- ifelse(df.sidi$N == '40%', 40, df.sidi$N)
  df.sidi$N <- ifelse(df.sidi$N == '10%  or less, but not 0', 5, df.sidi$N)
  df.sidi$N <- ifelse(df.sidi$N == 'no clouds', 0, df.sidi$N)
  df.sidi$N <- ifelse(df.sidi$N == '60%', 60, df.sidi$N)
  df.sidi$N <- ifelse(df.sidi$N == '20-30%', 25, df.sidi$N)
  df.sidi$N <- as.numeric(df.sidi$N)
  
  func <- function(x) {
    result <- c()
    for (item in x) {
      if (item == '0') {
        result <- c(result, 0)
      } else if (item == "2500 or more, or no clouds.") {
        result <- c(result, 2500)
      } else {
        tmp <- unlist(strsplit(item, '-'))
        stopifnot(length(tmp) == 2)
        result <- c(result, mean(as.numeric(tmp)))
      }
    }
    return(result)
  }
  df.sidi$H <- as.character(df.sidi$H)
  df.sidi$H <- ifelse(df.sidi$H == '' & df.sidi$N == 0, 0, df.sidi$H)
  df.sidi$H <- func(df.sidi$H)
  
  ## Convert the wind direction into degreees, note there remain missing values.
  df.sidi$DD_blowing <- ifelse(df.sidi$DD == 'Calm, no wind', 0, 1)
  df.sidi$DD_degree  <- NA
  df.sidi$DD_degree  <- ifelse(df.sidi$DD == 'Wind blowing from the north', 0.0, df.sidi$DD_degree)
  df.sidi$DD_degree  <- ifelse(df.sidi$DD == 'Wind blowing from the north-northeast', 22.50, df.sidi$DD_degree)
  df.sidi$DD_degree  <- ifelse(df.sidi$DD == 'Wind blowing from the north-east', 45.0, df.sidi$DD_degree)
  df.sidi$DD_degree  <- ifelse(df.sidi$DD == 'Wind blowing from the east-northeast', 67.50, df.sidi$DD_degree)
  df.sidi$DD_degree  <- ifelse(df.sidi$DD == 'Wind blowing from the east', 90.0, df.sidi$DD_degree)
  df.sidi$DD_degree  <- ifelse(df.sidi$DD == 'Wind blowing from the east-southeast', 112.5, df.sidi$DD_degree)
  df.sidi$DD_degree  <- ifelse(df.sidi$DD == 'Wind blowing from the south-east', 135.00, df.sidi$DD_degree)
  df.sidi$DD_degree  <- ifelse(df.sidi$DD == 'Wind blowing from the south-southeast', 157.50, df.sidi$DD_degree)
  df.sidi$DD_degree  <- ifelse(df.sidi$DD == 'Wind blowing from the south', 180.00, df.sidi$DD_degree)
  df.sidi$DD_degree  <- ifelse(df.sidi$DD == 'Wind blowing from the south-southwest', 202.50, df.sidi$DD_degree)
  df.sidi$DD_degree  <- ifelse(df.sidi$DD == 'Wind blowing from the south-west', 225.00, df.sidi$DD_degree)
  df.sidi$DD_degree  <- ifelse(df.sidi$DD == 'Wind blowing from the west-southwest', 247.50, df.sidi$DD_degree)
  df.sidi$DD_degree  <- ifelse(df.sidi$DD == 'Wind blowing from the west', 270.00, df.sidi$DD_degree)
  df.sidi$DD_degree  <- ifelse(df.sidi$DD == 'Wind blowing from the west-northwest', 292.50, df.sidi$DD_degree)
  df.sidi$DD_degree  <- ifelse(df.sidi$DD == 'Wind blowing from the north-west', 315.00, df.sidi$DD_degree)
  df.sidi$DD_degree  <- ifelse(df.sidi$DD == 'Wind blowing from the north-northwest', 337.50, df.sidi$DD_degree)
  
  # Not ideal, but we created the DD_blowing variable to catch that
  df.sidi$DD_degree <- ifelse(df.sidi$DD == 'Calm, no wind', NA, df.sidi$DD_degree)
  df.sidi$DD_degree <- zoo::na.locf(df.sidi$DD_degree, na.rm=FALSE) # hopefully this will introduce fewer bias than setting to one value only.
  df.sidi$DD_degree <- zoo::na.locf(df.sidi$DD_degree, fromLast=TRUE)
  
  stopifnot(!any(is.na(df.sidi$DD_degree)))
  df.sidi$DD <- NULL # we transformed it
  
  colnames(df.sidi) <- c('time', paste0('sidi_', colnames(df.sidi)[-c(1)]))
  
  ## Resample to two hour format of the macro-climate data
  time_delta <- 120 # in minutes
  df.sidi$time_bin <- df.sidi$time + ((as.numeric(df.sidi$time)) %% (60*time_delta))
  
  df.sidi <- df.sidi %>%
    group_by(time_bin) %>%
    summarise_each(funs(mean(., na.rm=TRUE)))
  
  df.sidi$time     <- df.sidi$time_bin
  df.sidi$time_bin <- NULL
  
  ## Specify the full time period including gaps
  period_min <- as.POSIXct(strptime('2013-11-24 00:00:00', "%Y-%m-%d %H:%M:%S"), tz='UTC')
  period_max <- as.POSIXct(strptime('2016-01-04 10:00:00', "%Y-%m-%d %H:%M:%S"), tz='UTC')
  
  df.time <- data.frame('time'=seq(period_min, period_max, 2*60*60)) # the two hour intervall
  df.sidi <- left_join(df.time, df.sidi, by='time')

  return(df.sidi)
}


