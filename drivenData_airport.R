#
# Driven Data Competition: From Fog Nets to Neural Nets
#   (https://www.drivendata.org/competitions/9/)
#
# Author: Niklas Berliner
#
# Prepare the airport data
#

folder <- '/path/data/'

# Load the Agadir Airport Data
fname               <- paste(folder, 'e384729e-3b9e-4f53-af1b-8f5449c69cb7.csv', sep='')
df.agadir           <- read.csv(fname, header=TRUE)
colnames(df.agadir) <- c('time', colnames(df.agadir)[-1])
df.agadir$time      <- as.POSIXct(strptime(df.agadir$time, "%Y-%m-%d %H:%M:%S"), tz='WET')
attributes(df.agadir$time)$tzone <- "UTC"


df.agadir$ff10 <- NULL # too many missing values
df.agadir$WW   <- NULL
df.agadir$W.W. <- NULL
df.agadir$c    <- NULL # for now drop the cloud coverage
df.agadir$DD   <- NULL
df.agadir$P0   <- NULL

## Convert the visibility to numeric
df.agadir$VV <- as.character(df.agadir$VV)
df.agadir[ df.agadir$VV == '10.0 and more', 'VV'] <- '10.0'
df.agadir[ df.agadir$VV == '', 'VV'] = '-1'
df.agadir$VV <- as.numeric(df.agadir$VV)

colnames(df.agadir) <- c('time', paste0('agadir_', colnames(df.agadir)[-c(1)]))


## Sample the data onto the time intervall of the microclimate data
period_min <- as.POSIXct(strptime('2013-11-23 16:00:00', "%Y-%m-%d %H:%M:%S"), tz='UTC')
period_max <- as.POSIXct(strptime('2016-01-04 10:00:00', "%Y-%m-%d %H:%M:%S"), tz='UTC')

df.data <- data.frame('time'=seq(period_min, period_max, 60*5)) # the 5min intervall
df.agadir <- left_join(df.data, df.agadir, by='time')


col.names <- paste('agadir', colnames(df.agadir), sep='_')
colnames(df.agadir) <- c('time', col.names[-1])




## Load the Guelmim Airport data
fname                <- paste(folder, '41d4a6af-93df-48ab-b235-fd69c8e5dab9.csv', sep='')
df.guelmim           <- read.csv(fname, header=TRUE)
colnames(df.guelmim) <- c('time', colnames(df.guelmim)[-1])
df.guelmim$time      <- as.POSIXct(strptime(df.guelmim$time, "%Y-%m-%d %H:%M:%S"), tz='WET')
attributes(df.guelmim$time)$tzone <- "UTC"

df.guelmim$ff10 <- NULL # too many missing values
df.guelmim$WW   <- NULL
df.guelmim$W.W. <- NULL
df.guelmim$c    <- NULL # for now drop the cloud coverage
df.guelmim$DD   <- NULL


## Convert the visibility to numeric
df.guelmim$VV <- as.character(df.guelmim$VV)
df.guelmim[ df.guelmim$VV == '10.0 and more', 'VV'] <- '10.0'
df.guelmim[ df.guelmim$VV == '', 'VV'] = '-1'
df.guelmim$VV <- as.numeric(df.guelmim$VV)

df.guelmim$P0 <- NULL

colnames(df.guelmim) <- c('time', paste0('guelmim_', colnames(df.guelmim)[-c(1)]))



## Sample the data onto the time intervall of the microclimate data
period_min <- as.POSIXct(strptime('2013-11-23 16:00:00', "%Y-%m-%d %H:%M:%S"), tz='UTC')
period_max <- as.POSIXct(strptime('2016-01-04 10:00:00', "%Y-%m-%d %H:%M:%S"), tz='UTC')

#df.data <- data.frame('time'=seq(period_min, period_max, 2*60*60)) # the two hour intervall
df.data <- data.frame('time'=seq(period_min, period_max, 60*5)) # the 5min intervall
df.guelmim <- left_join(df.data, df.guelmim, by='time')




