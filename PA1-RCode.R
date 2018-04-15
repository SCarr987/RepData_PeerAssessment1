# source PA1.R CH5W2 - Project 1


# packages
library(ggplot2)
library(data.table)
library(lubridate)

# directory structure and get source data
if(!file.exists("./data")){dir.create("./data")}

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
if(!file.exists('./data/data.zip')){download.file(fileUrl,destfile = './data/data.zip')} #store zip data 
unzip('./data/data.zip', exdir = 'data') # Unzip file and put in data directory

dir("./data/")                      # read names of files - data file is activity.zip

activity <- fread('./data/activity.csv', data.table = TRUE)  # returns a data table

# inspect data
head(activity); tail(activity); str(activity)  # interval is clock time HHMM in 5 min increments


# Deliverable 1 total steps per day
actdata <- activity[!is.na(activity$steps),]   # strip the NA's
totsteps <- tapply(actdata$steps,actdata$date,sum) # find sum per day

## Display histogram, mean and median 
hist(totsteps, col = 'lightblue', 
      main = 'Activity Monitoring Device \nDaily Steps', xlab = 'Total Number of Steps', ylab = 'Frequency')
      abline(v=mean(totsteps), col = 'blue', lty = 1)     
      abline(v=median(totsteps), col = 'darkblue', lty=3)  
      mtext(expression(italic('NA removed ')), side = 3, adj = 1, line = -4)

      legend('topright', legend = c('mean','median'), 
             col = c('blue', 'darkblue'),  lty = c(1,3))
      
## Capture mean and median
meanNoNA <-  round(mean(totsteps),0)
mediNoAN <-  round(median(totsteps),0)

      
# Deliverable 2 average daily activity pattern for number of steps within a particular time interval
intsteps <- tapply(actdata$steps,actdata$interval,mean) # find average per interval
plot(names(intsteps),intsteps, type = 'l',
      main = 'Activity Monitoring Device \nSteps at Time Interval', xlab = 'Time Interval (HHMM)', ylab = 'Average Steps')

      timemax <- names(intsteps[which.max(intsteps)])
      valmax <- round(max(intsteps),0)   
     
      ## Enhance readability of text comments
      mtext(c(paste('maximum:',valmax,'steps at ',timemax,'time ')), side = 3, adj = 1, line = -2)
      mtext(expression(italic('NA removed ')), side = 3, adj = 1, line = -4)

      
# Deliverable 3 Imputing missing values
## Explore data
      sum(is.na(activity$steps))   #2304
      ## look for patterns in missing data prior to imputing missing values
      actdata <- activity[is.na(activity$steps),]
      tabNA <-table(actdata$date)   # eight dates are missing data, 24*60/5 = 288 measurements

## Create new dataset with imputed values from mean for 5 minute interval
      actimp <- activity # copy initial dataset since small number of na

      ## Replace NA by row with means found in part 2
      for (i in 1:nrow(actimp)) {
            if(is.na(actimp$steps[i])) {
                  actimp$steps[i] <- intsteps[which(actimp$interval[i]==as.numeric(names(intsteps)))]
            }
      }
      
      ## Recalculate data with imputed information
      totsteps <- tapply(actimp$steps,actimp$date,sum) # find sum per day
      
 ## Display histogram, mean and median 
      hist(totsteps, col = 'lightgreen', 
           main = 'Activity Monitoring Device \nDaily Steps', xlab = 'Total Number of Steps', ylab = 'Frequency')
            abline(v=mean(totsteps), col = 'blue', lty = 1)     
            abline(v=median(totsteps), col = 'darkblue', lty=3) 
            mtext(expression(italic('NA imputed ')), side = 3, adj = 1, line = -4)
      
      legend('topright', legend = c('mean','median'), 
             col = c('blue', 'darkblue'),  lty = c(1,3))
      
## Capture median and mean
      meanimp <- round(mean(totsteps),0)
      mediimp <- round(median(totsteps),0)     
      
      # the mean and median with na, 'r meanleg' and 'r medileg' have the same values
      # after imputing with the average number of steps by interval, 'r meanlegimp' and 'r medilegimp'
      
      
# Deliverable 4 - comparison of activity patterns on weekdays and weekends
## Add a factor column for type of day - weekend or weekday
      ##  transform date to posix
      actimp$date <- ymd(actimp$date)
      
      ## logic to categorize days - Sat and Sun begin with S
      daytype <- function(x) {if(substr(weekdays(x),1,1)=="S")  'weekend' else 'weekday' }
      
      ## add the factor column for type of day 
      actimp$dayt <- as.factor(sapply(actimp$date, daytype))
      
      ## make a time series of the data comparing weekend and weekday 
      totsteps <- aggregate(steps ~ interval+dayt, actimp, mean)
 
## Make a panel plot compring weekday to weekend          
      plt <- ggplot(totsteps, aes(interval, steps)) +
            geom_line(stat = "identity", aes(colour = dayt)) +
            theme_gray() +
            facet_grid(dayt ~ ., scales="fixed", space="fixed") +
            labs(x="Time Interval (HHMM)", y=expression("Average Number of Steps")) +
            ggtitle("Activity Monitoring Device Daily Steps")
      print(plt)  
      
## Calcuate mean steps for each case
      meansteps <- tapply(actimp$steps,actimp$dayt,mean)    
      meanweekday <- round(as.numeric(meansteps[1]),0)
      meanweekend <- round(as.numeric(meansteps[2]),0)
      
      
      