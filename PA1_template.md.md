To display chunknames
=====================

    #Set working directory
    #setwd("./data")

    #install packages
    #install.packages("knitr")
    #install.packages("rmarkdown")
    library(knitr)

    ## Warning: package 'knitr' was built under R version 3.5.3

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(lubridate)

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

    #Create directoty for file if it does'nt already exist
    fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    if (!file.exists("data")) {
      dir.create("data")}
    if (!file.exists("PA1_template.md")) {
      dir.create("PA1_template.md")}

    #Download data file
    #file <- download.file(fileURL, destfile = "./data/acivity.csv", method="auto")
    dateDownloaded <- date()
    dateDownloaded

    ## [1] "Wed Mar 20 12:49:09 2019"

    #List the file(s)
    list.files("./data")

    ## [1] "acivity.csv" "data"

    #Extract file from zipped file
    # File unzip verification. If the directory does not exist, unzip the downloaded file.
    if(!file.exists("./data")){
      unzip("./data", files = NULL, exdir=".")
    }

    #Read file
    activity_Data <- read.csv("activity.csv")

    #show the file (activity_Data)
    #activity_Data

    #Look at the heading
    head(activity_Data)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

    #Check the number of columns
    ncol(activity_Data)

    ## [1] 3

    #Look at the number of rows
    nrow(activity_Data)

    ## [1] 17568

    #Look at the summary
    summary(activity_Data)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

    #Look at the names of th columns
    names(activity_Data)

    ## [1] "steps"    "date"     "interval"

    #Look at the last entries of the data frame
    tail(activity_Data)

    ##       steps       date interval
    ## 17563    NA 2012-11-30     2330
    ## 17564    NA 2012-11-30     2335
    ## 17565    NA 2012-11-30     2340
    ## 17566    NA 2012-11-30     2345
    ## 17567    NA 2012-11-30     2350
    ## 17568    NA 2012-11-30     2355

    #Converte activit_Data to date class using package, zoo
    activity_Data$date <- as.Date(activity_Data$date, format = "%Y-%m-%d")
    suppressWarnings(library(zoo))

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    is.regular(activity_Data$date)

    ## [1] TRUE

    unique(activity_Data$date)

    ##  [1] "2012-10-01" "2012-10-02" "2012-10-03" "2012-10-04" "2012-10-05"
    ##  [6] "2012-10-06" "2012-10-07" "2012-10-08" "2012-10-09" "2012-10-10"
    ## [11] "2012-10-11" "2012-10-12" "2012-10-13" "2012-10-14" "2012-10-15"
    ## [16] "2012-10-16" "2012-10-17" "2012-10-18" "2012-10-19" "2012-10-20"
    ## [21] "2012-10-21" "2012-10-22" "2012-10-23" "2012-10-24" "2012-10-25"
    ## [26] "2012-10-26" "2012-10-27" "2012-10-28" "2012-10-29" "2012-10-30"
    ## [31] "2012-10-31" "2012-11-01" "2012-11-02" "2012-11-03" "2012-11-04"
    ## [36] "2012-11-05" "2012-11-06" "2012-11-07" "2012-11-08" "2012-11-09"
    ## [41] "2012-11-10" "2012-11-11" "2012-11-12" "2012-11-13" "2012-11-14"
    ## [46] "2012-11-15" "2012-11-16" "2012-11-17" "2012-11-18" "2012-11-19"
    ## [51] "2012-11-20" "2012-11-21" "2012-11-22" "2012-11-23" "2012-11-24"
    ## [56] "2012-11-25" "2012-11-26" "2012-11-27" "2012-11-28" "2012-11-29"
    ## [61] "2012-11-30"

    #Question 0
    #What is mean total number of steps taken per day?
    #Calculating the total number of steps taken per day
    colMeans(is.na(activity_Data))

    ##     steps      date  interval 
    ## 0.1311475 0.0000000 0.0000000

    steps_per_day <- aggregate(steps ~ date, rm.na = TRUE, data = activity_Data, FUN = sum)

    #Plot the histogram, missing values are not omitted
    plot(steps_per_day, type = "h", lwd = 10, lend = "square")

![](projetf_files/figure-markdown_strict/unnamed-chunk-1-1.png)

    #Calculate and report the mean and median of the total number of steps taken per day.
    m <- plot(aggregate(steps ~ date, data = activity_Data, FUN = mean))

![](projetf_files/figure-markdown_strict/unnamed-chunk-1-2.png)

    n <- plot(aggregate(steps ~ date, data = activity_Data, FUN = median))

![](projetf_files/figure-markdown_strict/unnamed-chunk-1-3.png)

    #What is the average daily activity pattern 
    #Make a time series plot (i.e. type = Ã¢ÂÂlÃ¢ÂÂ) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
    plot(aggregate(steps ~ interval, data = activity_Data, FUN = mean), type = "l")

![](projetf_files/figure-markdown_strict/unnamed-chunk-1-4.png)

    #Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
    #Using the base function max
    max(activity_Data$steps, na.rm = TRUE)

    ## [1] 806

    #806

    #Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
    sum(is.na(activity_Data))

    ## [1] 2304

    #2304

    #Create a new dataset that is equal to the original dataset but with the missing data filled in.
    activity_data <- activity_Data
    sapply(activity_data, class)

    ##     steps      date  interval 
    ## "integer"    "Date" "integer"

    ##     steps      date  interval 
    ## "integer"    "Date" "integer"
    activity_data$steps[is.na(activity_data$steps)] <- mean(na.omit(activity_Data$steps))
    activity_data$date <- as.Date(activity_data$date, format = "%Y-%m-%d")

    #Make a histogram of the total number of steps taken each day 
    #and Calculate and report the mean and median total number of steps taken per day. 
    #Do these values differ from the estimates from the first part of the assignment? 
    #What is the impact of imputing missing data on the estimates of the total daily number of steps?

    steps_per_day2 <- aggregate(steps ~ date, rm.na = TRUE, data = activity_data, FUN = sum)
    par(mfrow = c(1, 2))
    plot(steps_per_day, type = "h", lwd = 5,lend = "square", main = "With NAs")
    abline(h = seq(0, 20000, 2500), lty = "dashed")
    plot(steps_per_day2, type = "h", lwd = 5, lend = "square", main = "NAs filled")
    abline(h = seq(0, 20000, 2500), lty = "dashed")

![](projetf_files/figure-markdown_strict/unnamed-chunk-1-5.png)

    #dev.off()

    #The distribution shape seems to change and become more clustered about its mean. The mean, median, and standard deviation changed after imputing missing values.
    cbind(rbind(summary(steps_per_day$steps)[c("Mean", "Median")],
                summary(steps_per_day2$steps)[c("Mean", "Median")]), 
          data.frame(SD = c(sd(steps_per_day$steps, na.rm = TRUE), sd(steps_per_day2$steps))))

    ##       Mean   Median       SD
    ## 1 10767.19 10766.00 4269.180
    ## 2 10767.19 10767.19 3974.391

    #Are there differences in activity patterns between weekdays and weekends?
    activity_data$weekday <- factor(format(activity_data$date, "%A"))

    levels(activity_data$weekday) <- list(weekday = c("Monday", "Tuesday",
                                                  "Wednesday", "Thursday",
                                                  "Friday"), weekend =
                                        c("Saturday", "Sunday"))

    #Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
    #and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
    #See the README file in the GitHub repository to see an example of what this plot should look like 
    #using simulated data.
    par(mfrow = c(2, 1))

    with(activity_data[activity_data$weekday == "weekend",], plot(aggregate(steps ~ interval, FUN = mean), type = "l", main = "Weekends"))

    with(activity_data[activity_data$weekday == "weekday",], plot(aggregate(steps ~ interval, FUN = mean), type = "l", main = "Weekdays"))

![](projetf_files/figure-markdown_strict/unnamed-chunk-1-6.png)

    dev.off

    ## function (which = dev.cur()) 
    ## {
    ##     if (which == 1) 
    ##         stop("cannot shut down device 1 (the null device)")
    ##     .External(C_devoff, as.integer(which))
    ##     dev.cur()
    ## }
    ## <bytecode: 0x000000000749c8c0>
    ## <environment: namespace:grDevices>
