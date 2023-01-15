``` yaml
---
title: "PA1_template"
author: "Hasitha N"
date: "2023-01-13"
output:
  html_document: default
      keep_md: true
---
```



## Loading and processing the data


```r
data <- read.csv("C:\\Users\\14084\\OneDrive\\Documents\\Python Scripts\\repdata_data_activity\\activity.csv"
               , header = TRUE)
data$date <- as.Date(data$date, "%Y-%m-%d")
```


```r
#Ignore the missing values in the dataset 

data2 <- data[!is.na(data$steps),] 
summary(data2)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-02   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-29   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-30   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-16   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-29   Max.   :2355.0
```

## What is mean total number of steps taken per day?


```r
#Compute the total number of steps per day 

data2.total <- aggregate(steps ~ date, data=data2, FUN=sum)
head(data2.total)
```


```total
#Display histogram of the total number of steps
hist(data2.total$steps,xlab="Steps", main = "Total Number of Steps per Day")

```


```r
#Show mean and median 
mean(data2.total$steps) 
```

```
## [1] 10766.19
```

```r
median(data2.total$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
#Compute average number of steps per interval 
data3 <- data2 %>% group_by(interval) %>% summarize(avg_steps=mean(steps))
str(data3)
```

```
## tibble [288 × 2] (S3: tbl_df/tbl/data.frame)
##  $ interval : int [1:288] 0 5 10 15 20 25 30 35 40 45 ...
##  $ avg_steps: num [1:288] 1.717 0.3396 0.1321 0.1509 0.0755 ...
```


```plot
#Plot using line chart 
plot(x=data3$interval, y=data3$avg_steps,
type="l", xlab = "Interval", ylab="Average Number of Steps", main =
"Average Number of Steps Across all Dates per Interval")

```


```r
#Show the rowdata containing the maximum number of steps
data3[which.max(data3$avg_steps),]
```

## Imputing missing values


```r
#Only steps column contains NAs 
summary(data)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

```r
#Count NA rows in steps column 
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
#Create copy of original dataset 
data4 <- data

#Join with data which already contains the average steps per interval
data4.imp <- inner_join(x = data4, y = data3, by="interval")
summary(data4.imp)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304                                          
##    avg_steps      
##  Min.   :  0.000  
##  1st Qu.:  2.486  
##  Median : 34.113  
##  Mean   : 37.383  
##  3rd Qu.: 52.835  
##  Max.   :206.170  
## 
```

```r
#Get the mean value for those with null steps
data4.imp[is.na(data4.imp$steps),"steps"] <- data4.imp[is.na(data4.imp$steps),]$avg_steps

#Now there are no NAs
summary(data4.imp)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##    avg_steps      
##  Min.   :  0.000  
##  1st Qu.:  2.486  
##  Median : 34.113  
##  Mean   : 37.383  
##  3rd Qu.: 52.835  
##  Max.   :206.170
```

```r
anyNA(data4.imp$steps)   
```

```
## [1] FALSE
```

```r
#output should show FALSE
```


```r
# Calculate total for the histogram

data4.imp.total <- aggregate(steps ~ date, data=data4.imp, FUN=sum)

# Show histogram

hist(data4.imp.total$steps, xlab="Steps", main = "Total Number of Steps per Day with Imputed Values")
```

(figure/histogram-1.png)

```r
# Compute the mean and median

summary(data4.imp.total)
```

```
##       date                steps      
##  Min.   :2012-10-01   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 9819  
##  Median :2012-10-31   Median :10766  
##  Mean   :2012-10-31   Mean   :10766  
##  3rd Qu.:2012-11-15   3rd Qu.:12811  
##  Max.   :2012-11-30   Max.   :21194
```

```r
mean(data4.imp.total$steps) 
```

```
## [1] 10766.19
```

```r
median(data4.imp.total$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?


```r
data4.imp[weekdays(data4.imp$date, TRUE) %in% c("Sat", "Sun"), "day"] <-  "weekend"
data4.imp[!weekdays(data4.imp$date, TRUE) %in% c("Sat", "Sun"), "day"] <-  "weekday"

data5 <- data4.imp %>% group_by(interval,day) %>% summarize(avg_steps2=mean(steps)) %>% select(interval, avg_steps2, day)
```

```
## `summarise()` has grouped output by 'interval'. You can
## override using the `.groups` argument.
```

```r
graph <-  ggplot(data5, aes(interval,avg_steps2)) + geom_line() + 
  ggtitle("Weekend vs Weekday Average Steps per Interval") + 
  labs(x="Interval", y="Average Number of Steps")
graph + facet_grid( day ~ . ) 
```

(figure/data4-1.png)


