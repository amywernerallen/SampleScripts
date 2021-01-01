
setwd("~/Documents/sproutbabyexport")
feed = read.csv("Feeding Tracker.csv")
sleep = read.csv("Sleep Tracker.csv")
growth = read.csv("Growth Tracker.csv")
library('lubridate')
library('ggplot2')
library('scales')
library('hrbrthemes')
library('dplyr')
library('reshape2')

##############################################################
####### data cleanup
feed[is.na(feed)] <- 0
feed$total = feed$L + feed$R 
feed$total = round(feed$total/60,1)

### datetime data
feed$DateTime <- mdy_hms(paste(feed$Date, feed$Time))
feed$Date = as.Date(feed$DateTime)
feed$Time = format(feed$DateTime, format = "%H:%M:%S")
feed$Endtime = format(as_datetime(hms(feed$Time)+minutes(round(feed$total,0))), format = "%H:%M:%S")

feed = feed[,c(1,2,15,13)]
colnames(feed) = c('Date', 'StartTime', 'EndTime', 'total')

### data point errors
feed[feed$Date=="2019-12-10",]
feed$total[feed$total==140.9] = 20
feed$EndTime[feed$EndTime=="00:07:08"] = "23:59:59"

feed[feed$Date=="2019-12-16",]
feed$EndTime[feed$EndTime=="00:06:09"] = "23:59:59"

feed[feed$Date=="2020-01-01",]
feed$EndTime[feed$EndTime=="00:03:23"] = "23:59:59"

feed[feed$Date=="2020-03-05",]
feed$EndTime[feed$EndTime=="00:15:00"] = "23:59:59"

### split by month
feed$month = 12
feed$month[feed$Date < "2020-11-10"] = 11
feed$month[feed$Date < "2020-10-10"] = 10
feed$month[feed$Date < "2020-09-10"] = 9
feed$month[feed$Date < "2020-08-10"] = 8
feed$month[feed$Date < "2020-07-10"] = 7
feed$month[feed$Date < "2020-06-10"] = 6
feed$month[feed$Date < "2020-05-10"] = 5
feed$month[feed$Date < "2020-04-10"] = 4
feed$month[feed$Date < "2020-03-10"] = 3
feed$month[feed$Date < "2020-02-10"] = 2
feed$month[feed$Date < "2020-01-10"] = 1

### growth data
growth$Date = as.Date(growth$Date, format="%m/%d/%y")
growth$lb = growth$Weight/16
new.entry <- data.frame(Date = as.Date("2020-12-10", format="%Y-%m-%d"), Weight = 387, Height=0, Head = 0, lb=24.3, week=52)
growth = rbind(growth, new.entry)

growth$week = difftime(growth$Date, as.Date("2019-12-10", format="%Y-%m-%d"), units = c("weeks"))
growth$week = floor(as.numeric(growth$week))+1

growth_weight = aggregate(lb~week, growth, max)

##############################################################
#### TIMES AND DURATION

### MONTH
feed_time = aggregate(total ~ month, feed, mean)
months = month.abb[feed_time$month]
feed_time$month_abb = months

feed_count = aggregate(total ~ month, feed, length)
feed_count$total = feed_count$total/30
feed_count$month_abb = months

feed_std = aggregate(total ~ Date, feed, length)
feed_std$month = 12
feed_std$month[feed_std$Date < "2020-11-10"] = 11
feed_std$month[feed_std$Date < "2020-10-10"] = 10
feed_std$month[feed_std$Date < "2020-09-10"] = 9
feed_std$month[feed_std$Date < "2020-08-10"] = 8
feed_std$month[feed_std$Date < "2020-07-10"] = 7
feed_std$month[feed_std$Date < "2020-06-10"] = 6
feed_std$month[feed_std$Date < "2020-05-10"] = 5
feed_std$month[feed_std$Date < "2020-04-10"] = 4
feed_std$month[feed_std$Date < "2020-03-10"] = 3
feed_std$month[feed_std$Date < "2020-02-10"] = 2
feed_std$month[feed_std$Date < "2020-01-10"] = 1

feed_avg = aggregate(total ~ month, feed_std, mean)
feed_std = aggregate(total ~ month, feed_std, sd)
feed_count = aggregate(total ~ month, feed, length)
feed_std = as.data.frame(cbind(feed_std$month, feed_std$total, feed_count$total))
colnames(feed_std) = c("month", "stddev", "count")
feed_std$stderr = feed_std$stddev/sqrt(feed_std$count)
feed_count = as.data.frame(cbind(feed_avg$month, feed_avg$total, feed_std$stderr))
colnames(feed_count) = c("month", "avg", "stderr")

#line chart: avg minutes
ggplot(feed_time, aes(x=month, y=total)) +
  geom_line(color="#69b3a2", size=0.5, alpha=0.9, linetype=2) +
  scale_x_discrete(limits = month.abb) +
  ylim(0, 30)+
  xlab("") + ylab("Average Min. per Feeding\n") +
  ggtitle("Time Nursing per Session") +
  theme(axis.title.y=element_text(angle=90, vjust=5)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme_ipsum() 

#bar chart: times per day
ggplot(feed_count, aes(x=month_abb, y=total)) + 
  geom_bar(stat="identity", fill="#69b3a2") +
  xlab("") + ylab("Sessions per Day\n") +
  ggtitle("Daily Nursing Sessions") +
  theme(axis.title.y=element_text(angle=90, vjust=5)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme_ipsum() 


feed_agg = as.data.frame(cbind(feed_time$month_abb, feed_time$month, feed_time$total, feed_count$avg, feed_count$stderr))
colnames(feed_agg) = c("month_abb", "month", "duration", "times", "times_sd")
feed_agg$month = factor(feed_agg$month, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"))
feed_agg$times = as.numeric(feed_agg$times)
feed_agg$duration = as.numeric(feed_agg$duration)
feed_agg$times_sd = as.numeric(feed_agg$times_sd)

ggplot(data = feed_agg, 
       mapping = aes(x = month, group=1)) + 
  geom_col(aes(y = times, fill = "times per day")) +
  geom_errorbar(aes(ymin=times-times_sd, ymax=times+times_sd), width=.2,
                position=position_dodge(.9), alpha = 0.3, color = "dark blue") +
  geom_line(aes(y = duration, color = "min per session")) + 
  scale_fill_manual(name = "", values = c("times per day" = "#7fcdbb")) +
  scale_color_manual(name = "", values = c("min per session" = "#2c7fb8"))+
  scale_y_continuous(breaks=seq(0,25,4)) +
  xlab("Month") + ylab("") +
  ggtitle("Times per Day and Length of Sessions") +
  theme(axis.title.y=element_text(angle=90, vjust=2)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"), legend.position="bottom")

### WEEK

feed$week = difftime(feed$Date, as.Date("2019-12-10", format="%Y-%m-%d"), units = c("weeks"))
feed$week = floor(as.numeric(feed$week))+1

feed_timew = aggregate(total ~ week, feed, mean)
feed_countw = aggregate(total ~ week, feed, length)
feed_countw$total = feed_countw$total/7

feed_agg_wk = as.data.frame(cbind(feed_timew$week, feed_timew$total, feed_countw$total))
colnames(feed_agg_wk) = c("week", "duration", "times")
feed_growth = merge(feed_agg_wk, growth_weight, by=c("week"), all.x=TRUE, all.y=FALSE)

ggplot(data = feed_growth, 
       mapping = aes(x = week, group=1)) + 
  geom_col(aes(y = times, fill = "times per day")) +
  geom_line(aes(y = duration, color = "min per session")) + 
  geom_point(aes(y = lb, color = "weight (lbs)")) + 
  scale_fill_manual(name = "", values = c("times per day" = "#7fcdbb")) +
  scale_color_manual(name = "", values = c("min per session" = "#2c7fb8", "weight (lbs)" = "red"))+
  scale_y_continuous(breaks=seq(0,25,4)) +
  xlab("Week") + ylab("") +
  ggtitle("Times per Day and Length of Sessions") +
  theme(axis.title.y=element_text(angle=90, vjust=2)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"), legend.position="bottom")


##############################################################
#### DAILY VIEW
month1_3 = feed[feed$month<=3,]
month1_3$StartTime = as.POSIXct(month1_3$StartTime, format="%H:%M")
month1_3$EndTime = as.POSIXct(month1_3$EndTime, format="%H:%M")

ggplot(month1_3, aes(x=Date, y=StartTime, color=as.factor(month))) + 
  geom_linerange(aes(ymin = StartTime, ymax = EndTime),size = 2)+ coord_flip() +
  scale_x_date(breaks = date_breaks("7 days")) +
  scale_y_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  ylab("Hour of the Day") + xlab("") +
  ggtitle("First Three Months") +
  scale_color_manual(values=c("#253494", "#1d91c0", "#7fcdbb")) +
  geom_vline(xintercept = as.Date("2020-12-28 09:00:00"), color="red") +
  theme(axis.title.x=element_text(angle=0, vjust=-2)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"), legend.position="none")


month1_6 = feed[feed$month<=6,]
month1_6$StartTime = as.POSIXct(month1_6$StartTime, format="%H:%M")
month1_6$EndTime = as.POSIXct(month1_6$EndTime, format="%H:%M")

ggplot(month1_6, aes(x=Date, y=StartTime, color=as.factor(month))) + 
  geom_linerange(aes(ymin = StartTime, ymax = EndTime),size = 2)+ coord_flip() +
  scale_x_date(breaks = date_breaks("7 days")) +
  scale_y_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  ylab("Hour of the Day") + xlab("") +
  ggtitle("First Six Months") +
  scale_color_manual(values=c('#081d58','#253494', '#225ea8', '#1d91c0','#41b6c4','#7fcdbb', '#c7e9b4')) +
  geom_vline(xintercept = as.Date("2020-12-28 09:00:00"), color="red") +
  theme(axis.title.x=element_text(angle=0, vjust=-2)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"), legend.position="none")


month1_8 = feed[feed$month<=8,]
month1_8$StartTime = as.POSIXct(month1_8$StartTime, format="%H:%M")
month1_8$EndTime = as.POSIXct(month1_8$EndTime, format="%H:%M")

ggplot(month1_8, aes(x=Date, y=StartTime, color=as.factor(month))) + 
  geom_linerange(aes(ymin = StartTime, ymax = EndTime),size = 2)+ 
  scale_x_date(breaks = date_breaks("1 month")) +
  scale_y_continuous(trans = rev_date) +
  #scale_y_datetime(date_breaks = "2 hours", date_labels = "%H:%M", trans = rev_date) +
  ylab("Hour of the Day") + xlab("") +
  ggtitle("First 8 Months") +
  scale_color_manual(values=c("#832388","#912884","#9E2C80","#AC317C","#BA3577","#C83A73","#D53E6F","#E3436B"))+
  #scale_color_manual(values=c('#081d58','#253494', '#225ea8', '#1d91c0','#41b6c4','#7fcdbb', '#9ebcda', '#8c96c6')) +
  geom_vline(xintercept = as.Date("2020-12-28 09:00:00"), color="red") +
  theme(axis.text.x=element_text(angle=15, vjust=0)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"), legend.position="none")


c_trans <- function(a, b, breaks = b$breaks, format = b$format) {
  a <- as.trans(a)
  b <- as.trans(b)
  
  name <- paste(a$name, b$name, sep = "-")
  
  trans <- function(x) a$trans(b$trans(x))
  inv <- function(x) b$inverse(a$inverse(x))
  
  trans_new(name, trans, inverse = inv, breaks = breaks, format=format)
  
}
rev_date <- c_trans("reverse", "time")

##############################################################
#### TIME DIFF

feed$ds = paste(feed$Date, feed$StartTime)
feed2 <- feed %>%
  mutate(difft = difftime(ds, lag(ds)))

#feed2 = feed2[,c(1:5,9)]
feed2[is.na(feed2)] <- 0
feed2$difft = -1*feed2$difft

## MAX -- MONTH
feed_max = aggregate(difft ~ month, feed2, max)
feed_max$difft = as.numeric(feed_max$difft)

feed_max = aggregate(difft ~ Date, feed2, max)
feed_max$difft = as.numeric(feed_max$difft)
feed_max$month = 12
feed_max$month[feed_max$Date < "2020-11-10"] = 11
feed_max$month[feed_max$Date < "2020-10-10"] = 10
feed_max$month[feed_max$Date < "2020-09-10"] = 9
feed_max$month[feed_max$Date < "2020-08-10"] = 8
feed_max$month[feed_max$Date < "2020-07-10"] = 7
feed_max$month[feed_max$Date < "2020-06-10"] = 6
feed_max$month[feed_max$Date < "2020-05-10"] = 5
feed_max$month[feed_max$Date < "2020-04-10"] = 4
feed_max$month[feed_max$Date < "2020-03-10"] = 3
feed_max$month[feed_max$Date < "2020-02-10"] = 2
feed_max$month[feed_max$Date < "2020-01-10"] = 1

feed_avg_max = aggregate(difft ~ month, feed_max, mean)
colnames(feed_avg_max) = c("month", "value")
feed_avg_max$value = feed_avg_max$value/60
feed_avg_max$type = "max"
feed_avg_max$month = as.factor(feed_avg_max$month)

ggplot(feed_avg_max, aes(x=month, y=value)) + 
  geom_bar(stat="identity", fill="#69b3a2") +
  xlab("") + ylab("Sessions per Day") +
  ggtitle("Daily Nursing Sessions") +
  theme(axis.title.y=element_text(angle=90, vjust=5)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) 

## MAX -- WEEK
feed_max = aggregate(difft ~ week, feed2, max)
feed_max$difft = as.numeric(feed_max$difft)

feed_max = aggregate(difft ~ Date, feed2, max)
feed_max$difft = as.numeric(feed_max$difft)
feed_max$week = difftime(feed_max$Date, as.Date("2019-12-10", format="%Y-%m-%d"), units = c("weeks"))
feed_max$week = floor(as.numeric(feed_max$week))+1

feed_avg_max = aggregate(difft ~ week, feed_max, mean)
colnames(feed_avg_max) = c("week", "value")
feed_avg_max$value = feed_avg_max$value/60
feed_avg_max$type = "max"
feed_avg_max$month = as.factor(feed_avg_max$month)

ggplot(feed_avg_max[feed_avg_max$week<54,], aes(x=week, y=value)) + 
  geom_bar(stat="identity", fill="#c994c7") +
  xlab("Week") + ylab("Max Hours between Sessions") +
  ggtitle("Sleeping Through the Night") +
  geom_hline(yintercept = 8, color="blue", alpha=0.5, linetype = "dashed")+
  scale_x_continuous(breaks=c(1,seq(4,53,4))) +
  theme(axis.title.y=element_text(angle=90, vjust=5)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) 

##############################################################
##############################################################
#### SLEEEEEP

sleep$Start <- mdy_hms(paste(sleep$Date, sleep$Start.Time))
sleep$End <- mdy_hms(paste(sleep$Date.1, sleep$End.Time))
sleep$tdiff = difftime(sleep$End, sleep$Start, units="hours")
sleep$tdiff = as.numeric(sleep$tdiff)
sleep$type = "nap"
sleep$type[hour(sleep$Start)>18 | hour(sleep$Start)<7] = "night"
sleep = sleep[,c(-5)]
sleep$Date = as.Date(sleep$Date, "%m/%d/%y")

sleep$month = 12
sleep$month[sleep$Date < "2020-11-10"] = 11
sleep$month[sleep$Date < "2020-10-10"] = 10
sleep$month[sleep$Date < "2020-09-10"] = 9
sleep$month[sleep$Date < "2020-08-10"] = 8
sleep$month[sleep$Date < "2020-07-10"] = 7
sleep$month[sleep$Date < "2020-06-10"] = 6
sleep$month[sleep$Date < "2020-05-10"] = 5
sleep$month[sleep$Date < "2020-04-10"] = 4
sleep$month[sleep$Date < "2020-03-10"] = 3
sleep$month[sleep$Date < "2020-02-10"] = 2
sleep$month[sleep$Date < "2020-01-10"] = 1
sleep$week = difftime(sleep$Date, as.Date("2019-12-10", format="%Y-%m-%d"), units = c("weeks"))
sleep$week = floor(as.numeric(sleep$week))+1

sleep_avg = aggregate(tdiff ~ type + month, sleep, mean)
sleep_avg$month = as.factor(sleep_avg$month)
aggregate(tdiff ~ type + month, sleep, length)
sleep_avg = sleep_avg[,c(2,3,1)]
colnames(sleep_avg) = c("month", "value", "type")

sleep_num = aggregate(tdiff ~ type + Date, sleep, length)
naps = sleep_num[sleep_num$type=="nap",]
nap_avg = aggregate(tdiff ~ month, naps, mean)

ggplot(sleep_avg, aes(month, tdiff, fill=type)) +
  geom_bar(stat='Identity',position=position_dodge())

sleep_agg = rbind(sleep_avg, feed_avg_max)
sleep_agg2 = dcast(data = sleep_agg, formula = month ~ type, fun.aggregate = mean, value.var = "value")
sleep_agg2[is.na(sleep_agg2)] <- 0
sleep_agg2$nap[sleep_agg2$month==9] = 0

ggplot(data = sleep_agg2, 
       mapping = aes(x = month, group=1)) + 
  geom_col(aes(y = nap, fill = "avg naptime")) +
  geom_line(aes(y = max, color = "max interval")) + 
  scale_fill_manual(name = "", values = c("avg naptime" = "#7fcdbb")) +
  scale_color_manual(name = "", values = c("max interval" = "#2c7fb8"))+
  geom_vline(xintercept = as.factor(7), color="red")+
  xlab("Month") + ylab("") +
  ggtitle("Times per Day and Length of Sessions") +
  theme(axis.title.y=element_text(angle=90, vjust=2)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"), legend.position="bottom")

feed$type = "day"
feed$type[hour(feed$ds)>18 | hour(feed$ds)<7] = "night"
feed_day <- feed %>%
  filter(type=="day") %>%
  mutate(difft = difftime(ds, lag(ds)))
feed_day$difft = as.numeric(feed_day$difft)

feed_day_avg = aggregate(difft ~ month, feed_day, mean)



##############################################################
##############################################################
#### CALENDAR PLOT

ggplot(aes(x = .x, y = .y)) +
  geom_tile(aes(fill = Max_Counts), colour = "grey50") +
  scale_fill_viridis()
prettify(p8, label = "label", label.padding = unit(0.2, "lines"))


feed_daily = aggregate(total ~ Date, feed, sum)
calendarHeat(feed_daily$Date, feed_daily$total, ncolors = 99, color="r2b",
             varname="AMZN Adjusted Close")

p1 = ggplot_calendar_heatmap(feed_daily, 'Date', 'total',
                        dayBorderSize = 0.25, dayBorderColour = "black",
                        monthBorderSize = 2, monthBorderColour = "black",
                        monthBorderLineEnd = "round") 
p1+ 
  #xlab(NULL) + ylab(NULL) + 
  scale_fill_continuous(low = 'blue', high = 'red') + 
  theme( axis.text = element_blank(), axis.ticks = element_blank(), legend.position = 'none', strip.background = element_blank())









