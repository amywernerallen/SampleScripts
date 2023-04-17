
#import data
setwd("~/Desktop/")
snoo = read.csv("SNOO_data.csv")
library('lubridate')
library('hms')
library('ggplot2')
library('scales')
library('hrbrthemes')
library('dplyr')
library('reshape2')
library("viridis") 
library(RColorBrewer)

##############################################################

################### DATA CLEANUP AND MANIP

snoo$start_date = as.Date(snoo$start_date, format="%m/%d/%y")

#put duration in hours
### duration is currently in seconds; divide by 60x60
snoo$duration_hrs = snoo$duration/3600

#identify nighttime sleep
### define nighttime sleep between 8PM and 8:30AM

snoo$start_time = format(as_datetime(hms(snoo$start_time)), format = "%H:%M:%S")
snoo$end_time = format(as_datetime(hms(snoo$end_time)), format = "%H:%M:%S")

snoo$nighttime = 0
snoo$nighttime[snoo$start_time > "20:00:00"] = 1
snoo$nighttime[snoo$end_time < "08:30:00"] = 1

#adjust startdate
### if nighttime sleep, attribute to prior day

snoo$adj_start_date = snoo$start_date
snoo$adj_start_date = ymd(snoo$adj_start_date)
snoo$adj_start_date[snoo$end_time < "08:30:00"] = ymd(snoo$start_date[snoo$end_time < "08:30:00"])-days(1)

#calculate week given birth date
### birth date is 12/28
snoo$week = difftime(snoo$adj_start_date, as.Date("2022-12-28", format="%Y-%m-%d"), units = c("weeks"))
snoo$week = floor(as.numeric(snoo$week))+1

################### DATA SUMMARY

nighttime = snoo[snoo$nighttime==1,]
nighttime = nighttime[nighttime$week!=13,]

# average wakeups per night

nightsleep_sum = nighttime %>%
  group_by(adj_start_date, week) %>%
  summarise(total_sleep = sum(duration_hrs),
            avg_sleep_stretch = mean(duration_hrs),
            max_sleep_stretch = max(duration_hrs),
            wakings = sum(nighttime)-1)

nightsleep_sum$wakings = as.numeric(nightsleep_sum$wakings)
nightsleep_sum = as.data.frame(nightsleep_sum)
nightsleep_sum$adj_total_sleep = nightsleep_sum$total_sleep-0.16*nightsleep_sum$wakings

nightsleep_wk = nightsleep_sum %>%
  group_by(week) %>%
  summarise(avg_total = mean(total_sleep),
            stddev_total = sqrt(sd(total_sleep)),
            avg_adj_total = mean(adj_total_sleep),
            avg_sleep_stretch = mean(avg_sleep_stretch), 
            max_sleep_stretch = mean(max_sleep_stretch),
            avg_wakings = mean(wakings),
            stddev_wakings = sqrt(sd(wakings)))

# average total sleep
df2 = melt(nightsleep_wk, id.vars='week')
df2 = df2 %>% filter(variable == "avg_adj_total" | variable == "avg_total")
levels(df2$variable) = c("Average Total", "", "Average Adjusted Total", "", "", "", "") 

ggplot(df2, aes(x=week, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  xlab("week") + ylab("") +
  scale_x_continuous(breaks=seq(0,14,1)) +
  scale_y_continuous(breaks=seq(0,12,2)) +
  geom_hline(yintercept = 8, color="red") +
  ggtitle("Average Total Sleep per Night") +
  theme(axis.title.y=element_text(angle=90, vjust=2)) +
  scale_fill_brewer(palette = "Paired")+
  theme(plot.margin = unit(c(1,1,1,1), "cm"), legend.position="bottom")


# average wakeups per night
ggplot(data = nightsleep_wk, 
       mapping = aes(x = week, group=1)) + 
  geom_col(aes(y = avg_wakings, fill = "average wakeups")) +
  scale_fill_manual(name = "", values = c("average wakeups" = "#8856a7")) +
  geom_errorbar(aes(ymin=avg_wakings-stddev_wakings, ymax=avg_wakings+stddev_wakings), width=.2,
                position=position_dodge(.9), alpha = 0.3, color = "dark blue") +
  xlab("week") + ylab("") +
  scale_x_continuous(breaks=seq(0,14,1)) +
  geom_hline(yintercept = 3, color="yellow") +
  ggtitle("Average Wakeups per Night") +
  theme(axis.title.y=element_text(angle=90, vjust=2)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"), legend.position="bottom")

# stretches of sleep
df2 = melt(nightsleep_wk, id.vars='week')
df2 = df2 %>% filter(variable == "avg_sleep_stretch" | variable == "max_sleep_stretch")
levels(df2$variable) = c("", "", "", "Avg Sleep Stretch", "Max Sleep Stretch", "", "") 

ggplot(df2, aes(x=week, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  xlab("week") + ylab("") +
  scale_x_continuous(breaks=seq(0,14,1)) +
  #geom_hline(yintercept = 8, color="red") +
  ggtitle("Sleep Stretches") +
  theme(axis.title.y=element_text(angle=90, vjust=2)) +
  scale_fill_manual(values=c("#832388","#E3436B"))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"), legend.position="bottom")

# time of wakeups
nighttime$wakeup = hour(hms(nighttime$end_time))

nightsleep_wakeup = nighttime %>%
  group_by(wakeup) %>%
  summarise(total = n())


ggplot(nightsleep_wakeup, aes(x = wakeup, y = total, fill=total)) + 
  geom_bar(stat='identity')+
  coord_polar(start = -0.11, direction=1) + 
  ggtitle("Wakeups by time of day") + 
  scale_fill_continuous(low = 'blue', high = 'red') +
  scale_x_continuous("", breaks = seq(0, 24), labels = seq(0, 24))
