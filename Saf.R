
library(lubridate)
library(scales)
library(ggplot2)
library(dplyr)
require(ggplot2)
# load the data into R
remove(list = objects())
df <- read.csv("C:/datasci_course_materials-master/assignment6/sanfrancisco_incidents_summer_2014.csv")

# data manipulation
df$Date = as.POSIXct(df$Date, format="%m/%d/%Y")
df$Year = year(df$Date) 
df$Month = month(df$Date)
df$Day = day(df$Date)
df$Time = as.POSIXct(df$Time, format="%H:%M")
df$Hour = hour(df$Time)
df$Minute = minute(df$Time)

# data manipulation test
proper_case <- function(x) {
  return (gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2" , x, perl = TRUE))
}

df <- df %>% mutate(Category = proper_case(Category),
                    Descript = proper_case(Descript),
                    PdDistrict = proper_case(PdDistrict),
                    Resolution = proper_case(Resolution))
df %>% head(10)

#***************************************************************************************************************	 
# time series: average counts by time of day
#*************************************************************************************************************** 

 df_dayCounts <- df %>%
     group_by(Date) %>% 
     summarize(count = n()) %>%
     arrange(Date)
 ggplot(df_dayCounts, aes(x = Date, y = count)) + geom_point(colour = "red") + 
     geom_line(colour = "green", size = 1.5) + 
     theme_light(base_size = 12) + xlab("Date") + ylab("Count of indicents") + 
     ggtitle("The number of incidents in Summer 2014 of San Francisco") + 
     theme(plot.title=element_text(size=16))
 ggplot(hour_avg_counts, aes(x = Hour, y = count)) + geom_point(colour = "red") + 
     geom_line(colour = "green", size = 1.5) + 
     theme_light(base_size = 12) + xlab("Time of day") + ylab("Count of incidents") + 
     scale_x_continuous(breaks=c(0:23)) + 
     ggtitle("The average number of incidents by time of day") + 
     theme(plot.title = element_text(size = 16))

#***************************************************************************************************************
# histogram: average counts by time of day
#***************************************************************************************************************

 # average counts per hour
 daily_group = group_by(df, Month, Day, Hour)
 day_hour_counts = summarise(daily_group, count = n())
 hour_group = group_by(day_hour_counts, Hour)
 hour_avg_counts = summarise(hour_group, count = mean(count))
 ggplot(hour_avg_counts, aes(x = Hour, y = count)) + 
     geom_bar(position = "dodge", stat = "identity", fill = "#FF9933") +
     theme_light(base_size = 12) + labs(x = "Time of day & Night", y = "Count of Incidents") + 
     scale_x_continuous(breaks=c(0:23)) + 
     ggtitle("The average number of incident by time of day & Night") + 
     theme(plot.title = element_text(size = 16))
	 
	 
#scatter plot
ggplot(df, aes(x = PdDistrict, y =Hour )) + geom_point(aes(colour = factor(Category)), size = 1.25) + 
theme_light(base_size = 10) + xlab("X") + ylab("Y") +
ggtitle("The number of incidents: Hour vs. Police District") + theme(plot.title=element_text(size = 16))
#***************************************************************************************************************
#                                                Hour vs. PdDistrict, Heat Map
#***************************************************************************************************************
hourly_group = group_by(df, PdDistrict, Month, Day, Hour)
district_day_hour_counts = summarise(hourly_group, count = n())
district_hourly_group = group_by(district_day_hour_counts, PdDistrict, Hour)
district_hour_avg_counts = summarise(district_hourly_group, count = mean(count))

ggplot(district_hour_avg_counts, aes(x = Hour, y = PdDistrict)) + 
  geom_tile(aes(fill = count)) + 
  scale_fill_gradient(name = "Average counts", low = "white", high = "green") +
  scale_x_continuous(breaks=c(0:23)) + 
  theme(axis.title.y = element_blank()) + theme_light(base_size = 10) + 
  theme(plot.title = element_text(size = 16)) + 
  ggtitle("The number of incidents: Hour vs. PdDistrict") 
#***************************************************************************************************************
#							The number of incidents: PdDistrict vs. Category, Heat Map
#***************************************************************************************************************
category_group = group_by(df, Month, Day, PdDistrict, Category)
day_district_category_counts = summarise(category_group, count = n())
district_category_group = group_by(day_district_category_counts, PdDistrict, Category)
district_category_avg_counts = summarise(district_category_group, count = mean(count))

ggplot(district_category_avg_counts, aes(x = PdDistrict, y = Category)) + 
    geom_tile(aes(fill = count)) + 
    scale_fill_gradient(name="Average counts", low="white", high="green") +
    theme(axis.title.y = element_blank()) + theme_light(base_size = 10) + 
    theme(plot.title = element_text(size = 16)) + 
    ggtitle("The number of incidents: PdDistrict vs. Category") + 
    theme(axis.text.x = element_text(angle = 45,size = 8, vjust = 0.5))