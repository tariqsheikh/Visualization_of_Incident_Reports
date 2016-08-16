#============================================================================================
#============================================================================================
#============================================================================================
dfset <- read.csv("C:/datasci_course_materials-master/assignment6/seattle_incidents_summer_2014.csv")

dfset <- dfset %>% mutate(Offense.Type = proper_case(Offense.Type),
                          Summarized.Offense.Description = proper_case(Hundred.Block.Location),
                          Hundred.Block.Location = proper_case(Hundred.Block.Location))
dfset %>% head(10)



get_hour <-function(x) {
  x <- as.character(x)
  return (as.numeric(strsplit(x,":")[[1]][1]))
}
dow_format <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
hour_format <- c(paste(c(12,1:11),"AM"), paste(c(12,1:11),"PM"))
# data manipulation
dfset$Date = as.POSIXct(dfset$Date, format="%m/%d/%Y")
dfset$Year = year(dfset$Date) 
dfset$Month = month(dfset$Date)
dfset$Day = day(dfset$Date)
dfset$Time = as.POSIXct(dfset$Time, format="%H:%M")
dfset$Hour = hour(dfset$Time)
dfset$Minute = minute(dfset$Time)
#============================================================================================
#how do incidents vary month to month in the Summer 2014 dataset?
#============================================================================================
df_dayCounts <- dfset %>%
    group_by(Date) %>% 
    summarize(count = n()) %>%
    arrange(Date)
ggplot(df_dayCounts, aes(x = Date, y = count)) + geom_point(colour = "red") + 
    geom_line(colour = "green", size = 1) + 
    theme_light(base_size = 12) + xlab("Date") + ylab("Count of indicents") + 
    ggtitle("The number of Crime in Summer 2014 of Seattle") + 
    theme(plot.title=element_text(size=16))
	
	
#============================================================================================
#how do incidents vary month to month in the Summer 2014 dataset?
#============================================================================================
hourly_group = group_by(dfset, Offense.Type, Month, Day, Hour)
Offense_Type_day_hour_counts = summarise(hourly_group, count = n())
Offense_Type_hourly_group = group_by(Offense.Type_day_hour_counts, Offense.Type, Hour)
Offense_Type_hour_avg_counts = summarise(Offense.Type_hourly_group, count = mean(count))

ggplot(Offense_Type_hour_avg_counts , aes(x = Hour, y = Offense.Type)) + 
    geom_tile(aes(fill = count)) + 
    scale_fill_gradient(name = "Average counts", low = "white", high = "yellow") +
    scale_x_continuous(breaks=c(0:23)) + 
    theme(axis.title.y = element_blank()) + theme_light(base_size = 10) + 
    theme(plot.title = element_text(size=16)) + 
    ggtitle("The No. of incidents per Hour as per Offense.Type")
#============================================================================================
#============================================================================================
 hourly_group = group_by(dfset, District.Sector, Month, Day, Hour)
 district_day_hour_counts = summarise(hourly_group, count = n())
 district_hourly_group = group_by(district_day_hour_counts, District.Sector, Hour)
 district_hour_avg_counts = summarise(district_hourly_group, count = mean(count))
 
 ggplot(district_hour_avg_counts, aes(x = Hour, y = District.Sector)) + 
     geom_tile(aes(fill = count)) + 
     scale_fill_gradient(name = "Average counts", low = "white", high = "green") +
     scale_x_continuous(breaks=c(0:23)) + 
     theme(axis.title.y = element_blank()) + theme_light(base_size = 10) + 
     theme(plot.title = element_text(size = 16)) + 
     ggtitle("The number of Crime: Hour vs. District.Sector") 	
#============================================================================================
#============================================================================================	 
 category_group = group_by(dfset, Month, Day, District.Sector, Offense.Type)
 day_district_category_counts = summarise(category_group, count = n())
 district_category_group = group_by(day_district_category_counts, District.Sector, Offense.Type)
 district_category_avg_counts = summarise(district_category_group, count = mean(count))
 
 ggplot(district_category_avg_counts, aes(x = District.Sector, y = Offense.Type)) + 
     geom_tile(aes(fill = count)) + 
     scale_fill_gradient(name="Average counts", low="white", high="green") +
     theme(axis.title.y = element_blank()) + theme_light(base_size = 10) + 
     theme(plot.title = element_text(size = 16)) + 
     ggtitle("The number of incidents: District.Sector vs. Offense.Type") + 
     theme(axis.text.x = element_text(angle = 45,size = 8, vjust = 0.5)) 	
	
	
	
	
	
	