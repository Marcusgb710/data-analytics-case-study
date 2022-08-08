
library("tidyverse")
library("readr")
library("lubridate")



#assign data to variables
quarter_2_2019 <- read_csv("Data/Divvy_Trips_2019_Q2.csv")
quarter_3_2019 <- read_csv("Data/Divvy_Trips_2019_Q3.csv")
quarter_4_2019 <- read_csv("Data/Divvy_Trips_2019_Q4.csv")
quarter_1_2020 <- read_csv("Data/Divvy_Trips_2020_Q1.csv")

#Look at column names to identify differences
colnames(quarter_2_2019)
colnames(quarter_3_2019)
colnames(quarter_4_2019)
colnames(quarter_1_2020)

#Rename column names so they are all similar

quarter_2_2019 <- rename( quarter_2_2019
       ,ride_id="01 - Rental Details Rental ID"
       ,rideable_type="01 - Rental Details Bike ID"
       ,started_at="01 - Rental Details Local Start Time"
       ,ended_at="01 - Rental Details Local End Time"
       ,start_station_name="03 - Rental Start Station Name"
       ,start_station_id="03 - Rental Start Station ID"
       ,end_station_name="02 - Rental End Station Name"
       ,end_station_id="02 - Rental End Station ID" 
       ,member_casual="User Type" 
         )
quarter_3_2019 <- rename( quarter_3_2019
  ,ride_id=trip_id
    ,rideable_type=bikeid
    ,started_at=start_time
    ,ended_at=end_time
    ,start_station_name=from_station_name
    ,start_station_id=from_station_id
    ,end_station_name=to_station_name
    ,end_station_id=to_station_id
    ,member_casual=usertype 
)
quarter_4_2019 <- rename( quarter_4_2019
  ,ride_id=trip_id
  ,rideable_type=bikeid
  ,started_at=start_time
  ,ended_at=end_time
  ,start_station_name=from_station_name
  ,start_station_id=from_station_id
  ,end_station_name=to_station_name
  ,end_station_id=to_station_id
  ,member_casual=usertype 
)


#Mutate data types so they stack correctly

quarter_2_2019 <- mutate(quarter_2_2019, ride_id = as.character(ride_id))
quarter_3_2019 <- mutate(quarter_3_2019, ride_id = as.character(ride_id))
quarter_4_2019 <- mutate(quarter_4_2019, ride_id = as.character(ride_id))
quarter_2_2019 <- mutate(quarter_2_2019, rideable_type = as.character(rideable_type))
quarter_3_2019 <- mutate(quarter_3_2019, rideable_type = as.character(rideable_type))
quarter_4_2019 <- mutate(quarter_4_2019, rideable_type = as.character(rideable_type))


#create a new data frame where we combine them all based on column name
all_trips <- bind_rows(quarter_2_2019, quarter_3_2019, quarter_4_2019, quarter_1_2020)
View(all_trips)
colnames(all_trips)

#Maintane data integrity by dropping sensitive, old, ore irrelevant data
all_trips <- all_trips %>% 
  select(-c("01 - Rental Details Duration In Seconds Uncapped", "05 - Member Details Member Birthday Year", "start_lat", "end_lat", "start_lng", "end_lng", "gender", "Member Gender", "birthyear", "tripduration"))
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)


#mutate the data so the values in the member_casual column are uniform
all_trips <- all_trips %>% 
  mutate(member_casual=recode(member_casual, "Subscriber"="member", "Customer"="casual"))

 
#create new columns based on Datetime and another column calculating the difference of ride times to get the duration
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$started_at), "%m")
all_trips$day <- format(as.Date(all_trips$started_at), "%d")
all_trips$year <- format(as.Date(all_trips$started_at), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$started_at), "%A")
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at, units="secs")


#create time breaks so we can categorize data based on a time of day and create a new column for it
breaks <- hour(hm("00:00", "05:00", "12:00", "18:00", "23:59"))
labels <- c("Night", "Morning", "Afternoon", "Evening")
all_trips$time_of_day <- cut(x=hour(all_trips$started_at), breaks=breaks, labels=labels, include.lowest=TRUE)

#order the days from numbers to there names and convert the timestamp column into a time object
all_trips$day_of_week <- ordered(all_trips$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
all_trips$time_stamp <- hms::as_hms(all_trips$started_at)

is.factor(all_trips$ride_length)
#convert the ride_length column to a numberic value for easy calculation
all_trips$ride_length <- as.numeric(all_trips$ride_length)
View(all_trips$ride_length)

#create a new dataframe where we remove rows based on if the bike was serviced at HQ
all_trips_v2 <- subset(all_trips, ride_length > 0 & start_station_name != "HQ QR")

#conduct some analysis
mean(all_trips_v2$ride_length)
median(all_trips_v2$ride_length)
max(all_trips_v2$ride_length)
min(all_trips_v2$ride_length)

summary(all_trips_v2$ride_length)

#create separate subsets of data for uncluttered analysis

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=min)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN=mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN=mean)

aggregate(all_trips_v2$member_casual ~ all_trips_v2$month, FUN=max)
aggregate(all_trips_v2$member_casual ~ all_trips_v2$month, FUN=min)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$time_stamp, FUN=mean)

all_trips_v2 %>% 
  group_by(member_casual, start_station_name) %>% 
  summarise(number_of_rides=n(), average_duration=mean(ride_length), month_name=month.abb[month]) %>% 
  arrange(start_station_name)

all_trips_v2 %>% 
  group_by(member_casual, end_station_name) %>% 
  summarise(number_of_rides=n(), average_duration=mean(ride_length)) %>% 
  arrange(member_casual)

all_trips_v2 %>% 
  group_by(start_station_name, member_casual) %>% 
  summarise(number_of_trips=n(), average_trip_duration=mean(ride_length)) %>% 
  arrange(start_station_name, member_casual) %>% 
  summary()

all_trips_v2 %>% 
  group_by(year, member_casual) %>% 
  summarise(number_of_rides=n(), average_ride_length=mean(ride_length)) %>% 
  arrange(year, member_casual)


all_trips_v2 %>% 
  mutate(time_hms=hms::as_hms(time)) %>% 
  group_by(member_casual, time_hms) %>% 
  summarise(average_duration=mean(ride_length), number_of_subscriptions=n(), average_time=mean(time_hms)) %>% 
  arrange(month)

#brief visualization to better understand data
all_trips_v2 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_subscriptions=n()) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x=month, y=number_of_subscriptions, fill=member_casual)) +
  geom_col(position="dodge")


all_trips_v2 %>% 
  mutate(weekday=wday(started_at, label=TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides=n(), average_duration=mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y=average_duration, fill=member_casual)) +
  geom_col(position="dodge")

all_trips_v2 %>% 
  mutate(weekday=wday(started_at, label=TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides=n(), average_duration=mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y=number_of_rides, fill=member_casual)) +
  geom_col(position="dodge")





  
#create subsets of the data for further analysis in tableau 

time_df <- all_trips_v2 %>% 
  group_by(member_casual, time_of_day) %>% 
  summarise(number_of_subscriptions=n(), average_trip_duration=mean(ride_length)) %>% 
  arrange(time_of_day, member_casual)

rides_per_month <- all_trips_v2 %>% 
  mutate(month=month.abb[as.numeric(month)]) %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_subscriptions=n(), average_trip_duration=mean(ride_length)) %>% 
  arrange(member_casual, month) 
comp<- all_trips_v2 %>% 
  group_by(time_stamp, member_casual) %>% 
  summarise(avg_ride_l=mean(ride_length), number_of_rides=n())

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN=mean)
comp <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$time_stamp, FUN=mean)

write.csv(rides_per_month, file="C:/Users/marcu/Desktop/datacasestudy/Clean Data/rides_per_month.csv")
write.csv(time_df, file="C:/Users/marcu/Desktop/datacasestudy/Clean Data/avg_trip_during_day.csv")
write.csv(counts, file="C:/Users/marcu/Desktop/datacasestudy/Clean Data/avg_ride_length.csv")
write.csv(comp, file="C:/Users/marcu/Desktop/datacasestudy/Clean Data/ride_length_comparison2.csv")

