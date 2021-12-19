#install packages
#tidyverse for data wrangling
#lubridate for data
#ggplot2 for data visualisation
install.packages("tidyverse")

library(tidyverse)
library(lubridate)
library(ggplot2)
getwd()
setwd()

#1.COLLECT DATA
#Upload data here
Divvy_Trips_2020_Q1 <- read.csv("~/Divvy_Trips/Divvy_Trips_2020_Q1.csv")
Divvy_Trips_2019_Q2 <- read.csv("~/Divvy_Trips/Divvy_Trips_2019_Q2", header=False)
Divvy_Trips_2020_Q3 <- read.csv("~/Divvy_Trips/Divvy_Trips_2020_Q3.csv")
Divvy_Trips_2020_Q4 <- read.csv("~/Divvy_Trips/Divvy_Trips_2020_Q4.csv")


#2.WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#Compare column names
colnames(Divvy_Trips_2020_Q1)
colnames(Divvy_Trips_2019_Q2)
colnames(Divvy_Trips_2019_Q3)
colnames(Divvy_Trips_2019_Q4)
#colnames for dataset Divvy_Trips_2019_Q4,Divvy_Trips_2019_Q2 & Divvy_Trips_2019_Q3 match
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file
#Rename columns  to make them consisent with q1_2020 
Divvy_Trips_2019_Q4<-rename(Divvy_Trips_2019_Q4,
                            ride_id=trip_id,
                            rideable_type=bikeid,
                            started_at=start_time,
                            ended_at = end_time,  
                            ,start_station_name = from_station_name 
                            ,start_station_id = from_station_id 
                            ,end_station_name = to_station_name 
                            ,end_station_id = to_station_id 
                            ,member_casual = usertype)
                            
Divvy_Trips_2019_Q3 <- rename(Divvy_Trips_2019_Q3,
                              ride_id = trip_id,
                              rideable_type = bikeid,
                              started_at = start_time,
                              ended_at = end_time,
                              start_station_name = from_station_name,
                              start_station_id = from_station_id,
                              end_station_name = to_station_name,
                              end_station_id = to_station_id,
                              member_casual = usertype)
Divvy_Trips_2019_Q2 <- rename(Divvy_Trips_2019_Q2
                  ,ride_id = X01...Rental.Details.Rental.ID
                  ,rideable_type = X01...Rental.Details.Bike.ID 
                  ,started_at = X01...Rental.Details.Local.Start.Time  
                  ,ended_at = X01...Rental.Details.Local.End.Time  
                  ,start_station_name = X03...Rental.Start.Station.Name 
                  ,start_station_id = X03...Rental.Start.Station.ID
                  ,end_station_name = X02...Rental.End.Station.Name 
                  ,end_station_id = X02...Rental.End.Station.ID
                  ,member_casual = User.Type)
# Inspect the dataframes and look for incongruencies
str(Divvy_Trips_2020_Q1)
str(Divvy_Trips_2019_Q2)
str(Divvy_Trips_2019_Q3)
str(Divvy_Trips_2019_Q4)
# Convert ride_id and rideable_type to character so that they can stack correctly
Divvy_Trips_2019_Q4 <-  mutate(Divvy_Trips_2019_Q4, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
Divvy_Trips_2019_Q3 <-  mutate(Divvy_Trips_2019_Q3, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
Divvy_Trips_2019_Q2 <-  mutate(Divvy_Trips_2019_Q2, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

all-trips<-rbind(Divvy_Trips_2020_Q1,Divvy_Trips_2019_Q2,Divvy_Trips_2019_Q3,Divvy_Trips_2019_Q4)                                              
#rbind combines columns that match.
#use bind_rows instead
all_trips<-bind_rows(Divvy_Trips_2020_Q1,Divvy_Trips_2019_Q2,Divvy_Trips_2019_Q3,Divvy_Trips_2019_Q4)
# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%  
  select(-c("start_lat","start_lng","end_lat","end_lng","birthyear", "gender","X01...Rental.Details.Duration.In.Seconds.Uncapped","X05...Member.Details.Member.Birthday.Year","Member.Gender","tripduration"))

# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics
# There are a few problems we will need to fix:
# (1) In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). We will need to consolidate that from four to two labels.
# (2) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (3) We will want to add a calculated field for length of ride since the 2020Q1 data did not have the "tripduration" column. We will add "ride_length" to the entire dataframe for consistency.
# (4) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.

# In the "member_casual" column, replace "Subscriber" with "member" and "Customer" with "casual"
# Before 2020, Divvy used different labels for these two types of riders ... we will want to make our dataframe consistent with their current nomenclature
# N.B.: "Level" is a special property of a column that is retained even if a subset does not contain any values from a specific level
# Begin by seeing how many observations fall under each usertype
table(all_trips$member_casual)
# Reassign to the desired values (we will go with the current 2020 labels)
all_trips <-all_trips %>% 
  mutate(member_casual = recode(member_casual,"Subscriber"="member","Customer"="casual"))
# Check to make sure the proper number of observations were reassigned
table(all_trips$member_casual)

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
# more on date formats in R found at that link
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
# Add a "ride_length" calculation to all_trips (in seconds)
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
# N.B.: This file location is for a Mac. If you are working on a PC, change the file location accordingly (most likely "C:\Users\YOUR_USERNAME\Desktop\...") to export the data. You can read more here: https://datatofish.com/export-dataframe-to-csv-in-r/
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = '~/Desktop/Divvy_Exercise/avg_ride_length.csv')

