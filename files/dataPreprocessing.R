# setwd("D:/finalProjectData/RShiny/NYCTaxiVis")

# Valuable Info.
# travel_time
# passenger_number
# total_fare
# tip_amount
# address_on_map
# --Additional--
# travel_frequency/count

# Faults exists where:
# passenger -> 0
# travel_time -> 0
# distance -> 0
# fare -> 0
# fare_not_equal_total


# library(ggmap)
# library(dplyr)
# load("D:/finalProjectData/12-17-13-32.RData")
source("files/calculateDayTime.R")

yellow_june <- read.csv("data/sample.csv")

# Discarded
# yellow_june_test <- yellow_june
# yellow_july_test <- yellow_july

# Data preprocessing
# June
vec_ID <- c(1:nrow(yellow_june))
vec_pickup_time <- as.POSIXct(yellow_june$tpep_pickup_datetime)
vec_dropoff_time <- as.POSIXct(yellow_june$tpep_dropoff_datetime)
vec_travel_time_sec <- (vec_dropoff_time - vec_pickup_time) %>% as.numeric()
vec_pickup_month <- substr(yellow_june$tpep_pickup_datetime, 6, 7) %>% as.numeric() %>% as.factor()
vec_pickup_day <- substr(yellow_june$tpep_pickup_datetime, 9, 10) %>% as.numeric() %>% as.factor()
vec_pickup_hour <- substr(yellow_june$tpep_pickup_datetime, 12, 13) %>% as.numeric() %>% as.factor()
vec_dropoff_month <- substr(yellow_june$tpep_dropoff_datetime, 6, 7) %>% as.numeric() %>% as.factor()
vec_dropoff_day <- substr(yellow_june$tpep_dropoff_datetime, 9, 10) %>% as.numeric() %>% as.factor()
vec_dropoff_hour <- substr(yellow_june$tpep_dropoff_datetime, 12, 13) %>% as.numeric() %>% as.factor()
vec_pickup_weekday <- weekdays(vec_pickup_time) %>% as.factor()
levels(vec_pickup_weekday) # view first
levels(vec_pickup_weekday) <- c("mon", "tue", "wed", "fri", "sat", "sun", "thu") # change order: week_day
vec_dropoff_weekday <- weekdays(vec_dropoff_time) %>% as.factor()
levels(vec_dropoff_weekday) # view first
levels(vec_dropoff_weekday) <- c("mon", "tue", "wed", "fri", "sat", "sun", "thu") # change order: week_day
vec_isWeekend <- vec_pickup_weekday %in% c("sat", "sun")

# calculate day_time
# prerequesite: vec_isWeekend, vec_pickup_hour calculated
vec_weekend_index <- which(vec_isWeekend == TRUE)
vec_weekend_time <- vec_pickup_hour[vec_isWeekend]
vec_weekendTime <- apply(data.frame(vec_weekend_index, vec_weekend_time), 1, custom_weekend)

vec_workday_index <- which(vec_isWeekend == FALSE)
vec_workday_time <- vec_pickup_hour[!vec_isWeekend]
vec_workdayTime <- apply(data.frame(vec_workday_index, vec_workday_time), 1, custom_work_day)

yellow_june_test <- data.frame(ID = vec_ID,
                               pickup_time = vec_pickup_time, 
                               dropoff_time = vec_dropoff_time, 
                               travel_time_sec = vec_travel_time_sec, 
                               pickup_month = vec_pickup_month, 
                               pickup_day = vec_pickup_day, 
                               pickup_hour = vec_pickup_hour, 
                               dropoff_month = vec_dropoff_month, 
                               dropoff_day = vec_dropoff_day, 
                               dropoff_hour = vec_dropoff_hour, 
                               pickup_weekday = vec_pickup_weekday, 
                               dropoff_weekday = vec_dropoff_weekday,
                               passenger_count = yellow_june$passenger_count, 
                               trip_distance = yellow_june$trip_distance, 
                               pickup_longitude = yellow_june$pickup_longitude, 
                               pickup_latitude = yellow_june$pickup_latitude, 
                               dropoff_longitude = yellow_june$dropoff_longitude, 
                               dropoff_latitude = yellow_june$dropoff_latitude, 
                               payment_type = yellow_june$payment_type, 
                               fare_amount = yellow_june$fare_amount, 
                               extra  = yellow_june$extra, 
                               mta_tax = yellow_june$mta_tax, 
                               tip_amount = yellow_june$tip_amount, 
                               tolls_amount = yellow_june$tolls_amount, 
                               improvement_surcharge = yellow_june$improvement_surcharge, 
                               total_amount = yellow_june$total_amount
                               )
# mutate day_time part
yellow_june_test <- yellow_june_test %>% mutate(isWeekend = vec_isWeekend)

yellow_june_test <- yellow_june_test %>% mutate(weekendTime = 99)
yellow_june_test$weekendTime[vec_weekend_index] <- vec_weekendTime
yellow_june_test$weekendTime <- as.factor(yellow_june_test$weekendTime)

yellow_june_test <- yellow_june_test %>% mutate(workTime = 99)
yellow_june_test$workTime[vec_workday_index] <- vec_workdayTime
yellow_june_test$workTime <- as.factor(yellow_june_test$workTime)

########## Annotated ##########
# July
# vec_ID <- c(1:nrow(yellow_july))
# vec_pickup_time <- as.POSIXct(yellow_july$tpep_pickup_datetime)
# vec_dropoff_time <- as.POSIXct(yellow_july$tpep_dropoff_datetime)
# vec_travel_time_sec <- vec_dropoff_time - vec_pickup_time %>% as.numeric()
# vec_pickup_month <- substr(yellow_july$tpep_pickup_datetime, 6, 7) %>% as.numeric() %>% as.factor()
# vec_pickup_day <- substr(yellow_july$tpep_pickup_datetime, 9, 10) %>% as.numeric() %>% as.factor()
# vec_pickup_hour <- substr(yellow_july$tpep_pickup_datetime, 12, 13) %>% as.numeric() %>% as.factor()
# vec_dropoff_month <- substr(yellow_july$tpep_dropoff_datetime, 6, 7) %>% as.numeric() %>% as.factor()
# vec_dropoff_day <- substr(yellow_july$tpep_dropoff_datetime, 9, 10) %>% as.numeric() %>% as.factor()
# vec_dropoff_hour <- substr(yellow_july$tpep_dropoff_datetime, 12, 13) %>% as.numeric() %>% as.factor()
# vec_pickup_weekday <- weekdays(vec_pickup_time) %>% as.factor()
# levels(vec_pickup_weekday) # view first
# levels(vec_pickup_weekday) <- c("mon", "tue", "wed", "fri", "sat", "sun", "thu") # change order: week_day
# vec_dropoff_weekday <- weekdays(vec_dropoff_time) %>% as.factor()
# levels(vec_dropoff_weekday) # view first
# levels(vec_dropoff_weekday) <- c("mon", "tue", "wed", "fri", "sat", "sun", "thu") # change order: week_day
# 
# # calculate day_time
# # prerequesite: vec_isWeekend, vec_pickup_hour calculated
# vec_weekend_index <- which(vec_isWeekend == TRUE)
# vec_weekend_time <- vec_pickup_hour[vec_isWeekend]
# vec_weekendTime <- apply(data.frame(vec_weekend_index, vec_weekend_time), 1, custom_weekend)
# 
# vec_workday_index <- which(vec_isWeekend == FALSE)
# vec_workday_time <- vec_pickup_hour[!vec_isWeekend]
# vec_workdayTime <- apply(data.frame(vec_workday_index, vec_workday_time), 1, custom_work_day)
# 
# yellow_july_test <- data.frame(ID = vec_ID,
#                                pickup_time = vec_pickup_time, 
#                                dropoff_time = vec_dropoff_time, 
#                                travel_time_sec = vec_travel_time_sec, 
#                                pickup_month = vec_pickup_month, 
#                                pickup_day = vec_pickup_day, 
#                                pickup_hour = vec_pickup_hour, 
#                                dropoff_month = vec_dropoff_month, 
#                                dropoff_day = vec_dropoff_day, 
#                                dropoff_hour = vec_dropoff_hour, 
#                                pickup_weekday = vec_pickup_weekday, 
#                                dropoff_weekday = vec_dropoff_weekday,
#                                passenger_count = yellow_july$passenger_count, 
#                                trip_distance = yellow_july$trip_distance, 
#                                pickup_longitude = yellow_july$pickup_longitude, 
#                                pickup_latitude = yellow_july$pickup_latitude, 
#                                dropoff_longitude = yellow_july$dropoff_longitude, 
#                                dropoff_latitude = yellow_july$dropoff_latitude, 
#                                payment_type = yellow_july$payment_type, 
#                                fare_amount = yellow_july$fare_amount, 
#                                extra  = yellow_july$extra, 
#                                mta_tax = yellow_july$mta_tax, 
#                                tip_amount = yellow_july$tip_amount, 
#                                tolls_amount = yellow_july$tolls_amount, 
#                                improvement_surcharge = yellow_july$improvement_surcharge, 
#                                total_amount = yellow_july$total_amount
#                                )
# # mutate day_time part
# yellow_july_test <- yellow_june_test %>% mutate(isWeekend = vec_isWeekend)
# 
# yellow_july_test <- yellow_july_test %>% mutate(weekendTime = 99)
# yellow_july_test$weekendTime[vec_weekend_index] <- vec_weekendTime
# 
# yellow_july_test <- yellow_july_test %>% mutate(workTime = 99)
# yellow_july_test$workTime[vec_workday_index] <- vec_workdayTime
########## Annotated ##########

########## Data Cleaning ##########
# drop invalid coordinate tuples
# range = c(-74.05, 40.54, -73.70001, 40.91553)
yellow_june_cleaned <- yellow_june_test %>% 
  filter(pickup_latitude   >=  40.54, pickup_latitude   <=  40.91553, 
         pickup_longitude  >= -74.05, pickup_longitude  <= -73.70001, 
         dropoff_latitude  >=  40.54, dropoff_latitude  <=  40.91553, 
         dropoff_longitude >= -74.05, dropoff_longitude <= -73.70001)
yellow_june_cleaned <- yellow_june_test %>% 
   filter(pickup_latitude   >=  40.54, pickup_latitude   <=  40.91553, 
          pickup_longitude  >= -74.05, pickup_longitude  <= -73.70001, 
          dropoff_latitude  >=  40.54, dropoff_latitude  <=  40.91553, 
          dropoff_longitude >= -74.05, dropoff_longitude <= -73.70001) %>% 
   filter(passenger_count > 0, 
          travel_time_sec >= 60, travel_time_sec <= 10800, 
          format(yellow_june_cleaned$pickup_time, "%X") != 
             format(yellow_june_cleaned$dropoff_time[42271], "%X"), 
          trip_distance > 0.2, trip_distance <= 100, 
          total_amount > 0) # time == "00:00:00"


########## Annotated ##########
# yellow_july_cleaned <- yellow_july_test %>% 
#   filter(pickup_latitude   >=  40.54, pickup_latitude   <=  40.91553, 
#          pickup_longitude  >= -74.05, pickup_longitude  <= -73.70001, 
#          dropoff_latitude  >=  40.54, dropoff_latitude  <=  40.91553, 
#          dropoff_longitude >= -74.05, dropoff_longitude <= -73.70001)
yellow_july_cleaned <- data.frame()
########## Annotated ##########

########## Day time to Hours data.frame  ##########
dayTimeToHour <- data.frame(
   order = c(1:9),
   dayTime = c("work_midnight", "work_on_work", "work_whiteday", "work_off_work", "work_night", 
               "week_midnight", "week_morning", "week_afternoon", "week_night"), 
   hour = c("(0-7)", "(7-9)", "(9-16)", "(16-18)", "(18-24)", 
            "(0-5)", "(5-12)", "(12-17)", "(17-24)"), 
   length = c(7, 2, 7, 2, 6, 
              5, 7, 5, 7)
)


source("files/districtNYC.R")

# collect dropped variables
rm(vec_ID, 
   vec_dropoff_day, 
   vec_dropoff_hour, 
   vec_dropoff_month, 
   vec_dropoff_time, 
   vec_dropoff_weekday, 
   vec_isWeekend, 
   vec_pickup_day, 
   vec_pickup_hour, 
   vec_pickup_month, 
   vec_pickup_time, 
   vec_pickup_weekday, 
   vec_travel_time_sec, 
   vec_weekend_index, 
   vec_weekend_time,
   vec_weekendTime, 
   vec_workday_index, 
   vec_workday_time, 
   vec_workdayTime
   )
