###########################################
# Title: Fog Monitoring Clean Up Script
# Purpose: Clean up & combine data from all fog weather stations at CABR
# Author: Virginia Javier & Lauren Pandori 
# Created: 06/14/2023
# Last Edited: 06/14/2023
###########################################

##### load packages #####
library(tidyverse)
library(lubridate)
library(janitor)

##### step 1: load and clean up all fog weather station data #####

# clean up = select columns, unit conversions, rename columns, combine WEEWX & CR800 data
# there are four stations total and two types of stations present at CABR: 
# Davis station (Event Bluff & Tide Pool) = CR800 & WEEWX files
# Campbell station (New New & Spicy Bunker) = CR800 file ONLY
# all stations have 3 Leaf Wetness Sensors (LWS) 17ft, 50ft, & 66ft away from the station
# 12 LWS in total
# since there are different station types, there are different clean up processes

# Note on BattV_Avg & PTemp_C_Avg columns: the battery voltage and panel temperatures
# are included in the final compiled data set because they can be indicators as to 
# why there may be some data anomalies. check these columns if you are unsure about
# the validity of some data values

##### load & clean up processed Event Bluff (EB) data #####
# The Event Bluff (EB) Station is a Davis station, so it collects data as both CR800 & WEEWX files
# we have to combine these files together to get a complete set of data from this station
# the columns below were selected because they share measurements across both Davis & 
# Campbell stations and are also important to help quantify fog presence/intensity

## load CR800 file
# columns selected: 2 (TIMESTAMP), 4 (BattV_Avg), 5 (PTemp_C_Avg), 
# 31:45 (all Leaf Wetness Sensor (LWS) data for 17ft,50ft,66ft sensors)
EB_processed_CR800 <- read_csv('./clean_data/EB_processed_CR800.csv') %>% 
  select(c(2,4,5,31:45))

## load WEEWX file
# columns selected: 1 (dateTime), 68 (outHumidity), 69 (outTemp), 77 (rain), 
# 114 (windSpeed)
# units conversions were performed to match the format of CR800 files
EB_processed_WEEWX <- read_csv('clean_data/EB_Davis_processed_WEEWX_20230609.csv') %>% 
  select(c(1,68,69,77,114)) %>% 
  # convert dateTime values from unix_epoch format to TS (seen in CR800 files)
  mutate(dateTime = as_datetime(dateTime)) %>% 
  # change the timezone to PST
  mutate(dateTime = with_tz(dateTime,tzone = 'US/Pacific')) %>% 
  # some values were recorded as occurring in the year 2009 before the date/time was adjusted
  # just to check if the stations were operational, filter these values out
  filter(year(dateTime) > 2009) %>% 
  # convert rain units from inches to millimeter 
  mutate(rain = rain*25.4) %>% 
  # convert windSpeed units from miles per hour to meters per second
  mutate(windSpeed = windSpeed*0.44704) %>%
  # convert temperature units from Fahrenheit to Celsius
  mutate(outTemp = (outTemp-32) - (5/9)) %>% 
  # rename the columns selected to match CR800 tables
  rename(TIMESTAMP = dateTime,
         Rain_mm_Tot = rain,
         WS_ms_Avg = windSpeed,
         RH_Avg = outHumidity,
         AirT_C_Avg = outTemp) 

## use the full_join() function to combine both processed data frames by the 'TIMESTAMP' column
# add a column that assigns the station name: 'Event Bluff'
# arrange the dates and times in ascending order
EB_clean <- full_join(EB_processed_CR800,EB_processed_WEEWX, by = 'TIMESTAMP') %>% 
  mutate(Station = 'Event Bluff') %>% 
  arrange('TIMESTAMP')


##### load & clean up processed New New (NN) data #####
# The New New (NN) station is a Campbell station, so it collects data in the form
# of a CR800 file ONLY. There is no need to combine different sets of data together
# the columns below were selected because they share measurements across both Davis & 
# Campbell stations and are also important to help quantify fog presence/intensity

## load CR800 file
# columns selected: 2 (TIMESTAMP), 4 (BattV_Avg), 5 (PTemp_C_Avg), 8 (Rain_mm_Tot), 
# 12 (WS_ms_Avg), 15 (AirT_C_Avg), 21 (RH_Avg),
# 31:45 (all Leaf Wetness Sensor (LWS) data for 17ft,50ft,66ft sensors)
# add a column that assigns the station name: 'New New'
# arrange the dates and times in ascending order
NN_clean <- read_csv('./clean_data/NN_processed_CR800.csv') %>% 
  select(c(2,4,5,8,12,15,21,31:45)) %>% 
  mutate(Station = 'New New') %>% 
  arrange('TIMESTAMP')


##### load & clean up Spicy Bunker (SB) data #####
# The Spicy Bunker (SB) station is a Campbell station, so it collects data in the form
# of a CR800 file ONLY. There is no need to combine different sets of data together
# the columns below were selected because they share measurements across both Davis & 
# Campbell stations and are also important to help quantify fog presence/intensity

## load CR800 file
# columns selected: 2 (TIMESTAMP), 4 (BattV_Avg), 5 (PTemp_C_Avg), 8 (Rain_mm_Tot), 
# 12 (WS_ms_Avg), 15 (AirT_C_Avg), 21 (RH_Avg),
# 31:45 (all Leaf Wetness Sensor (LWS) data for 17ft,50ft,66ft sensors)
# add a column that assigns the station name: 'Spicy Bunker'
# arrange the dates and times in ascending order
SB_clean <- read_csv('./clean_data/SB_processed_CR800.csv') %>% 
  select(c(2,4,8,5,12,15,21,31:45)) %>% 
  mutate(Station = 'Spicy Bunker') %>% 
  arrange('TIMESTAMP')


##### load & clean up Tide Pool (TP) data #####
# The Tide Pool (TP) Station is a Davis station, so it collects data as both CR800 & WEEWX files
# must combine these files together to get a complete set of data from this station
# the columns below were selected because they share measurements across both Davis & 
# Campbell stations and are also important to help quantify fog presence/intensity

## load CR800 file
# columns selected: 2 (TIMESTAMP), 4 (BattV_Avg), 5 (PTemp_C_Avg), 
# 31:45 (all Leaf Wetness Sensor (LWS) data for 17ft,50ft,66ft sensors)
TP_processed_CR800 <- read_csv('./clean_data/TP_processed_CR800.csv') %>% 
  select(c(2,4,5,31:45))

## load WEEWX file
# columns selected: 1 (dateTime), 68 (outHumidity), 69 (outTemp), 77 (rain), 
# 114 (windSpeed)
# units conversions were performed to match the format of CR800 files
TP_processed_WEEWX <- read_csv('./clean_data/TP_Davis_processed_WEEWX_20230609.csv') %>% 
  select(c(1,68,69,77,114)) %>% 
  # convert dateTime values from unix_epoch format to TS (seen in CR800 files)
  mutate(dateTime = as_datetime(dateTime)) %>% 
  # change the timezone to PST
  mutate(dateTime = with_tz(dateTime,tzone = 'US/Pacific')) %>% 
  # some values were recorded as occurring in the year 2009 before the date/time was adjusted
  # just to check if the stations were operational, filter these values out
  filter(year(dateTime) > 2009) %>% 
  # convert rain units from inches to millimeter 
  mutate(rain = rain*25.4) %>% 
  # convert windSpeed units from miles per hour to meters per second
  mutate(windSpeed = windSpeed*0.44704) %>%
  mutate(outTemp = (outTemp-32) - (5/9)) %>% 
  # rename the columns selected to match CR800 tables
  rename(TIMESTAMP = dateTime,
         Rain_mm_Tot = rain,
         WS_ms_Avg = windSpeed,
         RH_Avg = outHumidity,
         AirT_C_Avg = outTemp) 

## use the full_join() function to combine both processed data frames by the 'TIMESTAMP' column
# add a column that assigned the station name: 'Tide Pool'
# arrange the dates and times in ascending order
TP_clean <- full_join(TP_processed_CR800,TP_processed_WEEWX,by = 'TIMESTAMP') %>% 
  mutate(Station = 'Tide Pool') %>% 
  arrange('TIMESTAMP')

##### step 2: join all 4 Stations #####
# use the bind_rows() function to combine the rows based on column names
fog_all_stations <- bind_rows(TP_clean,EB_clean,NN_clean,SB_clean) %>% 
  # Campbell stations collect data every 10 minutes. Davis stations collect data
  # every 30 minutes. add a column that summarizes the time to the nearest
  # half hour
  mutate(halfhour = ceiling_date(TIMESTAMP,unit = '30 mins')) %>% 
  # arrange the dates and times in ascending order 
  arrange(TIMESTAMP) %>% 
  # the stations didn't all start recording data at the same time
  # filter the dates/times so we only analyze data when all four stations
  # were recording data (the code for time bounds are off by 7 hours because
  # the set timezone R is 7 hours ahead of PST)
  filter(TIMESTAMP >= '2022-07-26 4:00:00', ## to get the start date/time to be 2022-07-26 11:00:00 
         TIMESTAMP <= '2023-05-25 5:30:00') %>% ## to get the end date/time to be 2023-05-25 12:30:00
  # remove the TIMESTAMP column
  select(-TIMESTAMP) %>% 
  # calculate the mean of all of the columns by the half hour and Station
  group_by(Station,halfhour) %>% 
  summarize_all(mean)

##### step 3: save compiled data as a .csv file #####
write_csv(fog_all_stations, file = 'Combined_FogMon_Data2023.csv', append = FALSE)


