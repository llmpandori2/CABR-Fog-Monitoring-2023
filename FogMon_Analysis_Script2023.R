###########################################
# Title: Fog Monitoring Analysis Script
# Purpose: Analyze data and create figures using fog, vegetation,
# & LWS experiment data from all fog weather stations at CABR
# Author: Virginia Javier & Lauren Pandori 
# Created: 06/28/2023
# Last Edited: 08/02/2023 
###########################################

#### load packages #####
library(tidyverse)
library(janitor)
library(scales)
library(agricolae)

#### figure set 1: Mediterranean climate ####

## figure 1: mean monthly temperature of all four fog stations

# load & clean up processed fog data
station_day <- read_csv('./data/clean_data/Combined_FogMon_Data2023.csv',
                     show_col_types = FALSE) %>% 
  # select columns halfhour and AirT_C_Avg (Average Temperature)
  select(halfhour,Rain_mm_Tot, AirT_C_Avg) %>%
  # take avg daily temp
  mutate(day = floor_date(halfhour, unit = 'day')) %>%
  group_by(day) %>%
  summarize(day_temp = mean(AirT_C_Avg, na.rm = TRUE),
            day_rain = mean(Rain_mm_Tot, na.rm = TRUE)) %>%
  # give agnostic month (for graphing) and year
  ungroup() %>%
  mutate(year = as_factor(year(day)),
         year_day = yday(day))


# load and summarize airport data 
# note: data pulled from SD Airport from NOAA's LCD Dataset
# see code: https://github.com/llmpandori2/sandbox/blob/main/SD_Weather_Data/SD_Weather_Abiotic_Query.R
airport_day <- read_csv("data/airport_data/SD_Intl_Airport_1995-2023.csv") %>%
  # remove duplicates
  distinct() %>%
  # calculate mean daily temperature and precipitation 
  group_by(date_col) %>%
  summarize(day_temp = mean(hourlydrybulbtemperature, na.rm = TRUE),
            day_precip = mean(hourlyprecipitation, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(mutate(year = as_factor(year(date)),
                year_day = yday(date)),
                # convert F to C
                day_temp = (day_temp-32)*(5/9))

# create figure line graph
ggplot(mapping = aes(x = day, y = day_temp, color = year, group = year)) +
  geom_line(data = airport_day, alpha = 0.2) + 
  # increase size of line graph
  geom_line(data = temp_month, size = 2) +
  # rename x axis
  xlab('Month') +
  # rename y axis
  ylab('Temperature (°C)') +
  # change the y-axis limits to 10 - 30
  ylim(10,30) +
  #scale_x_date(date_breaks = '2 month', date_labels = '%b %y') + 
  theme_classic() +
  # change the axis line thickness
  theme(axis.line=element_line(size=1))+
  # change the axis labels color and font size
  theme(axis.text=element_text(colour = 'black',size=15)) +
  # change the axis titles font size and make labels bold
  theme(axis.title=element_text(size=20))




# create figure: line graph
ggplot(data = temp_month,
       mapping = aes(x = month, y = monthly_temp)) +
  # increase size of line graph
  geom_line(size = 1) +
  # rename x axis
  xlab('Month and Year') +
  # rename y axis
  ylab('Temperature (°C)') +
  # change the y-axis limits to 15 - 35
  ylim(15,35) +
  scale_x_date(date_breaks = '2 month', date_labels = '%b %y') + 
  theme_classic() +
  # change the axis line thickness
  theme(axis.line=element_line(size=1))+
  # change the axis labels color and font size
  theme(axis.text=element_text(colour = 'black',size=15)) +
  # change the axis titles font size and make labels bold
  theme(axis.title=element_text(size=20))

# save figure as a .jpg file (change size if needed)
ggsave('./figs/temp_yr.png', width = 6, height = 6)

## figure 2: mean monthly precipitation

# load & clean up processed data
precip_month <- read_csv('./data/clean_data/Combined_FogMon_Data2023.csv') %>% 
  # select columns: halfhour and Rain_mm_Tot (Total amount of rain)
  select(halfhour,Rain_mm_Tot,Station) %>%
  # remove any rows with missing values
  remove_missing() %>% 
  # add a column that mutates halfhour values into months
  mutate(month = floor_date(halfhour, unit = 'month')) %>% 
  # add the total amount of rain by month per station
  group_by(month,Station) %>% 
  summarize(monthly_precip = sum(Rain_mm_Tot)) %>%
  ungroup() %>%
  group_by(month) %>%
  summarize(monthly_precip = mean(monthly_precip)) %>%
  ungroup() %>%
  mutate(month = as_date(month))

# create figure: line graph
ggplot(data = precip_month,
       mapping = aes(x = month, y = monthly_precip)) +
  # change the data line thickness 
  geom_line(size = 1) +
  # change x-axis scale and rename labels (3 - March, 6 - June, 9 - September, 12 - December)
  scale_x_date(date_breaks = '2 month', date_labels = '%b %y') +
  scale_y_continuous(limits = c(0, 30)) + 
  # rename x axis
  xlab('Month and Year') +
  # rename y axis
  ylab('Rainfall (mm)') +
  theme_classic() +
  # change the axis line thickness
  theme(axis.line=element_line(size=1))+
  # change the axis labels color and font size
  theme(axis.text=element_text(colour = 'black',size=15)) +
  # change the axis titles font size and make labels bold
  theme(axis.title=element_text(size=20))

# save figure as a .jpg file (change size if needed)
ggsave('./figs/precip_stations_up.jpg', width = 6, height = 6)

#### figure set 2: vegetation communities ####

## figure 3: vegetation species richness

# load & clean up vegetation species richness data
veg_cabr_richness <- readxl::read_xlsx('./data/clean_data/Veg Monitoring/veg_cabr_richness.xlsx') %>% 
  # select columns 3 (SiteCode), 6 (SurveyYear),8 (Species_Code), 10 (Frequency)
  select(c(3,6,8,10)) %>%
  # select site codes closest to fog monitoring stations
  # 076 = Event Bluff, 053 = Tide Pool, 018 = Spicy Bunker, 045 = New New
  filter(SiteCode == '076' | SiteCode == '053' | SiteCode == '018' | SiteCode == '045') %>% 
  # remove any rows with missing data
  remove_missing() %>% 
  # arrange the site codes in ascending order
  arrange(SiteCode) %>% 
  # create a Station column and assign the site codes to stations
  mutate(Station = case_when( SiteCode == '076' ~ 'Ocean-Side, High Elevation - Chamise Coastal Mesa Chaparral',
                              SiteCode == '053' ~ 'Ocean-Side, Low Elevation - California Brittle Bush Scrub',
                              SiteCode == '018' ~ 'Bay-Side, Mid-Elevation - Wart-stemmed Ceanothus Chaparral',
                              SiteCode == '045' ~ 'Ocean-Side, Mid-Elevation - Lemonade Berry Scrub')) %>%
  # select the sum veg data collected from the most recent monitoring year
  group_by(Station) %>% 
  filter(SurveyYear == max(SurveyYear))

# create figure: bar graph
ggplot(data = veg_cabr_richness,
       # reverse the order and color fill of stations (TP, SB, NN, EB)
       mapping = aes(x = Species_Code, y = Frequency, group = Station, fill = Station)) +
  geom_col() +
  # change the order of stations and assign a fill color in the legend
  scale_fill_manual(values = c("Ocean-Side, Low Elevation - California Brittle Bush Scrub" = "#F8766D",
                               "Bay-Side, Mid-Elevation - Wart-stemmed Ceanothus Chaparral" = "#7CAE00",
                               "Ocean-Side, Mid-Elevation - Lemonade Berry Scrub" = "#00BFC4",
                               "Ocean-Side, High Elevation - Chamise Coastal Mesa Chaparral" = "#C77CFF")) + 
  # separate station data into different graphs 
  facet_wrap(~factor(Station,levels=c("Ocean-Side, Low Elevation - California Brittle Bush Scrub", 
                                      "Bay-Side, Mid-Elevation - Wart-stemmed Ceanothus Chaparral", "Ocean-Side, Mid-Elevation - Lemonade Berry Scrub", 
                                      "Ocean-Side, High Elevation - Chamise Coastal Mesa Chaparral"))) +
  # remove legend
  guides(fill = FALSE) +
  # rename x-axis
  xlab("Species Code") +
  theme_classic() +
  # change x-axis labels to be 90° 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8)) +
  # change the axis line thickness
  theme(axis.line=element_line(size=1))+
  # change the axis labels color and font size
  theme(axis.text=element_text(colour = 'black',size=10)) +
  # change the axis titles font size and make labels bold
  theme(axis.title=element_text(size=12,face="bold")) +
  # bold the facet wrap titles
  theme(strip.text = element_text(face = "bold"))
  
# find the default R colors for 4 variables (used in scale_fill_manual())
hex <- hue_pal()(4)

# save figure as a .jpg file (change size if needed)
ggsave('./figs/veg_richness_new_stations_up.jpg', width = 9, height = 6)

#### figure set 3: haze & LWS experiment ####

## figure 4: hazer experiment

# load, clean up, & combine fog data
clean_up_AF <- read_csv('./data/clean_data/Combined_FogMon_Data2023.csv') %>% 
  # add a column that mutates halfhour values into days 
  mutate(day = date(halfhour)) %>% 
  # remove rows with missing data
  remove_missing() %>% 
  # select columns: Station, day, and LWS
  select(Station,day,LWmV_17ft_Avg,LWmV_50ft_Avg,LWmV_66ft_Avg) %>% 
  # calculate the mean of each LWS by day 
  group_by(day) %>% 
  summarize(mean_LW_17 = mean(LWmV_17ft_Avg),
            mean_LW_50 = mean(LWmV_50ft_Avg),
            mean_LW_66 = mean(LWmV_66ft_Avg)) %>% 
  # calculate the mean of all three LWS by day
  group_by(day) %>% 
  summarize(mean_LW = mean(mean_LW_17, mean_LW_50, mean_LW_66))

# load Andrew's Finest trail camera calculated data
AF_haze <- read_csv('./data/processed_data/AF_HazeOutput_2023.csv')

# join fog and camera data frames 
LWS_haze <- full_join(AF_haze,clean_up_AF, by = 'day') %>% 
  # filter data for days after 01/19/2023
  filter(date(day) > 2023-01-19) %>% 
  # remove rows with any missing data
  remove_missing()

# calculate regression values 
reg <- lm(formula = mean_LW ~ haze, data = LWS_haze)
# print values in console
summary(reg)

# create figure: point graph
ggplot(data = LWS_haze,
       mapping = aes(x = mean_LW, y = haze)) +
  geom_point() +
  # rename x-axis
  xlab('Leaf Wetness (mV)') +
  # rename y-axis
  ylab('Haze') +
  theme_classic() +
  # change the axis line thickness
  theme(axis.line=element_line(size=1))+
  # change the axis labels color and font size
  theme(axis.text=element_text(colour = 'black',size=12)) +
  # change the axis titles font size and make labels bold
  theme(axis.title=element_text(size=12,face="bold"))


# save figure as a .jpg file (change size if needed)
ggsave('./figs/hazer_graph.jpg', width = 6, height = 4)

## figure 5: LWS experiment

# load & clean up experiment data
exp_lws_regression <- read_csv('./data/clean_data/exp_processed_cr800_20230627.csv',
                               # transform column values to register as numeric
                               col_types = cols(LWmV_17ft_Avg = col_number(), 
                                                   LWmV_50ft_Avg = col_number(), LWmV_66ft_Avg = col_number())) %>% 
  # select columns: TIMESTAMP, LWmV_17ft_Avg, LWmV_50ft_Avg
  select(TIMESTAMP,LWmV_17ft_Avg,LWmV_50ft_Avg,LWmV_66ft_Avg) %>% 
  # filter values between experiment start and end times
  filter(TIMESTAMP >= '2023-06-27 3:20:00', # start date/time 2023-06-27 10:20:00 
         TIMESTAMP <= '2023-06-27 6:40:00') %>% # end date/time to be 2023-06-27 13:40:00
  # isolate the time from the TIMESTAMP column
  mutate(time = hms::as_hms(TIMESTAMP)) %>% 
  # assign water amount to experiment times/set by row
  mutate(h2o_mL = case_when(row_number(time) == 1:3 ~ '0.1',
                           row_number(time) == 4:6 ~ '0.5',
                           row_number(time) == 7:9 ~ '1',
                           row_number(time) == 10:12 ~ '1.5',
                           row_number(time) == 13:15 ~ '2.0',
                           row_number(time) == 16:18 ~ '2.5',
                           row_number(time) == 19:21 ~ '3.0')) %>%
  # calculate the mean of each water amount per LWS
  group_by(h2o_mL) %>% 
  summarize(mean_lw17 = mean(LWmV_17ft_Avg),
            mean_lw50 = mean(LWmV_50ft_Avg),
            mean_lw66 = mean(LWmV_66ft_Avg)) %>%
  # there was an issue with LWS 17ft during the 2.0g set, so remove this rounded value
  mutate(mean_lw17 = if_else(round(mean_lw17) == 567,
                             NA,mean_lw17)) %>% 
  # make the table longer by adding a column identifying the LWS distance and 
  # adding the mV values to another column
  pivot_longer(mean_lw17:mean_lw50, names_to = 'LWS_dist_ft', values_to = 'mV') %>% 
  # rename the LWS_dist_ft values to just state the distance number
  mutate(LWS_dist_ft = case_when(LWS_dist_ft == "mean_lw17" ~ '17',
                                 LWS_dist_ft == "mean_lw50" ~ '50')) %>% 
  # transform h2o_mL to register as numeric (run everything before this and then run this line)
  mutate(h2o_mL = as.numeric(h2o_mL)) 

# calculate regression values and print values in console
reg <- lm(formula = mV ~ h2o_mL, data = exp_lws_regression)
summary(reg)
reg
# intercept = 335.9 mV (will use later)
# equation --> y = 335.9 + 150.9x (will use later)

# calculate the average of 66ft LWS (fog threshold) DO NOT USE
threshold = exp_lws_regression %>% 
  mutate(threshold = mean(mean_lw66))
# calculated threshold ~ 274 mV 

# create figure: point graph & regression line
ggplot(data = exp_lws_regression, 
       mapping = aes(x = h2o_mL, y = mV, color = 'mV')) + 
  # plot points separately (LWS 17ft/50ft & 66ft)
  geom_point() + 
  geom_point(data = exp_lws_regression,
             mapping = aes(x = h2o_mL, y = mean_lw66, group = mean_lw66, color = 'mean_lw66')) +
  # plot linear regression line
  geom_smooth(method = 'lm') + 
  # change x-axis scale (min = 0, max = 3, scale = 0.5)
  scale_x_continuous(breaks = seq(0,3,0.5)) +
  # remove size legend
  guides(size = FALSE) +
  # rename the x-axis
  xlab(expression("H"[2]*"O"~"(mL)")) +
  # rename the y-axis
  ylab('Leaf Wetness (mV)') +
  # rename legend title
  labs(colour = 'Leaf Wetness Sensor') +
  # rename label values and assign colors/theme
  scale_color_manual(values = c("dodgerblue3","brown3"),
                     labels = c('66ft Sensor', '17ft & 50ft Sensors')) +
  theme_classic() +
  # change the axis line thickness
  theme(axis.line=element_line(size=1))+
  # change the axis labels color and font size
  theme(axis.text=element_text(colour = 'black',size=15)) +
  # change the axis titles font size and make labels bold
  theme(axis.title=element_text(size=20)) + 
  # change legend title and text font size
  theme(legend.title = element_text(size=15), 
        legend.text = element_text(size=12)) 
  

# save figure as a .jpg file (change size if needed)
ggsave('./figs/lws_exp_stations_esa.jpg', width = 8, height = 6)

#### figure set 4: fog presence & intensity over space & time ####

## figure 6: fog presence

# load & clean up processed fog data
fog_presence <- read_csv('./data/clean_data/Combined_FogMon_Data2023.csv') %>% 
  # selecting rows where rain total is less than or equal to 0 mm
  # no rain = fog
  subset(Rain_mm_Tot <= 0) %>%    
  # select columns 1 (Station), 2 (halfhour), 5 (LWmV_17ft_Avg), 10 (LWmV_50ft_Avg),
  # and 15 (LWmV_66ft_Avg)
  select(c(1,2,5,10,15)) %>% 
  # remove any rows with missing data
  remove_missing() %>% 
  # calculate the average of each LWS by halfhour and station
  group_by(halfhour, Station) %>% 
  summarize(avg_lw17 = mean(LWmV_17ft_Avg),
            avg_lw50 = mean(LWmV_50ft_Avg),
            avg_lw66 = mean(LWmV_66ft_Avg)) %>% 
  # make the table longer by adding a column identifying the LWS distance and 
  # adding the mV values to another column
  pivot_longer(avg_lw17:avg_lw66, names_to = 'LWS_dist_ft', values_to = 'mV') %>%
  mutate(day = date(halfhour)) %>% 
  # calculate the average leaf wetness mV by halfhour and station
  group_by(day, Station) %>% 
  summarize(mean_lw = mean(mV)) %>% 
  # subtract fog threshold (335.9 mV, intercept of the calculated regression line) 
  mutate(mean_lw = mean_lw - 335.9) %>% 
  # rename stations
  mutate(Station = case_when( Station == 'Event Bluff' ~ 'Ocean-Side, High Elevation',
                              Station == 'Tide Pool' ~ 'Ocean-Side, Low Elevation',
                              Station == 'Spicy Bunker' ~ 'Bay-Side, Mid-Elevation',
                              Station == 'New New' ~ 'Ocean-Side, Mid-Elevation'))

# load and clean up data
water_conversion_over0 <- fog_presence %>% 
  # select the maximum recorded mean LW by day and station
  group_by(day, Station) %>% 
  summarize(max = max(mean_lw)) %>% 
  # divide the maximum number by 150.9 (already subtracted 335.9 in fog_presence)
  mutate(mL = (max/150.9)) %>% 
  # mutate mL values where any negative value is changed to equal 0
  mutate(mL = if_else(mL <= 0, 0,mL)) 


# create figure: line graph (can't replicate - with *day part)
ggplot() +
  geom_line(data = fog_presence,
            # change color order of graphs 
            mapping = aes(x = day, y = mean_lw, group = Station, color = fct_rev(Station))) +
  geom_line(data = water_conversion_over0,
            # change color order of graphs
            mapping = aes(x = day, y = mL, group = Station, color = fct_rev(Station))) +
  scale_y_continuous(name = "Leaf Wentesss (mV)", sec.axis = sec_axis(~.*day, name =expression("H"[2]*"O"~"(mL)"))) +
  # remove color legend
  guides(color = FALSE) +
  # rename x-axis
  xlab('Month') +
  # rename y-axis
  ylab('Leaf Wetness (mV)') +
  # separate the station data into different graphs, change the order and assign colors
  scale_color_manual(values = c("Ocean-Side, Low Elevation" = "#F8766D",
                                "Bay-Side, Mid-Elevation" = "#7CAE00",
                                "Ocean-Side, Mid-Elevation" = "#00BFC4",
                                "Ocean-Side, High Elevation" = "#C77CFF")) + 
  # separate station data into different graphs in this order 
  facet_wrap(~factor(Station,levels=c("Ocean-Side, Low Elevation", 
                                      "Bay-Side, Mid-Elevation", "Ocean-Side, Mid-Elevation", 
                                      "Ocean-Side, High Elevation"))) +
  theme_classic() +
  # change the axis line thickness
  theme(axis.line=element_line(size=1))+
  # change the axis labels color and font size
  theme(axis.text=element_text(colour = 'black',size=10)) +
  # change the axis titles font size and make labels bold
  theme(axis.title=element_text(size=12,face="bold")) +
  # bold the facet wrap titles
  theme(strip.text = element_text(face = "bold"))

# save figure as a .jpg file (change size if needed)
ggsave('/Users/virginiajavier/Desktop/SIP 2023/SIP_VGJ_2023/figures/fog_presence_up.jpg', width = 6, height = 4)

## figure 7: fog intensity

# load & clean up data
fog_intensity <- read_csv('./data/clean_data/Combined_FogMon_Data2023.csv') %>% 
  # select columns 1 (Station), 2 (halfhour), 5 (LWmV_17ft_Avg), 10 (LWmV_50ft_Avg),
  # 15 (LWmV_66ft_Avg), 22 (Rain_mm_Tot)
  select(c(1,2,5,10,15,22)) %>%
  # create a column that identifies if the data records rain or no rain
  mutate(rain_present = if_else(Rain_mm_Tot > 0, 'RAIN','NO RAIN')) %>% 
  # make the table longer by adding a column identifying the LWS distance and 
  # adding the mV values to another column
  pivot_longer(LWmV_17ft_Avg:LWmV_66ft_Avg, names_to = 'LWS_dist_ft', values_to = 'mV') %>% 
  # calculate the mean lw by halfhour, Station, and if rain is present/absent
  group_by(halfhour,Station,rain_present) %>% 
  summarize(mean_lw = mean(mV)) %>% 
  # fog threshold was calculated as 335.9 mV
  # create a column that identifies time where there is fog and no fog
  # create another column that isolates the day from halfhour column
  mutate(fog_no_fog = if_else(mean_lw > 335.9, 'FOG', 'NO FOG'),
         Day = date(halfhour)) %>%
  # count the number of hours per day when there is/isn't fog and add to a new column
  group_by(Day,Station,fog_no_fog) %>%
  count(fog_no_fog, sort = TRUE, name = "n_fog_hrs") %>%
  # make the table wider by adding columns FOG, NO FOG, NA from the fog_no_fog column
  # and values from the n_fog_hrs column
  pivot_wider(names_from =fog_no_fog, values_from = n_fog_hrs) %>%
  # rename FOG column to fog_hours
  rename(fog_hours = FOG) %>% 
  # remove columns 3 (NO FOG) and 5 (NA)
  select(-3,-5) %>%
  # remove any rows with missing data
  remove_missing() %>% 
  # tally up the number of fog hours per station and day
  # divide by 2 to convert the number of half hours to hours
  group_by(Day,Station) %>% 
  tally(fog_hours/2) %>% 
  # rename the stations
  mutate(Station = case_when( Station == 'Event Bluff' ~ 'Ocean-Side, High Elevation',
                              Station == 'Tide Pool' ~ 'Ocean-Side, Low Elevation',
                              Station == 'Spicy Bunker' ~ 'Bay-Side, Mid-Elevation',
                              Station == 'New New' ~ 'Ocean-Side, Mid-Elevation'))
  
# calculate the median of each station
median <- fog_intensity %>% 
  group_by(Station) %>% 
  summarize(median = median(n))

# ANOVA test ####

# calculate ANOVA using fog intensity data
fog_aov <- aov(n ~ Station,
               data = fog_intensity
)

# use HSD test and turn results into a data frame
hsd_test <- as.data.frame(HSD.test(fog_aov, trt = 'Station')$group) %>%
  # make station names into a column
  rownames_to_column(var = "Station")


hsd_test$Station <- fct_relevel(hsd_test$Station, c("Ocean-Side, Low Elevation",
                                            "Ocean-Side, Mid-Elevation",
                                            "Bay-Side, Mid-Elevation",
                                            "Ocean-Side, High Elevation"))


fog_intensity$Station <- fct_relevel(fog_intensity$Station, c("Ocean-Side, Low Elevation",
                                                              "Bay-Side, Mid-Elevation",
                                                         "Ocean-Side, Mid-Elevation",
                                                         "Ocean-Side, High Elevation"))

# add hsd info to fog intensity data
fog_intensity <- left_join(fog_intensity, select(hsd_test, Station, groups), by = 'Station')

# print summary of fog_aov
summary(fog_aov)

# create figure: boxplot
ggplot(data = fog_intensity,
       # reverse the order and color of stations 
       mapping = aes(x = Station, y = n, group = Station, fill = Station)) +
  # remove outlier points
  geom_boxplot() + 
  # add letter labels (calculated in ANOVA test section)
  geom_label(mapping = aes(x = Station, y = 0, label = groups, fontface='bold'), label.size = NA, fill = NA) +
  # remove color legend
  guides(fill = FALSE) +
  # rename x-axis
  xlab("Station") +
  # rename y-axis
  ylab("Fog Hours") +
  theme_classic() +
  theme(text = element_text(size = 12)) + 
  # tilt x-axis labels to 45 degrees
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12)) +
  theme(axis.title = element_text(face = 'bold')) + 
  # change the axis line thickness
  theme(axis.line=element_line(size=1))+
  # change the axis labels color and font size
  theme(axis.text=element_text(colour = 'black', size = 12))

# save figure as a .jpg file (change size if needed)
ggsave('./figs/fog_intensity_up.jpg', width = 7, height = 6)

#### figure set 5: water analysis graphs (no rain present) ####

# regression line equation to solve for mL of water 
# x = (y - 335.9)/150.9

## figure 8: mL for all stations without rain present

# load and clean up data
water_conversion_over0 <- fog_presence %>% 
  # select the maximum recorded mean LW by day and station
  group_by(day, Station) %>% 
  summarize(max = max(mean_lw)) %>% 
  # divide the maximum number by 150.9 (already subtracted 335.9 in fog_presence)
  mutate(mL = (max/150.9)) %>% 
  # mutate mL values where any negative value is changed to equal 0
  mutate(mL = if_else(mL <= 0, 0,mL)) 

# create figure: line graph
ggplot(data = water_conversion_over0,
       # change color order of graphs
       mapping = aes(x = day, y = mL, group = Station, color = Station)) + 
  geom_line() +
  # scale_color_discrete(limits = c("Ocean-Side, Low Elevation", "Bay-Side, Mid-Elevation", "Ocean-Side, Mid-Elevation", "Ocean-Side, High Elevation")) +
  # remove color legend
  guides(color = FALSE) +
  # assign name order and colors in legend/graphs
  scale_color_manual(values = c("Ocean-Side, Low Elevation" = "#F8766D",
                               "Bay-Side, Mid-Elevation" = "#7CAE00",
                               "Ocean-Side, Mid-Elevation" = "#00BFC4",
                               "Ocean-Side, High Elevation" = "#C77CFF")) + 
  # separate station data into different graphs in this order
  facet_wrap(~factor(Station,levels=c("Ocean-Side, Low Elevation", 
                                      "Bay-Side, Mid-Elevation", "Ocean-Side, Mid-Elevation", 
                                      "Ocean-Side, High Elevation"))) +
  # rename x axis
  xlab('Month') +
  # rename y axis
  ylab(expression("H"[2]*"O"~"(mL)")) +
  theme_classic() +
  # change the axis line thickness
  theme(axis.line=element_line(size=1))+
  # change the axis labels color and font size
  theme(axis.text=element_text(colour = 'black',size=10)) +
  # change the axis titles font size and make labels bold
  theme(axis.title=element_text(size=12, face="bold")) +
  theme(strip.text.x = element_text(face="bold"))

# save figure as a .jpg file (change size if needed)
ggsave('/Users/virginiajavier/Desktop/SIP 2023/SIP_VGJ_2023/figures/water_conversion_norain_up.jpg', width = 6, height = 4)

## figure 9: elevation gradient stations ONLY (TP, NN, EB)

# load data
elev_mL <- water_conversion_over0 %>% 
  # select TP, NN, and EB station
  filter(Station == 'Tide Pool' | Station == 'New New' | Station == 'Event Bluff') %>% 
  # rename stations
  mutate(Station = case_when(Station == "Tide Pool" ~ "Tide Pool (15m)",
                   Station == "New New" ~ "New New (51m)",
                   Station == "Event Bluff" ~ "Event Bluff (120m)"))


# create figure: line graph
ggplot(data = elev_mL,
       # change color order of graphs (TP, NN, EB)
       mapping = aes(x = day, y = mL, group = Station, color = fct_rev(Station))) + 
  geom_line() +
  # remove color legend
  guides(color = FALSE) +
  # separate the station data into different graphs and change the order (TP, NN, EB)
  facet_wrap(~fct_rev(Station)) +
  # rename x axis
  xlab('Month') +
  # rename y axis
  ylab(expression("H"[2]*"O"~"(mL)")) +
  theme_classic() +
  # change the axis line thickness
  theme(axis.line=element_line(size=1))+
  # change the axis labels color and font size
  theme(axis.text=element_text(colour = 'black',size=15)) +
  # change the axis titles font size and make labels bold
  theme(axis.title=element_text(size=20)) +
  # increase the size of facet_wrap titles
  theme(strip.text.x = element_text(size = 24))

# save figure as a .jpg file (change size if needed)
ggsave('/Users/virginiajavier/Desktop/SIP 2023/SIP_VGJ_2023/figures/water_conversion_norain_elev_ESA.jpg', width = 12, height = 7)

## figure 10: mL peninsula sides stations ONLY (SB & NN)

# load data
peninsula_mL <- water_conversion_over0 %>% 
  # select SB & NN stations
  filter(Station == 'Spicy Bunker' | Station == 'New New') %>% 
  # rename stations
  mutate(Station = case_when(Station == "Spicy Bunker" ~ "Spicy Bunker (Bay-Side)",
                             Station == "New New" ~ "New New (Ocean-Side)"))

# create figure: line graph
ggplot(data = peninsula_mL,
       # change color order of graphs (SB, NN)
       mapping = aes(x = day, y = mL, group = Station, color = fct_rev(Station))) + 
  geom_line() +
  # remove color legend
  guides(color = FALSE) +
  # separate the station data into different graphs and change the order (SB, NN)
  facet_wrap(~fct_rev(Station)) +
  # rename x axis
  xlab('Month') +
  # rename y axis
  ylab(expression("H"[2]*"O"~"(mL)")) +
  theme_classic() +
  # change the axis line thickness
  theme(axis.line=element_line(size=1))+
  # change the axis labels color and font size
  theme(axis.text=element_text(colour = 'black',size=15)) +
  # change the axis titles font size and make labels bold
  theme(axis.title=element_text(size=20)) +
  # change the font size of the facet_wrap title
  theme(strip.text.x = element_text(size = 24))


# save figure as a .jpg file (change size if needed)
ggsave('/Users/virginiajavier/Desktop/SIP 2023/SIP_VGJ_2023/figures/water_conversion_norain_peninsula_ESA.jpg', width = 12, height = 7)



#### figure set 6: sum of water (mm) of fog and rain per station (DONT USE)####

## load data

# fog, no rain
fog <- water_conversion_over0 %>% 
  mutate(fog_or_rain = "FOG")

# area of LWS = 69.6 cm2
  
# rain, no fog
rain <- read_csv('./clean_data/Combined_FogMon_Data2023.csv') %>% 
  # selecting rows where rain total is less than or equal to 0 mm
  # no rain = fog
  subset(Rain_mm_Tot > 0) %>%    
  # select columns 1 (Station), 2 (halfhour), 5 (LWmV_17ft_Avg), 10 (LWmV_50ft_Avg),
  # and 15 (LWmV_66ft_Avg)
  select(c(1,2,22)) %>% 
  # remove any rows with missing data
  remove_missing() %>% 
  mutate(day = date(halfhour)) %>% 
  # calculate the average of each LWS by halfhour and station
  group_by(day, Station) %>% 
  summarize(mm = sum(Rain_mm_Tot)) %>% 
  group_by(day,Station) %>% 
  summarize(mL = mm/0.5568) %>% 
  mutate(fog_or_rain = "RAIN") %>% 
  mutate(Station = case_when( Station == 'Event Bluff' ~ 'Ocean-Side, High Elevation',
                              Station == 'Tide Pool' ~ 'Ocean-Side, Low Elevation',
                              Station == 'Spicy Bunker' ~ 'Bay-Side, Mid-Elevation',
                              Station == 'New New' ~ 'Ocean-Side, Mid-Elevation')) 


# combine
rain_fog <- bind_rows(fog,rain) %>% 
  mutate(fog_or_rain = case_when(fog_or_rain == 'FOG' ~'Fog',
                                 fog_or_rain == 'RAIN' ~'Rain'))

## create figure
ggplot(data = rain_fog,
       mapping = aes(x = day, y = mL, group = fog_or_rain, fill = fog_or_rain)) +
  geom_col() +
  facet_wrap(~fog_or_rain) +
  guides(fill = FALSE) + 
  xlab('Month') +
  # rename y axis
  ylab(expression("H"[2]*"O"~"(mL)")) +
  theme_classic() +
  # change the axis labels color and font size
  theme(axis.text=element_text(colour = 'black',size=10)) +
  # change the axis titles font size and make labels bold
  theme(axis.title=element_text(size=12)) +
  theme(strip.text.x = element_text(size=12))

# save figure as a .jpg file (change size if needed)
ggsave('/Users/virginiajavier/Desktop/SIP 2023/SIP_VGJ_2023/figures/rain_vs_fog.jpg', width = 6, height = 4)


#### figure set 7: combined mV and mL graph ####

## combine data
mv_ml <- merge(fog_presence,water_conversion_over0) %>% 
  # make any negative values equal to 0 for mean_lw and mL
  mutate(mL = if_else(mL <= 0, 0,mL)) %>% 
  mutate(mean_lw = if_else(mean_lw <= 0,0,mean_lw))

## create figure: line graphs
ggplot(data = mv_ml, mapping = aes(x = day)) +
  # graph mean_lw data
  geom_line(mapping = aes(y = mean_lw, group = Station, color = fct_rev(Station))) +
  # rename left y-axis, create a right y-axis (divide values by 150.9, change the name of the axis, and change axis breaks)
  scale_y_continuous(name = "Leaf Wetness (mV)", sec.axis = sec_axis(trans = ~(./150.9),name =expression("H"[2]*"O"~"(mL)"), breaks = c(0,0.5,1,1.5,2))) +
  # remove color legend
  guides(color = FALSE) +
  # rename x-axis
  xlab('Month') +
  # rename y-axis
  ylab('Leaf Wetness (mV)') +
  # separate the station data into different graphs, change the order and assign colors
  scale_color_manual(values = c("Ocean-Side, Low Elevation" = "#F8766D",
                                "Bay-Side, Mid-Elevation" = "#7CAE00",
                                "Ocean-Side, Mid-Elevation" = "#00BFC4",
                                "Ocean-Side, High Elevation" = "#C77CFF")) + 
  # separate station data into different graphs in this order 
  facet_wrap(~factor(Station,levels=c("Ocean-Side, Low Elevation", 
                                      "Bay-Side, Mid-Elevation", "Ocean-Side, Mid-Elevation", 
                                      "Ocean-Side, High Elevation"))) +
  theme_classic() +
  # change the axis labels color and font size
  theme(axis.text=element_text(colour = 'black',size=10)) +
  # change the axis titles font size
  theme(axis.title=element_text(size=12)) +
  # change font size for station titles
  theme(strip.text = element_text(size = 12))

# save figure as a .jpg file (change size if needed)
ggsave('/Users/virginiajavier/Desktop/SIP 2023/SIP_VGJ_2023/figures/mv_ml.jpg', width = 6, height = 4)
