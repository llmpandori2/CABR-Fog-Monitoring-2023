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
library(lubridate)
library(janitor)
library(scales)
library(agricolae)
library(ggdark)
library(PNWColors)

# custom theme w dark field
lltheme_dark <- dark_theme_bw() + theme(text = element_text(size = 24),
                                        # add more space between panels
                                        panel.spacing = unit(1, 'lines'),
                                        # no background to wrap panels
                                        strip.background = element_blank(),
                                        strip.text = element_text(size = 24, hjust = 0),
                                        # panel labels outside x axis labels
                                        strip.placement = 'outside',
                                        # adjust x axis labels
                                        axis.text.y = element_text(size = 24),
                                        axis.text.x = element_text(size = 24),
                                        panel.grid = element_blank())


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
  geom_label(mapping = aes(x = Station, y = 0, label = groups, fontface='bold'), label.size = NA, size = 8,  fill = NA) +
  # remove color legend
  guides(fill = FALSE) +
  scale_fill_manual(values = pnw_palette(name = 'Sunset2', n = 4, type = c('discrete'))) + 
  # rename x-axis
  xlab("Station") +
  # rename y-axis
  ylab("Fog duration (hours/day)") +
  lltheme_dark +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 

# save figure as a .jpg file (change size if needed)
ggsave('./figs/fog_intensity_up.jpg', width = 12, height = 10)

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
ggplot(data = fog_presence,
       # change color order of graphs
       mapping = aes(x = day, y = mean_lw, group = Station, color = Station)) + 
  geom_line() +
  # scale_color_discrete(limits = c("Ocean-Side, Low Elevation", "Bay-Side, Mid-Elevation", "Ocean-Side, Mid-Elevation", "Ocean-Side, High Elevation")) +
  # remove color legend
  #guides(color = FALSE) +
  # assign name order and colors in legend/graphs
  scale_color_manual(values = pnw_palette(name = 'Sunset2', n = 4, type = c('discrete'))) + 
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
  mutate(Station = case_when( Station == 'Event Bluff' ~ 'Ocean side, high Elevation',
                              Station == 'Tide Pool' ~ 'Ocean Side, Low Elevation',
                              Station == 'Spicy Bunker' ~ 'Bay Side, Mid Elevation',
                              Station == 'New New' ~ 'Ocean Side, Mid Elevation')) 


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
  mutate(mean_lw = if_else(mean_lw <= 0,0,mean_lw)) %>%
  mutate(fctorder = case_when(Station == "Bay-Side, Mid-Elevation" ~ 2,
                              Station == "Ocean-Side, High Elevation" ~ 4,
                              Station == "Ocean-Side, Low Elevation" ~ 1,
                              Station == "Ocean-Side, Mid-Elevation" ~ 3))

## create figure: line graphs
ggplot(data = mv_ml, mapping = aes(x = day)) +
  # graph mean_lw data
  geom_line(mapping = aes(y = mean_lw, group = factor(fctorder), color = factor(fctorder))) +
  # rename left y-axis, create a right y-axis (divide values by 150.9, change the name of the axis, and change axis breaks)
  scale_y_continuous(name = "Leaf Wetness (mV)", sec.axis = sec_axis(trans = ~(./150.9),name =expression("H"[2]*"O"~"(mL)"), breaks = c(0,0.5,1,1.5,2))) +
  # remove color legend
  guides(color = FALSE) +
  # rename x-axis
  xlab('Month') +
  # rename y-axis
  ylab('Leaf Wetness (mV)') +
  # separate the station data into different graphs, change the order and assign colors
  scale_color_manual(values = pnw_palette(name = 'Sunset2', n = 4, type = c('discrete'))) + 
  # separate station data into different graphs in this order 
  facet_wrap(~factor(fctorder)) +
  lltheme_dark +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 

ggsave('./figs/panelfig.png', width = 12, height = 8)

  theme_classic() +
  # change the axis labels color and font size
  theme(axis.text=element_text(colour = 'black',size=10)) +
  # change the axis titles font size
  theme(axis.title=element_text(size=12)) +
  # change font size for station titles
  theme(strip.text = element_text(size = 12))

# save figure as a .jpg file (change size if needed)
ggsave('/Users/virginiajavier/Desktop/SIP 2023/SIP_VGJ_2023/figures/mv_ml.jpg', width = 6, height = 4)