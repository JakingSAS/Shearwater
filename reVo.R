library(tidyverse)
library(dplyr)
library(reshape2)
library(lubridate)
library(fastDummies)
library(ggplot2)
library(zoo)

# Read in reVo data after dumping it to csv
myfile1 <- "/Users/jking/Library/Mobile Documents/com~apple~CloudDocs/Projects/Diving/rEvo Skill List/Build and Closed Checklist-Table 1.csv"
temp <- read_csv(myfile1) %>%
  filter(., `Build Check` %in% c('%O2:','Bar:','%He:','Cell 1:','Cell 2:','Cell 3:','Cell 4:','Cell 5:',
                                'Check external battery: fire solenoid 10 seconds, EXT Voltage > 6.5V'))
raw_revo <-  data.frame(t(temp[-1]))
colnames(raw_revo) <- c('%O2','O2 Bar','%O2 Dil','%He Dil','Dil Bar','Cell 1','Cell 2','Cell 3',
                        'Cell 4','Cell 5','Voltage')
raw_revo <- tibble::rownames_to_column(raw_revo, "temp")
Date <- as.data.frame(as.Date(raw_revo$temp, "%m/%d/%Y"))
colnames(Date) <- c('Date')
raw_revo <- cbind(Date, raw_revo)


# Read in usage data
read_plus <- function(flnm) {
  read_csv(flnm, skip = 2, col_types = cols(.default = "c")) %>% 
    mutate(filename = flnm)
}

dives <-
  list.files(path = "/Users/jking/Library/Mobile Documents/com~apple~CloudDocs/Projects/Diving/Shearwater/",
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_plus(.))

left = function(text, num_char) {
  substr(text, 1, num_char)
}
mid = function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}
right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

dives <- dives %>%
  mutate(Time.ms = as.numeric(`Time (ms)`)/1000,
        `Time (m)` = (as.numeric(`Time (ms)`)/1000)/60,
         Dive.Number = as.numeric(left(right(filename, 18), 3)),
         `Date` = as.Date(mid(right(filename, 18), 5, 10)))


# summarize dives by date
dives_sum <- dives %>%
  group_by(`Dive.Number`, Date) %>%
  dplyr::summarize(
    `Time (m)` = max(as.numeric(`Time (m)`), na.rm = TRUE),
    `Min Depth` = min(as.numeric(Depth), na.rm = TRUE),
    `Mean Depth` = mean(as.numeric(Depth), na.rm = TRUE),
    `Max Depth` = max(as.numeric(Depth), na.rm = TRUE),
    `Max First Stop Depth` = max(as.numeric(`First Stop Depth`), na.rm = TRUE),
    `Max PPO2` = mean(as.numeric(`Average PPO2`), na.rm = TRUE),
    `Mean PPO2` = mean(as.numeric(`Average PPO2`), na.rm = TRUE),
    `Max FO2` = mean(as.numeric(`Fraction O2`), na.rm = TRUE),
    `Mean FO2` = mean(as.numeric(`Fraction O2`), na.rm = TRUE),
    `Mean FHe` = mean(as.numeric(`Fraction He`), na.rm = TRUE),
    `Max First Stop Time` = max(as.numeric(`First Stop Time`), na.rm = TRUE),
    `Min Water Temp` = min(as.numeric(`Water Temp`), na.rm = TRUE),
    `Mean Water Temp` = mean(as.numeric(`Water Temp`), na.rm = TRUE),
    `Max Water Temp` = max(as.numeric(`Water Temp`), na.rm = TRUE),
    `Min External O2 Sensor 1 (mV)` = min(as.numeric(`External O2 Sensor 1 (mV)`), na.rm = TRUE),
    `Mean External O2 Sensor 1 (mV)` = mean(as.numeric(`External O2 Sensor 1 (mV)`), na.rm = TRUE),
    `Max External O2 Sensor 1 (mV)` = max(as.numeric(`External O2 Sensor 1 (mV)`), na.rm = TRUE),
    `Min External O2 Sensor 2 (mV)` = min(as.numeric(`External O2 Sensor 2 (mV)`), na.rm = TRUE),
    `Mean External O2 Sensor 2 (mV)` = mean(as.numeric(`External O2 Sensor 2 (mV)`), na.rm = TRUE),
    `Max External O2 Sensor 2 (mV)` = max(as.numeric(`External O2 Sensor 2 (mV)`), na.rm = TRUE),
    `Min External O2 Sensor 3 (mV)` = min(as.numeric(`External O2 Sensor 3 (mV)`), na.rm = TRUE),
    `Mean External O2 Sensor 3 (mV)` = mean(as.numeric(`External O2 Sensor 3 (mV)`), na.rm = TRUE),
    `Max External O2 Sensor 3 (mV)` = max(as.numeric(`External O2 Sensor 3 (mV)`), na.rm = TRUE),
    `Min Battery Voltage` = min(as.numeric(`Battery Voltage`), na.rm = TRUE),
    `Mean Battery Voltage` = mean(as.numeric(`Battery Voltage`), na.rm = TRUE),
    `Max Battery Voltage` = max(as.numeric(`Battery Voltage`), na.rm = TRUE)
    )


dives_date_sum <- dives_sum %>%
  group_by(`Date`) %>%
  dplyr::summarize(
    `Dives` = n_distinct(`Dive.Number`),
    `Time (m)` = sum(as.numeric(`Time (m)`), na.rm = TRUE),
    `Min Depth` = min(as.numeric(`Min Depth`), na.rm = TRUE),
    `Mean Depth` = mean(as.numeric(`Mean Depth`), na.rm = TRUE),
    `Max Depth` = max(as.numeric(`Max Depth`), na.rm = TRUE),
    `Max First Stop Depth` = max(as.numeric(`Max First Stop Depth`), na.rm = TRUE),
    `Max PPO2` = max(as.numeric(`Max PPO2`), na.rm = TRUE),
    `Mean PPO2` = mean(as.numeric(`Mean PPO2`), na.rm = TRUE),
    `Max FO2` = max(as.numeric(`Max FO2`), na.rm = TRUE),
    `Mean FO2` = mean(as.numeric(`Mean FO2`), na.rm = TRUE),
    `Mean FHe` = mean(as.numeric(`Mean FHe`), na.rm = TRUE),
    `Max First Stop Time` = max(as.numeric(`Max First Stop Time`), na.rm = TRUE),
    `Min Water Temp` = min(as.numeric(`Min Water Temp`), na.rm = TRUE),
    `Mean Water Temp` = mean(as.numeric(`Mean Water Temp`), na.rm = TRUE),
    `Max Water Temp` = max(as.numeric(`Max Water Temp`), na.rm = TRUE),
    `Min External O2 Sensor 1 (mV)` = min(as.numeric(`Min External O2 Sensor 1 (mV)`), na.rm = TRUE),
    `Mean External O2 Sensor 1 (mV)` = mean(as.numeric(`Mean External O2 Sensor 1 (mV)`), na.rm = TRUE),
    `Max External O2 Sensor 1 (mV)` = max(as.numeric(`Max External O2 Sensor 1 (mV)`), na.rm = TRUE),
    `Min External O2 Sensor 2 (mV)` = min(as.numeric(`Min External O2 Sensor 2 (mV)`), na.rm = TRUE),
    `Mean External O2 Sensor 2 (mV)` = mean(as.numeric(`Mean External O2 Sensor 2 (mV)`), na.rm = TRUE),
    `Max External O2 Sensor 2 (mV)` = max(as.numeric(`Max External O2 Sensor 2 (mV)`), na.rm = TRUE),
    `Min External O2 Sensor 3 (mV)` = min(as.numeric(`Min External O2 Sensor 3 (mV)`), na.rm = TRUE),
    `Mean External O2 Sensor 3 (mV)` = mean(as.numeric(`Mean External O2 Sensor 3 (mV)`), na.rm = TRUE),
    `Max External O2 Sensor 3 (mV)` = max(as.numeric(`Max External O2 Sensor 3 (mV)`), na.rm = TRUE),
    `Min Battery Voltage` = min(as.numeric(`Min Battery Voltage`), na.rm = TRUE),
    `Mean Battery Voltage` = mean(as.numeric(`Mean Battery Voltage`), na.rm = TRUE),
    `Max Battery Voltage` = max(as.numeric(`Max Battery Voltage`), na.rm = TRUE)
  )


dives_date_cum <- dives_date_sum %>%
  mutate(
    `Dives` = cumsum(`Dives`),
    `Time (m)` = cumsum(as.numeric(`Time (m)`)),
    `Min Depth` = cummin(as.numeric(`Min Depth`)),
    `Mean Depth` = cummean(as.numeric(`Mean Depth`)),
    `Max Depth` = cummax(as.numeric(`Max Depth`)),
    `Max First Stop Depth` = cummax(as.numeric(`Max First Stop Depth`)),
    `Max PPO2` = cummax(as.numeric(`Max PPO2`)),
    `Mean PPO2` = cummean(as.numeric(`Mean PPO2`)),
    `Max FO2` = cummax(as.numeric(`Max FO2`)),
    `Mean FO2` = cummean(as.numeric(`Mean FO2`)),
    `Mean FHe` = cummean(as.numeric(`Mean FHe`)),
    `Max First Stop Time` = cummax(as.numeric(`Max First Stop Time`)),
    `Min Water Temp` = cummin(as.numeric(`Min Water Temp`)),
    `Mean Water Temp` = cummean(as.numeric(`Mean Water Temp`)),
    `Max Water Temp` = cummax(as.numeric(`Max Water Temp`)),
    `Min External O2 Sensor 1 (mV)` = cummin(as.numeric(`Min External O2 Sensor 1 (mV)`)),
    `Mean External O2 Sensor 1 (mV)` = cummean(as.numeric(`Mean External O2 Sensor 1 (mV)`)),
    `Max External O2 Sensor 1 (mV)` = cummax(as.numeric(`Max External O2 Sensor 1 (mV)`)),
    `Min External O2 Sensor 2 (mV)` = cummin(as.numeric(`Min External O2 Sensor 2 (mV)`)),
    `Mean External O2 Sensor 2 (mV)` = cummean(as.numeric(`Mean External O2 Sensor 2 (mV)`)),
    `Max External O2 Sensor 2 (mV)` = cummax(as.numeric(`Max External O2 Sensor 2 (mV)`)),
    `Min External O2 Sensor 3 (mV)` = cummin(as.numeric(`Min External O2 Sensor 3 (mV)`)),
    `Mean External O2 Sensor 3 (mV)` = cummean(as.numeric(`Mean External O2 Sensor 3 (mV)`)),
    `Max External O2 Sensor 3 (mV)` = cummax(as.numeric(`Max External O2 Sensor 3 (mV)`)),
    `Min Battery Voltage` = cummin(as.numeric(`Min Battery Voltage`)),
    `Mean Battery Voltage` = cummean(as.numeric(`Mean Battery Voltage`)),
    `Max Battery Voltage` = cummax(as.numeric(`Max Battery Voltage`))
  )




# plot deaths by natural causes
nat_plot <- nat_cause %>%
  filter(., `Week Ending Date` > as.Date("2014-01-10") &
           `Week Ending Date` < as.Date("2020-12-05")) %>%
  ggplot( aes(x=`MMWR Week`, y=`Natural Cause`,
              group=`MMWR Year`, color=factor(`MMWR Year`),)) +
  geom_line() +
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")
print(nat_plot + labs(title= "Weekly Deaths by Natural Causes, 2014-Present",
                  y="Deaths", x = "Week"))

# plot deaths by non-natural causes
nnat_plot <- nat_cause %>%
  filter(., `Week Ending Date` > as.Date("2014-01-10") &
           `Week Ending Date` < as.Date("2020-12-05")) %>%
  ggplot( aes(x=`MMWR Week`, y=`Non-Natural Cause`,
              group=`MMWR Year`, color=factor(`MMWR Year`),)) +
  geom_line() +
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")
print(nnat_plot + labs(title= "Weekly Deaths by Non-Natural Causes, 2014-Present",
                  y="Deaths", x = "Week"))

# plot deaths by flu
flu_plot <- nat_cause %>%
  filter(., `Week Ending Date` > as.Date("2014-01-10") &
           `Week Ending Date` < as.Date("2020-12-05")) %>%
  ggplot( aes(x=`MMWR Week`, y=`Influenza and pneumonia`,
              group=`MMWR Year`, color=factor(`MMWR Year`),)) +
  geom_line() +
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")
print(flu_plot + labs(title= "Weekly Deaths by Flu, 2014-Present",
                       y="Deaths", x = "Week"))

# try a linear model on all 3 
Natmod <- lm(`Natural Cause` ~ `MMWR Year` + `MMWR Week`, data=nat_cause)
summary(Natmod)

NNatmod <- lm(`Non-Natural Cause` ~ `MMWR Year` + `MMWR Week`, data=nat_cause)
summary(NNatmod)

Flumod <- lm(`Influenza and pneumonia` ~ `MMWR Year` + `MMWR Week`, data=nat_cause)
summary(Flumod)

# Normalize to a rate per 1000 people and re-run
years <- c(2014, 2015, 2016, 2017, 2018, 2019)
pop <- c(321418821, 323127515, 325719178, 327167439, 328239523, 331002651)
US_Pop <- data.frame(years, pop) %>%
  mutate(pop1000 = pop/1000)

nat_cause <- left_join(nat_cause, US_Pop, by=c("MMWR Year"="years")) %>%
  mutate(`Natural Cause 1000` = `Natural Cause`/pop1000,
         `Non-Natural Cause 1000` = `Non-Natural Cause`/pop1000,
         `Influenza and pneumonia 1000` = `Influenza and pneumonia`/pop1000
         )

# try a linear model on all 3 as rate/1000
Natmod <- lm(`Natural Cause 1000` ~ `MMWR Year` + `MMWR Week`, data=nat_cause)
summary(Natmod)

NNatmod <- lm(`Non-Natural Cause 1000` ~ `MMWR Year` + `MMWR Week`, data=nat_cause)
summary(NNatmod)

Flumod <- lm(`Influenza and pneumonia 1000` ~ `MMWR Year` + `MMWR Week`, data=nat_cause)
summary(Flumod)








