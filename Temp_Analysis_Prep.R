library(tidyverse)
library(dplyr)
library(reshape2)
library(lubridate)
library(fastDummies)
library(ggplot2)
library(tidyr)
library(readr)
library(zoo)
library(pastecs)


# read data from Subsurface
ss_dives <- read_csv('/Users/jking/Projects/Subsurface/Subsurface summary.csv') %>%
  drop_na(., `watertemp [F]`) %>%
  separate_wider_delim(location, " - ", names = c("Location", "Site"))
ss_dives$month <- format(ss_dives$date, "%B")
ss_dives$day <- format(ss_dives$date, "%d")

Water <- ss_dives %>%
  group_by(month, day, Location) %>%
  summarize(mean = mean(`watertemp [F]`),
            n = n()) %>%
  arrange(Location)

# read data from Shearwater csv's
# Function for read a csv passed in, skipping the first two rows
read_plus <- function(flnm) {
  read_csv(flnm, skip = 2) %>% 
    mutate(filename = flnm)
}

# get a list of all csv files in the Shearwater folder and read them all in
divesS <-
  list.files(path = "/Users/jking/Library/Mobile Documents/com~apple~CloudDocs/Projects/Diving/Shearwater",
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_plus(.)) %>%
  filter(`External O2 Sensor 1 (mV)` > 0)

# create left, mid and right substring functions to parse out the date
left = function(text, num_char) {
  substr(text, 1, num_char)
}
mid = function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}
right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

# create a time in seconds, dive number and date column
divesS <- divesS %>%
  #  mutate(Time.s = as.numeric(`Time (ms)`)/1000,
  mutate(Time.s = case_when(
    !is.na(`Time (ms)`) ~ `Time (ms)` / 1000,  # convert ms → seconds
    TRUE            ~ `Time (sec)`             # else keep existing seconds
  ),
  Dive.Number = as.numeric(left(right(filename, 18), 3)),
  `Date` = as.Date(mid(right(filename, 18), 5, 10)))

# another function to read the header portion of the Shearwater files
read_head <- function(flnm) {
  read.csv(flnm, nrows = 1) %>% 
    mutate(filename = flnm)
}

# Read the header, create a R date time column, but jsut keep the date time and dive number
head <- 
  list.files(path = "/Users/jking/Library/Mobile Documents/com~apple~CloudDocs/Projects/Diving/Shearwater",
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_head(.)) %>%
  mutate(.,Start.Dttm = as.POSIXct(str_replace(Start.Date," [A,P]M",""),format="%m/%d/%Y %H:%M:%S",tz=Sys.timezone())) %>%
  select(Dive.Number, Start.Dttm)

dives2 <- left_join(divesS, head, by = "Dive.Number") %>%
  mutate(Run.Time = Start.Dttm + Time.s) %>%
  arrange(desc(Dive.Number))

# Step 1: Create a new file for ascent analysis
temp1 <- dives2 %>%
  group_by(Dive.Number) %>%
  arrange(Time.s, .by_group = TRUE) %>%   # ensure same ordering as SAS BY
  mutate(
    vert_rate_fpm = if_else(
      Time.s >= 60,
      lag(Depth, 6) - Depth,
      NA_real_
    ),
    
    # Ascent: once ascent starts, it stays on
    Ascent = {
      temp <- 0
      out <- numeric(n())
      for (i in seq_along(Time.s)) {
        if (i == 1) temp <- 0
        if (!is.na(vert_rate_fpm[i]) && vert_rate_fpm[i] > 10) temp <- 1
        out[i] <- temp
      }
      out
    },
    
    # Deco stop
    Deco = ifelse(
      Ascent == 1 & (
        ((Depth >= (`First Stop Depth` - 3)) & (Depth <= (`First Stop Depth` + 3)) & `First Stop Time` > 0) |
          (`First Stop Depth` <= 20)
      ),
      1, 0
    ),
    
    # Descent: either the start of the dive or a strong negative rate
    Descent = ifelse(row_number() == 1 | vert_rate_fpm <= -8 | is.na(vert_rate_fpm), 1, 0),
    
    # Bottom time = neither ascent nor descent
    Bottom = ifelse((Ascent + Descent) == 0, 1, 0),
    
    # Phase (priority order: Deco > Ascent > Descent > Bottom)
    Phase = case_when(
      Descent == 1 ~ "Descent",
      Bottom == 1 ~ "Bottom",
      Deco == 1 ~ "Deco Stop",
      Ascent == 1 ~ "Ascent",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup() %>%
  select(Dive.Number, Time.s, Depth, vert_rate_fpm,
         `First Stop Depth`, `First Stop Time`,
         Ascent, Deco, Descent, Bottom, Phase)

# Summary like PROC MEANS by Phase
summary_phase <- temp1 %>%
  group_by(Phase) %>%
  summarise(
    rate_mean = mean(vert_rate_fpm, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Step 2: Get max first_stop_depth per dive
fsd <- temp1 %>%
  group_by(Dive.Number) %>%
  summarise(max_fsd = max(`First Stop Depth`, na.rm = TRUE), .groups = "drop")

# Step 3: Merge with temp1 and assign Type
temp2 <- temp1 %>%
  left_join(fsd, by = "Dive.Number") %>%
  filter(Ascent == 1, Depth > 0) %>%
  mutate(
    TYPE = ifelse(`First Stop Depth` == max_fsd & max_fsd > 0, 1, 2)
  )

# Step 4: Summarize the ascent rates by type
summary_rate <- temp2 %>%
  group_by(TYPE) %>%
  summarise(
    mean_rate = mean(vert_rate_fpm, na.rm = TRUE),
    median_rate = median(vert_rate_fpm, na.rm = TRUE),
    max_rate = max(vert_rate_fpm, na.rm = TRUE),
    min_rate = min(vert_rate_fpm, na.rm = TRUE),
    n = n()
  )

library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
# new
library(plotly)


# read data from Subsurface
ss_dives <- read_csv('/Users/jking/Projects/Subsurface/Subsurface summary.csv') %>%
  drop_na(., `watertemp [F]`) %>%
  separate_wider_delim(location, " - ", names = c("Location", "Site"))
ss_dives$month <- format(ss_dives$date, "%B")
ss_dives$day <- format(ss_dives$date, "%d")

Water <- ss_dives %>%
  group_by(month, day, Location) %>%
  summarize(mean = mean(`watertemp [F]`),
            n = n()) %>%
  arrange(Location)






