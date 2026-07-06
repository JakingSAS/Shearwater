library(tidyverse)
library(dplyr)
library(reshape2)
library(lubridate)
library(fastDummies)
library(ggplot2)
library(zoo)
library(pastecs)


# Read in usage data
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
  map_df(~read_plus(.))

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

# Use for Shearwater only
All_Shearwater <- left_join(divesS, head, by = "Dive.Number") %>%
  mutate(Run.Time = Start.Dttm + Time.s) %>%
  arrange(desc(Dive.Number))

temp <- All_Shearwater %>%
  group_by(Dive.Number) %>%
  summarize(mean = mean(`Water Temp`),
                  min = min(`Water Temp`),
                  max = max(`Water Temp`),
                  n = n())
