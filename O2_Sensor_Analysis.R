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
  map_df(~read_plus(.)) %>%
  filter(`External O2 Sensor 1 (mV)` > 0)

# get a list of all csv files in the Nerd folder and read them all in
# divesN <-
#   list.files(path = "/Users/jking/Library/Mobile Documents/com~apple~CloudDocs/Projects/Diving/Nerd/",
#              pattern = "*.csv", 
#              full.names = T) %>% 
#   map_df(~read_plus(.))

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

# divesN <- divesN[,c('Time (ms)', 'filename', 'External O2 Sensor 2 (mV)', 'External O2 Sensor 3 (mV)')] %>%
#   mutate(Time.s = as.numeric(`Time (ms)`)/1000,
#          Dive.Number = as.numeric(left(right(filename, 17), 2)) + 711,
#          `Date` = as.Date(mid(right(filename, 18), 5, 10))) %>%
#   rename(., `External O2 Sensor 4 (mV)` = `External O2 Sensor 2 (mV)`,
#          `External O2 Sensor 5 (mV)` = `External O2 Sensor 3 (mV)`)

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

# join the detail dive data with the header
# NOTE: odify the subset at the end for changes of start dates
# use for both Shearwater and Nerd
# dives2 <- left_join(divesS, head, by = "Dive.Number") %>%
#   left_join(., divesN[,c('Time.s', 'Dive.Number', 'Date', 'External O2 Sensor 4 (mV)', 'External O2 Sensor 5 (mV)')],
#             by = c('Time.s', 'Dive.Number', 'Date')) %>%
#   mutate(Run.Time = Start.Dttm + Time.s)
# %>%
#  subset(Start.Dttm > "2022-08-22")
# Use for Shearwater only
dives2 <- left_join(divesS, head, by = "Dive.Number") %>%
  mutate(Run.Time = Start.Dttm + Time.s) %>%
  arrange(desc(Dive.Number))
  
  

write.csv(dives2, "/Users/jking/Library/Mobile Documents/com~apple~CloudDocs/Projects/Diving/Shearwater.csv",
          row.names = FALSE)



# run a linear model on the 5 sensors
S1.Model <- lm(`External O2 Sensor 1 (mV)` ~ Depth + Time.s + `Fraction O2` + `Water Temp` +
                  Depth*Time.s + Depth*`Fraction O2` + Depth*`Water Temp` +
                  Time.s*`Fraction O2` + Time.s*`Water Temp` +
                  `Fraction O2`*`Water Temp`,
                data=dives2)
S2.Model <- lm(`External O2 Sensor 2 (mV)` ~ Depth + Time.s + `Fraction O2` + `Water Temp` +
                 Depth*Time.s + Depth*`Fraction O2` + Depth*`Water Temp` +
                 Time.s*`Fraction O2` + Time.s*`Water Temp` +
                 `Fraction O2`*`Water Temp`,
               data=dives2)
S3.Model <- lm(`External O2 Sensor 3 (mV)` ~ Depth + Time.s + `Fraction O2` + `Water Temp` +
                 Depth*Time.s + Depth*`Fraction O2` + Depth*`Water Temp` +
                 Time.s*`Fraction O2` + Time.s*`Water Temp` +
                 `Fraction O2`*`Water Temp`,
               data=dives2)
# S4.Model <- lm(`External O2 Sensor 4 (mV)` ~ Depth + Time.s + `Fraction O2` + `Water Temp` +
#                  Depth*Time.s + Depth*`Fraction O2` + Depth*`Water Temp` +
#                  Time.s*`Fraction O2` + Time.s*`Water Temp` +
#                  `Fraction O2`*`Water Temp`,
#                data=dives2)
# S5.Model <- lm(`External O2 Sensor 5 (mV)` ~ Depth + Time.s + `Fraction O2` + `Water Temp` +
#                  Depth*Time.s + Depth*`Fraction O2` + Depth*`Water Temp` +
#                  Time.s*`Fraction O2` + Time.s*`Water Temp` +
#                  `Fraction O2`*`Water Temp`,
#                data=dives2)

# Look at residual plots
plot(S1.Model, which=1, col=c("blue"))
plot(S2.Model, which=1, col=c("blue"))
plot(S3.Model, which=1, col=c("blue"))
# plot(S4.Model, which=1, col=c("blue"))
# plot(S5.Model, which=1, col=c("blue"))
plot(S1.Model, which=2, col=c("red"))
plot(S2.Model, which=2, col=c("red"))
plot(S3.Model, which=2, col=c("red"))
# plot(S4.Model, which=2, col=c("red"))
# plot(S5.Model, which=2, col=c("red"))
plot(S1.Model, which=3, col=c("blue"))
plot(S2.Model, which=3, col=c("blue"))
plot(S3.Model, which=3, col=c("blue"))
# plot(S4.Model, which=3, col=c("blue"))
# plot(S5.Model, which=3, col=c("blue"))

# basic stats on the residuals
S1.Res <- as.data.frame(S1.Model[["residuals"]]) %>%
  rename(., S1.Residuals = `S1.Model[["residuals"]]`)
S2.Res <- as.data.frame(S2.Model[["residuals"]]) %>%
  rename(., S2.Residuals = `S2.Model[["residuals"]]`)
S3.Res <- as.data.frame(S3.Model[["residuals"]]) %>%
  rename(., S3.Residuals = `S3.Model[["residuals"]]`)
# S4.Res <- as.data.frame(S4.Model[["residuals"]]) %>%
#   rename(., S4.Residuals = `S4.Model[["residuals"]]`)
# S5.Res <- as.data.frame(S5.Model[["residuals"]]) %>%
#   rename(., S5.Residuals = `S5.Model[["residuals"]]`)
Residuals1_3 <- cbind(S1.Res, S2.Res, S3.Res)
# Residuals4_5 <- cbind(S4.Res, S5.Res)

stat.desc(Residuals1_3)
# stat.desc(Residuals4_5)

dives2 <- dives2[order(dives2$Dive.Number, decreasing = TRUE),]

pct_plot <- dives2 %>%
  ggplot() +
  geom_line(aes(x = `Average PPO2`, y = `External O2 Sensor 1 (mV)`, color="red")) +
  geom_line(aes(x = `Average PPO2`, y = `External O2 Sensor 2 (mV)`, color="blue")) +
  geom_line(aes(x = `Average PPO2`, y = `External O2 Sensor 3 (mV)`, color="green")) +
  # geom_line(aes(x = `Average PPO2`, y = `External O2 Sensor 4 (mV)`, color="yellow")) +
  # geom_line(aes(x = `Average PPO2`, y = `External O2 Sensor 5 (mV)`, color="black")) +
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5))
print(pct_plot + labs(title= "Current As a Function of PPO2",
                      y="MV", x = "PPO2"))









