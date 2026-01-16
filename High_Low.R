library(tidyverse)
library(reshape2)
library(zoo)
library(pastecs)
library(broom)

tmp <- dives2 %>% group_split(Dive.Number) %>% 
  map_dfr(~bind_cols(select(.x), augment(lm(`External O2 Sensor 1 (mV)` ~ `Average PPO2`, data=.x))), .id = "Dive.Number")
S1L <- dives2 %>%
  filter(., `Average PPO2` <= 1.3) %>%
  group_by(Dive.Number) %>%
  group_modify(~ broom::tidy(lm(`External O2 Sensor 1 (mV)` ~ `Average PPO2`, data = .))) %>%
  filter(.,term == "`Average PPO2`") %>%
  select(., c(Dive.Number, estimate, std.error)) %>%
  rename(S1L.Parm = estimate,
         S1L.Stderr = std.error)
S1H <- dives2 %>%
  filter(., `Average PPO2` >= 1.3) %>%
  group_by(Dive.Number) %>%
  group_modify(~ broom::tidy(lm(`External O2 Sensor 1 (mV)` ~ `Average PPO2`, data = .))) %>%
  filter(.,term == "`Average PPO2`") %>%
  select(., c(Dive.Number, estimate, std.error)) %>%
  rename(S1H.Parm = estimate,
         S1H.Stderr = std.error)
S1A <- full_join(S1L, S1H) %>%
  mutate(Z = (S1L.Parm - S1H.Parm)/sqrt(S1L.Stderr^2 + S1H.Stderr^2),
         Bad = (S1H.Parm - S1L.Parm)/S1L.Parm < -0.2)




