library(tidyverse)
library(reshape2)
library(zoo)
library(pastecs)
library(broom)

fitted_models <- dives2 %>% 
  group_by(Dive.Number) %>% 
  do(model = lm(`External O2 Sensor 2 (mV)` ~ `Average PPO2`, data = .))

pct_plot <- dives2 %>%
  filter(Dive.Number == 797) %>%
  ggplot() +
  geom_line(aes(x = `Average PPO2`, y = `External O2 Sensor 1 (mV)`, color="red")) +
  geom_line(aes(x = `Average PPO2`, y = `External O2 Sensor 2 (mV)`, color="blue")) +
  geom_line(aes(x = `Average PPO2`, y = `External O2 Sensor 3 (mV)`, color="green")) +
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5))
print(pct_plot + labs(title= "Current As a Function of PPO2",
                      y="mV", x = "PPO2"))

fitted_models <- dives2 %>% 
  group_by(Dive.Number) %>% 
  do(data.frame(., as.list(augment(lm(`External O2 Sensor 2 (mV)` ~ `Average PPO2`, data = .), data = .)))) %>%
  mutate(bad = abs(.resid) >= 1)

temp <- fitted_models %>%
  group_by(Dive.Number) %>% 
  summarize(bad = sum(abs(.resid) >= 10))
temp1 <- fitted_models %>%
  filter(Dive.Number == 798) %>% 
  select(., c(Dive.Number, External.O2.Sensor.2..mV., Average.PPO2, .resid, bad))
# after looking at dives 795-798 you can see cell 2 degrade it should have the biggest error
# note that it's slope looks OK, need to look at the residuals

# Sensor 1
S1 <- dives2 %>% 
  group_by(Dive.Number) %>% 
  do(data.frame(., as.list(coef(lm(`External O2 Sensor 1 (mV)` ~ `Average PPO2`, data = .))))) %>% 
  distinct(Date, Dive.Number, X.Intercept., X.Average.PPO2.) %>%
  rename(., S1.Intercept = X.Intercept.,
         S1.Slope = X.Average.PPO2.)
S1$S1.lagPPO2 <- dplyr::lag(S1$S1.Slope, n=1) 
S1$S1.slope_diff <- (S1$S1.lagPPO2 - S1$S1.Slope)/S1$S1.lagPPO2
S1$S1.Bad <- (S1$S1.slope_diff > 0.25)

# Sensor 2
S2 <- dives2 %>% 
  group_by(Dive.Number) %>% 
  do(data.frame(., as.list(coef(lm(`External O2 Sensor 2 (mV)` ~ `Average PPO2`, data = .))))) %>% 
  distinct(Date, Dive.Number, X.Intercept., X.Average.PPO2.) %>%
  rename(., S2.Intercept = X.Intercept.,
         S2.Slope = X.Average.PPO2.)
S2$S2.lagPPO2 <- dplyr::lag(S2$S2.Slope, n=1) 
S2$S2.slope_diff <- (S2$S2.lagPPO2 - S2$S2.Slope)/S2$S2.lagPPO2
S2$S2.Bad <- (S2$S2.slope_diff > 0.25)

# Sensor 3
S3 <- dives2 %>% 
  group_by(Dive.Number) %>% 
  do(data.frame(., as.list(coef(lm(`External O2 Sensor 3 (mV)` ~ `Average PPO2`, data = .))))) %>% 
  distinct(Date, Dive.Number, X.Intercept., X.Average.PPO2.) %>%
  rename(., S3.Intercept = X.Intercept.,
         S3.Slope = X.Average.PPO2.)
S3$S3.lagPPO2 <- dplyr::lag(S3$S3.Slope, n=1) 
S3$S3.slope_diff <- (S3$S3.lagPPO2 - S3$S3.Slope)/S3$S3.lagPPO2
S3$S3.Bad <- (S3$S3.slope_diff > 0.25)

# Sensor 4
# S4 <- dives2 %>% 
#   filter(.,!is.na(`External O2 Sensor 4 (mV)`)) %>%
#   group_by(Dive.Number) %>% 
#   do(data.frame(., as.list(coef(lm(`External O2 Sensor 4 (mV)` ~ `Average PPO2`, data = .))))) %>% 
#   distinct(Date, Dive.Number, X.Intercept., X.Average.PPO2.) %>%
#   rename(., S4.Intercept = X.Intercept.,
#          S4.Slope = X.Average.PPO2.)
# S4$S4.lagPPO2 <- dplyr::lag(S4$S4.Slope, n=1) 
# S4$S4.slope_diff <- (S4$S4.lagPPO2 - S4$S4.Slope)/S4$S4.lagPPO2
# S4$S4.Bad <- (S4$S4.slope_diff > 0.25)

# Sensor 5
# S5 <- dives2 %>% 
#   filter(.,!is.na(`External O2 Sensor 5 (mV)`)) %>%
#   group_by(Dive.Number) %>% 
#   do(data.frame(., as.list(coef(lm(`External O2 Sensor 5 (mV)` ~ `Average PPO2`, data = .))))) %>% 
#   distinct(Date, Dive.Number, X.Intercept., X.Average.PPO2.) %>%
#   rename(., S5.Intercept = X.Intercept.,
#          S5.Slope = X.Average.PPO2.)
# S5$S5.lagPPO2 <- dplyr::lag(S5$S5.Slope, n=1) 
# S5$S5.slope_diff <- (S5$S5.lagPPO2 - S5$S5.Slope)/S5$S5.lagPPO2
# S5$S5.Bad <- (S5$S5.slope_diff > 0.25)


