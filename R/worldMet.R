

# Objective: calculate average cooling degree days (CDD) and heating degee days (HDD) by month for Perth, Western Australia

library(lubridate)
library(purrr)
library(tidyverse)
library(worldmet)
getMeta(lat = -31.95224, lon = 115.8614) # check the latitude and longitude, should indicate Perth, Western Australia & obtain weather station code

year <- seq(from = 2018, to = 2022, by = 1)

data <- map(year, worldmet::importNOAA, code = "946100-99999")
df <- data %>% bind_rows() %>%
  mutate( Calendar.Year = lubridate::year(date),
          Calendar.Month = lubridate::month(date),
          Calendar.Day = lubridate::day(date))  %>% 
  group_by(Calendar.Year, Calendar.Month,Calendar.Day) %>% 
    summarise(max.temp = max(air_temp, na.rm = TRUE),
              min.temp = min(air_temp, na.rm = TRUE)) %>% 
  mutate(CDD = ifelse(max.temp - 18 < 0,0, max.temp - 18),
         HDD = ifelse(18 - min.temp < 0, 0, 18 - min.temp)) %>% 
  group_by(Calendar.Year, Calendar.Month) %>% 
  summarise(CDD = sum(CDD),
            HDD = sum(HDD)) %>% 
  group_by(Calendar.Month) %>%
  summarise(CDD = mean(CDD),
         HDD = mean(HDD))


write.csv(df, file = './data/CDD_HDD.csv')
