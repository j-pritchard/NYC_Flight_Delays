library(tidyverse)

### DATA-JOINS ###

airlines <- read_csv("clean_data/airlines.csv")
origin <- read_csv("clean_data/origin_airports.csv")
destination <- read_csv("clean_data/dest_airports.csv")
flights<- read_csv("clean_data/flights.csv")
weather <- read_csv("clean_data/weather.csv")
snowfall <- read_csv("clean_data/snowfall.csv")

# planes not joined due to concern over data accuracy

all_flight_data <- flights %>% 
  left_join(weather, by = c("origin", "time_hour")) %>% 
  left_join(airlines, by = "carrier") %>% 
  left_join(origin, by = c("origin" = "faa")) %>% 
  left_join(destination, by = c("dest" = "faa")) %>% 
  mutate(date = date(time_hour)) %>% 
  left_join(snowfall, by = c("origin", "date")) %>% 
  select(time_hour, date, sched_dep_time, dep_time, dep_delay,
         delayed_flag, cancelled_flag, disrupted_flag,
         temp, dewpoint, humidity, precip, pressure,
         wind_speed, wind_dir, snow, visib,
         origin, origin_lat, origin_lon,
         dest, dest_name, dest_lat, dest_lon,
         carrier, flight, airline, tailnum) %>% 
  arrange(origin, time_hour, sched_dep_time)

write_csv(all_flight_data, "clean_data/all_flight_data.csv")


# subset for modelling weather influences
ewr_flights_weather <- all_flight_data %>% 
  filter(origin == "EWR") %>% 
  mutate(disrupted_flag = factor(disrupted_flag, levels = c(0, 1), labels = c("Not Disrupted", "Disrupted")),
         hour = hour(time_hour)) %>% 
  select(temp, dewpoint, humidity, precip, pressure,
         wind_speed, wind_dir, visib, snow, disrupted_flag)

write_csv(ewr_flights_weather, "clean_data/ewr_flights_weather.csv")


# subset for modelling weather influences and other predictors
ewr_flights_weather_extra <- all_flight_data %>% 
  filter(origin == "EWR") %>% 
  mutate(disrupted_flag = factor(disrupted_flag, levels = c(0, 1), labels = c("Not Disrupted", "Disrupted")),
         hour = hour(time_hour)) %>% 
  select(hour, airline, temp, dewpoint, humidity, precip, pressure,
         wind_speed, wind_dir, visib, snow, disrupted_flag)

write_csv(ewr_flights_weather, "clean_data/ewr_flights_weather.csv")