library(tidyverse)
library(janitor)



### WEATHER ###
  
# import weather data provided
raw_weather <- read_csv("raw_data/weather.csv")
  
# supplement with data obtained from
# https://power.larc.nasa.gov/data-access-viewer/
jfk_weather <- read_csv("raw_data/JFK_weather.csv") %>% clean_names()
lga_weather <- read_csv("raw_data/LGA_weather.csv") %>% clean_names()
ewr_weather <- read_csv("raw_data/EWR_weather.csv") %>% clean_names()
  
# add origin airport columns
ewr_weather <- ewr_weather %>% 
  mutate(origin = "EWR", .before = year)

jfk_weather <- jfk_weather %>% 
  mutate(origin = "JFK", .before = year)
  
lga_weather <- lga_weather %>% 
  mutate(origin = "LGA", .before = year)
  
# bind sourced data, tidy columns and names
nyc_weather <- 
  bind_rows(ewr_weather, jfk_weather, lga_weather) %>% 
  filter(year == 2017) %>% 
  mutate(time_hour = make_datetime(year, mo, dy, hr)) %>% 
  select(-c(year, mo, dy, hr)) %>% 
  rename(temp = t2m,
         dewpoint = t2mdew,
         humidity = rh2m,
         precip = prectotcorr,
         pressure = ps,
         wind_speed = ws50m,
         wind_dir = wd50m)

# join visibility from original data
all_weather <- nyc_weather %>% 
  left_join(raw_weather %>% select(origin, time_hour, visib),
            by = c("origin", "time_hour")) %>% 
  relocate(visib, .before = time_hour) %>% 
    
  # fill missing visibility with previous or next reading if possible
  mutate(visib = coalesce(visib, lag(visib, n = 1))) %>% 
  mutate(visib = coalesce(visib, lead(visib, n = 1))) %>% 
    
  # fill remaining missing visibility with median for day
  mutate(date = date(time_hour)) %>% 
  group_by(origin, date) %>% 
  mutate(visib = coalesce(visib, median(visib, na.rm = TRUE))) %>% 
  ungroup() %>% 
  select(-date)
  
# Write clean data
write_csv(all_weather, file = "clean_data/weather.csv")


  
### FLIGHTS ###
flights <- read_csv("raw_data/flights.csv")

flights_small <- flights %>% 
  # Select columns concerned with departures
  select(dep_time, sched_dep_time, dep_delay, carrier, flight, tailnum, origin, dest, time_hour) %>% 
  
  # Fill in some missingness
  mutate(dep_delay = case_when(dep_time == sched_dep_time ~ 0,
                               .default = dep_delay),
         # Add flag for whether the flight is delayed 15 mins or more
         delayed_flag = factor(ifelse(dep_delay >= 15, 1, 0)), 
         # Add flag for whether the flight is cancelled
         cancelled_flag = factor(ifelse(is.na(dep_delay) == TRUE, 1, 0)), 
         # Add flag for whether the flight is disrupted
         disrupted_flag = factor(ifelse(delayed_flag == 1 | cancelled_flag == 1, 1, 0)),
         .before = carrier)

# Write clean data
write_csv(flights_small, "clean_data/flights.csv")



### AIRPORTS ###
airports <- read_csv("raw_data/airports.csv")

puerto_rico <- tibble(
  faa = c("SJU", "BQN", "STT", "PSE"),
  name = c("Luis Muñoz Marín International Airport",
           "Rafael Hernández International Airport",
           "Cyril E. King Airport", "Mercedita International Airport"),
  lat = c(18.4378, 18.4954, 18.3361, 18.0106),
  lon = c(-66.0042, -67.1356, -64.9723, -66.5632)
)

# Select columns concerned with geospatial and add Puerto Rican airports
airports_small <- airports %>% 
  select(faa, name, lat, lon) %>% 
  bind_rows(puerto_rico)

# rename lat and lon for origin/destination
dest_airports <- airports_small %>% 
  rename(dest_lat = lat,
         dest_lon = lon,
         dest_name = name)

origin_airports <- airports_small %>%
  rename(origin_lat = lat,
         origin_lon = lon) %>% 
  filter(faa %in% c("EWR", "JFK", "LGA"))

# Write clean data
write_csv(dest_airports, "clean_data/dest_airports.csv")
write_csv(origin_airports, "clean_data/origin_airports.csv")



### PLANES ###
planes <- read_csv("raw_data/planes.csv")

# Reduce columns slightly
planes_small <- planes %>% 
  select(tailnum, year, type, model, seats, engines, engine)

# Write clean data
write_csv(planes_small, "clean_data/planes.csv")




### AIRLINES ###
airlines <- read_csv("raw_data/airlines.csv") %>% 
  rename(airline = name)

write_csv(airlines, "clean_data/airlines.csv")




### EXTREME WEATHER EVENTS ###
weather_events <- read_csv("raw_data/NWS New York Significant Weather Events 2017.csv") %>% 
  mutate(date = dmy(str_c(day, "-", month, "-", year))) %>% 
  select(date, event)

write_csv(weather_events, "clean_data/weather_events.csv")



### SNOWFALL ###
snow_files <- list.files("raw_data/snow/")

snowfall <- tibble()

for (i in 1:length(snow_files)) {
  this_month <-
    read_csv(str_c("../final_project/raw_data/snow/", snow_files[i]), skip = 1) %>% 
    janitor::clean_names() %>% 
    filter(station_name %in% c("JFK INTERNATIONAL AIRPORT",
                               "LAGUARDIA AIRPORT",
                               "NEWARK LIBERTY INTL AP")) %>% 
    select(-c(ghcn_id, county, state, elevation, latitude, longitude)) %>% 
    rename(origin = station_name) %>% 
    mutate(origin = replace(origin, origin == "JFK INTERNATIONAL AIRPORT", "JFK"),
           origin = replace(origin, origin == "LAGUARDIA AIRPORT", "LGA"),
           origin = replace(origin, origin == "NEWARK LIBERTY INTL AP", "EWR")) %>% 
    pivot_longer(cols = seq(2, ncol(.), 1), names_to = "date", values_to = "snow") %>% 
    mutate(snow = replace(snow, snow == "T", 0), # T stands for trace amount
           snow = as.numeric(snow)) %>% 
    mutate(separate_wider_delim(., cols = date, delim = "_", names = c("month", "day"))) %>% 
    mutate(date = ymd(str_c("2017", month, date))) %>% 
    select(origin, date, snow)
  
  snowfall <- bind_rows(this_month, snowfall)
}

write_csv(snowfall, file = "clean_data/snowfall.csv")