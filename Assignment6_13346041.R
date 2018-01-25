#load dplyr
library(dplyr)

#create tibble with wave data
wave <- read_csv("Wave.csv")

#create tibble with weather data
weather <- read_csv("Weather.csv")

#create tibble of average hourly windspeed by piping
wind_speed_hrly <- weather %>% group_by(Date) %>%
  group_by(Hour, add=T) %>% summarise(WindSpeedAveragePerHour = mean(`Wind Speed (m/s)`))

#create tibble of average hourly wave height by piping
wave_height_hrly <- wave %>% group_by(Date) %>%
  group_by(Hour, add=T) %>% summarise(WaveHeightAverage = mean(Havg_m))

#join previous two tibbles together by hour
tib <- full_join(wave_height_hrly,wind_speed_hrly)

#plot wave height against wind speed
ggplot(tib) +
  geom_point(mapping= aes(WindSpeedAveragePerHour,WaveHeightAverage))+
  geom_smooth(mapping= aes(WindSpeedAveragePerHour,WaveHeightAverage),method = "lm")+
  xlab("Wind Speed (average per hour) m/s") + ylab("Average Wave Height (per hour) m")
