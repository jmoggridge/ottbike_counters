# URL pieces for Ottawa Airport #49568
prefix <-  "https://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID="
station.id <- as.character(49568)
prefix <- paste(prefix, station.id,"&Year=", sep='')
suffix <-  "&Month=12&Day=14&timeframe=2&submit=Download+Data"

# grab data
weathers <- list()
years <- c(seq(2010,2019))
for (x in c(seq(length(years)))){
  year <- years[x]
  address <- paste(prefix, year, suffix, sep='')
  name <- paste('station',station.id, year,sep='_')
  GET(address, write_disk(tf <- tempfile(fileext = ".csv")))
  df <- read_csv(tf)
  weathers[[x]] <- df
}
names(weathers) <- as.character(years)
summary(weathers)

# concatenate dataframes
weather <- as.data.frame(weathers[1])
names <- names(weather)
for (year in weathers[-c(1)]){
  x <-  as.data.frame(year)
  names(x) <- names
  weather <- rbind(weather, x)
}
weather <- weather %>%
  select(c(5,10,12,14,24,28,30))
names(weather) <- c("Date", "Tmax", "Tmin", "Tmean", "Precip", "Wind.deg","Wind.speed")

weather <- weather %>%
  mutate(
    julian = yday(date),
    Precip = replace_na(Precip, 0),
    Wind.speed = str_replace_all(Wind.speed, '<', ''),
    Wind.speed = as.numeric(Wind.speed) - 30,
    Wind.speed = replace_na(Wind.speed, 0.1),
    Wind.deg = Wind.deg * 10,
    Wind.x = cos(Wind.deg/180 * pi),
    Wind.x = replace_na(Wind.x, 0),
    Wind.y = sin(Wind.deg/180 * pi),
    Wind.y = replace_na(Wind.y, 0),
    R.d.since = 0,
    julian = yday(date))
#

for (i in seq(1:nrow(weather))){
  if (i == 1){
    if (weather$Precip[i] <= 0.5){
      weather$R.d.since[i] <- 1
    }
    else {weather$R.d.since[i] <- 0}
  }
  else {
    if (weather$Precip[i] <= 0.5){
      weather$R.d.since[i] <- weather$R.d.since[i-1] + 1
    } else {weather$R.d.since[i] <- 0}
  }
}

weather <- weather %>%
  select(order(colnames(.))) %>%
  as_tsibble(index = date)

summary(weather)
# weather %>% autoplot(Tmean)
# weather %>% gg_season(Tmean)
# weather %>% gg_season(Tmax)
# weather %>% gg_season(Tmin)
# weather %>% gg_season(R.d.since)
# weather %>% gg_season(Precip)
# weather %>% autoplot(Wind.speed)
# plot(weather[, c("Wind.deg","Wind.x", "Wind.y")])
#
saveRDS(weather, file = 'data/weather.ts.RDS')

bikes <- readRDS("data/ottbike_counters.ts.RDS") %>%
  mutate(location = as.factor(location)) %>%
  pivot_wider(id_cols = "Date",
              names_from = "location",
              values_from = 'count')
bike.weather <- left_join(bikes, weather, index = "Date")


saveRDS(weather,"data/bike.weather.long.RDS")
rm(list = ls())
