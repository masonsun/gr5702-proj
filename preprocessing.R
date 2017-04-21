############################ Dependencies ############################
library(tidyverse)
library(lubridate)
library(leaflet)
library(RJSONIO)
library(gdata)
library(plotly)

############################ Load Data  ############################

indego_stations <- read.csv("./data/indego_stations.csv", header=T)
Y16Q4 <- read.csv("../data/Indego_Trips_2016Q4.csv", header=T)
Y16Q3 <- read.csv("../data/Indego_Trips_2016Q3.csv", header=T)
Y16Q2 <- read.csv("../data/Indego_Trips_2016Q2.csv", header=T)
Y16Q1 <- read.csv("../data/Indego_Trips_2016Q1.csv", header=T)
Y15Q4 <- read.csv("../data/Indego_Trips_2015Q4.csv", header=T)
Y15Q3 <- read.csv("../data/Indego_Trips_2015Q3.csv", header=T)
Y15Q2 <- read.csv("../data/Indego_Trips_2015Q2.csv", header=T)

########################## Preprocessing  ##########################

# Build Station Location Data Frame

station_loc <- Y16Q4 %>% dplyr::select(start_station_id, start_lat, start_lon) %>% unique
station_loc <- dplyr::left_join(station_loc, indego_stations, by = c("start_station_id" = "Station.ID")) %>% na.omit %>% .[, -6]
colnames(station_loc) <- c("id", "lat", "long", "name", "date")
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
station_loc$name <- trim(station_loc$name)
station_loc$date <- trim(station_loc$date %>% as.character())

# Parse Date and Time

parseTime_15 <- function(df) {
  df$s_hour <- format(as.POSIXct(strptime(df$start_time,"%m/%d/%y %H:%M",tz="")) ,format = "%H")
  df$s_min <- format(as.POSIXct(strptime(df$start_time,"%m/%d/%y %H:%M",tz="")) ,format = "%M")
  df$e_hour <- format(as.POSIXct(strptime(df$end_time,"%m/%d/%y %H:%M",tz="")) ,format = "%H")
  df$e_min <- format(as.POSIXct(strptime(df$end_time,"%m/%d/%y %H:%M",tz="")) ,format = "%M")
  df$year <- format(as.POSIXct(strptime(df$start_time,"%m/%d/%y %H:%M",tz="")) ,format = "%Y")
  df$month <- format(as.POSIXct(strptime(df$start_time,"%m/%d/%y %H:%M",tz="")) ,format = "%m")
  df$day <- format(as.POSIXct(strptime(df$start_time,"%m/%d/%y %H:%M",tz="")) ,format = "%d")
  df$date <- format(as.POSIXct(strptime(df$start_time,"%m/%d/%y %H:%M",tz="")) ,format = "%Y-%m-%d")
  return(df)
}
parseTime_16 <- function(df) {
  df$s_hour <- format(as.POSIXct(strptime(df$start_time,"%m/%d/%Y %H:%M",tz="")) ,format = "%H")
  df$s_min <- format(as.POSIXct(strptime(df$start_time,"%m/%d/%Y %H:%M",tz="")) ,format = "%M")
  df$e_hour <- format(as.POSIXct(strptime(df$end_time,"%m/%d/%Y %H:%M",tz="")) ,format = "%H")
  df$e_min <- format(as.POSIXct(strptime(df$end_time,"%m/%d/%Y %H:%M",tz="")) ,format = "%M")
  df$year <- format(as.POSIXct(strptime(df$start_time,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y")
  df$month <- format(as.POSIXct(strptime(df$start_time,"%m/%d/%Y %H:%M",tz="")) ,format = "%m")
  df$day <- format(as.POSIXct(strptime(df$start_time,"%m/%d/%Y %H:%M",tz="")) ,format = "%d")
  df$date <- format(as.POSIXct(strptime(df$start_time,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
  return(df)
}
Y15Q2 <- parseTime_15(Y15Q2)
Y15Q3 <- parseTime_15(Y15Q3)
Y15Q4 <- parseTime_15(Y15Q4)
Y16Q1 <- parseTime_16(Y16Q1)
Y16Q2 <- parseTime_16(Y16Q2)
Y16Q3 <- parseTime_16(Y16Q3)
Y16Q4 <- parseTime_16(Y16Q4)

df <- rbind(Y15Q2, Y15Q3, Y15Q4, Y16Q1, Y16Q2, Y16Q3, Y16Q4)
rm(Y15Q2, Y15Q3, Y15Q4, Y16Q1, Y16Q2, Y16Q3, Y16Q4)

# Begin Data Cleaning
dataClean <- function(df) {
  df$duration <- df$duration / 60
  df[, -c(1, 3, 4, 6, 7, 9, 10, 12)]
}
df <- dataClean(df)

write.table(df, "../data/cleanData.txt")
write.table(station_loc, "../data/station_loc")


# Save Environment

save <- TRUE
if(save == TRUE) {
  save.image(file="../data/clean_indego.RData")
}
