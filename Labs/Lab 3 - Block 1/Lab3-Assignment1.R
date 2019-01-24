set.seed(1234567890)
library(geosphere)

stations <- read.csv("stations.csv",fileEncoding = "Latin1")
temps <- read.csv("temps50k.csv")

st <- merge(stations,temps,by="station_number")
  h_distance <- 1000000 # These three values are up to the students
  h_date <- 100
  h_time <- 200
 
a <- 58.4274 # The point to predict (up to the students)
b <- 14.826

date <- "2013-11-04" # The date to predict (up to the students)

times <- c("04:00:00", "06:00:00", "08:00:00","10:00:00",
           "12:00:00" ,"14:00:00", "16:00:00","18:00:00",
           "20:00:00","22:00:00","24:00:00")

temp <- vector(length=length(times))
temp_Multi <- vector(length=length(times))
  # Students' code here

point_intreset <- c(a,b)

gaussion_distance <- function(dist)
{
  return(exp(-dist^2))
}

d_stations <- function(db_point, distance_POI)
{
  dist <- distHaversine(db_point, distance_POI)
  return(gaussion_distance(dist / h_distance))
}

d_date <- function(db_point, date_POI)
{
  date_diff <- as.numeric(difftime(db_point,date_POI,unit = "days"))
  
  date_diff = abs(date_diff)
  date_diff[date_diff > 182] = 365 - date_diff[date_diff > 182]
  
  return(gaussion_distance(date_diff / h_date))
}

d_hour <- function(db_point, hour_POI)
{
  hour_diff <- as.numeric(difftime(db_point,hour_POI,unit = "hours"))
  
  hour_diff = abs(hour_diff)
  hour_diff[hour_diff > 12] = 24 - hour_diff[hour_diff > 12]
  
  return(gaussion_distance(hour_diff / h_time))
}

kernal_sum <- function(Original_data, POI,index)
{
  Original_data = fix_time(Original_data)
  POI = fix_time(POI)
  
  data_dist  = Original_data[,c("longitude", "latitude")]
  obs_dist   = c(POI$longitude, POI$latitude)
  
  data_date = Original_data$date
  obs_date  = POI$date
  
  data_time = Original_data$time
  obs_time  = POI$time
  
  # Calcualte kernels Sum
  kernal_stations = d_stations(data_dist,obs_dist)
  kernal_date = d_date(data_date,obs_date)
  kernal_hour = d_hour(data_time,obs_time)
  
  dist = kernal_stations + kernal_date + kernal_hour
  
  Original_data$distance = dist
  Original_data$data_dist = data_dist
  Original_data$data_date = data_date
  Original_data$data_time = data_time
  
  selection = Original_data
  
  return(sum(selection$distance * selection$air_temperature) / sum(selection$distance))
}

kernal_multiply <- function(Original_data, POI,index)
{
  Original_data = fix_time(Original_data)
  POI = fix_time(POI)
  
  data_dist  = Original_data[,c("longitude", "latitude")]
  obs_dist   = c(POI$longitude, POI$latitude)
  
  data_date = Original_data$date
  obs_date  = POI$date
  
  data_time = Original_data$time
  obs_time  = POI$time
  
  # Calcualte kernels Multiply
  kernal_stations = d_stations(data_dist,obs_dist)
  kernal_date = d_date(data_date,obs_date)
  kernal_hour = d_hour(data_time,obs_time)
  
  dist = kernal_stations * kernal_date * kernal_hour
  
  Original_data$distance = dist
  Original_data$data_dist = data_dist
  Original_data$data_date = data_date
  Original_data$data_time = data_time
  
  selection = Original_data
  
  return(sum(selection$distance * selection$air_temperature) / sum(selection$distance))
}

fix_time = function(data){
  data$time = as.POSIXct(data$time,format="%H:%M:%S")
  data$date = sub('\\d{4}(?=-)', '2016', data$date, perl=TRUE)
  return(data)
}

n = length(times)
data = data.frame(date=rep(date,n/length(date)), time=rep(times,n/length(times)), longitude=rep(a,n), latitude=rep(b,n))

for(i in 1:nrow(data))
{
  temp[i] <- kernal_sum(st, data[i,],i) 
}
print(temp)
plot(temp, type="o")

for(i in 1:nrow(data))
{
  temp_Multi[i] <- kernal_multiply(st, data[i,],i)
}
print(temp_Multi)
plot(temp_Multi, type="o")
