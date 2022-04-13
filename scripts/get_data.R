setwd("X:/Spatial Stat/climate_cholera/")
#Countries to consider: 
# 
rm(list= ls())
library(rnoaa)
library(data.table)
library(ggplot2)
library(sf)
library(raster)
library(rgdal)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(precintcon)

theme_set(theme_minimal())
theme_update(legend.position = "bottom")
options(ggplot2.continuous.colour="viridis")
options(ggplot2.continuous.fill = "viridis")
par(mar=c(1,1,1,1))


## Countries to consider: 9 as suggested by Dr. Anyamba
country_vector<- c('Kenya', 'Uganda', 'Tanzania', 'South Sudan', 'Somalia', 
                   'Ethiopia', 'Zambia', 'Malawi', 
                   'DRC')

# great_lakes<- c( 'Democratic Republic of the Congo', 'Ethiopia', 'Kenya', 'Malawi', 
#                  'Mozambique', 'Rwanda', 'Zambia', 'Tanzania', 'Uganda')

# read cholera data
chl_dt_raw<- fread("X:/Spatial Stat/climate_cholera/data/Africa_El_Nino_Cases_April 2015-present.csv")
# view cholera data by country
chl_dt_raw[Disease=="CHL", .N , by= Country]
chl_dt_raw[Country=="Democratic Republic of the Congo", Country:="DRC"]
# getting cholera data
chl_dt<- chl_dt_raw[Disease=="CHL", ]
# subset target country data
chl_dt<- chl_dt[Country %in% country_vector,]


# format date and confirmed cases 
chl_dt$Start_Date<- as.Date(chl_dt$Start_Date, "%m/%d/%Y")
chl_dt$End_Date<- as.Date(chl_dt$End_Date, "%m/%d/%Y")
chl_dt$End_Date<- format(chl_dt$End_Date, "%Y-%m-%d")
chl_dt$End_Date<- as.Date(chl_dt$End_Date)

chl_dt$Confirmed_Cases<- as.numeric(chl_dt$Confirmed_Cases)

# plotting 
ggplot(data= chl_dt)+ 
  geom_point(aes(x= End_Date, y=log(Confirmed_Cases)))+ 
  facet_wrap(~Country)+ 
  labs(x= "Reporting date", y= 'Log of confirmed cholera cases')


# Point size by country plot --- maybe not useful here 
ggplot(data= chl_dt)+ 
  geom_point(aes(x= End_Date, y= Country, size= log(Confirmed_Cases)), pch= 2)


### map of cholera data






# read rainfall data as precipitation (mm)
# https://cran.r-project.org/web/packages/rnoaa/rnoaa.pdf
# read me of Africa Rainfall Climatology version2 (ARC2): https://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa/arc2/ARC2_readme.txt
tz.box <- c(xmin = -7, ymin = 33.4, xmax = -6, ymax = 37.2)
ea.box<- c(xmin = 10, ymin = -30, xmax = 55, ymax = 20)
date.frame<- seq(as.Date("2016-01-01"), as.Date("2016-06-30"), "days")

tz.rf<- arc2(date= "2017-06-14", box = tz.box, )
ea.rf<- arc2(date= "2017-06-14", box = ea.box)
ggplot(data= data.frame(ea.rf$`2017-06-14`), aes(x= lon, y= lat, color= precip))+geom_tile()

ea.rf.2017.06.14<- rasterFromXYZ(ea.rf$`2017-06-14`[,2:4], crs = "+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83")
plot(ea.rf.2017.06.14)
# To check if the rainfall data includes all target countries

taz.shp<- getData('GADM', country='TZA', level=0)
plot(taz.shp, add= T)
plot(getData('GADM', country='KEN', level=0), add= T)
plot(getData('GADM', country='UGA', level=0), add= T)
plot(getData('GADM', country='SSD', level=0), add= T)
plot(getData('GADM', country='SOM', level=0), add= T)
plot(getData('GADM', country='ETH', level=0), add= T)
plot(getData('GADM', country='ZMB', level=0), add= T)
plot(getData('GADM', country='MWI', level=0), add= T)
plot(getData('GADM', country='COD', level=0), add= T)




# tz.rf.16.1<- arc2(date= date.frame, box = tz.box, )

# tz.rf.16.1.df<- data.frame(lon= tz.rf.16.1[[1]]$lon, lat= tz.rf.16.1[[1]]$lat, sapply(tz.rf.16.1, function(x) x$precip))

# temp.df<- data.frame(lon= tz.rf[[1]]$lon, lat= tz.rf[[1]]$lat)
# date.frame<- seq(as.Date("2016-01-01"), as.Date("2016-01-31"), "days")
# for (i in date.frame){
#   i<- as.Date(i, origin = "1970-01-01")
#   print(i)
#   tz.temp<- arc2(date= i, box = tz.box)
#   temp.df<- data.frame(temp.df, sapply(tz.temp, function(x) x$precip))
#   colnames(temp.df)[ncol(temp.df)]<- paste0("d",substr(i, 9, 10))
#   file.remove(paste0("C:/Users/sikde/AppData/Local/cache/R/noaa_arc2/",list.files("C:/Users/sikde/AppData/Local/cache/R/noaa_arc2/")))
# }

# for now only write if for each year
source('scripts/helper_functions.R')

arc2015<-get_arc2(2015, area_box = c(xmin = 10, ymin = -30, xmax = 55, ymax = 20))
#ea.rf.2015.01.01<- rasterFromXYZ(arc2015[1:225951,], crs = "+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83")

arc2016<-get_arc2(2016)
arc2017<-get_arc2(2017)
arc2018<-get_arc2(2018)
arc2019<-get_arc2(2019)

arc2015.2019<- bind_rows(arc2015, arc2016, arc2017, arc2018, arc2019)


### ------------ get rainfall data as raster stack ----------------###
### Note: saving as raster is more memory efficient; 
### 2015 as dataframe= 859.3mb and as raster is 773.4mb
source('scripts/helper_functions.R')

arc2015_rast<- get_arc2_raster(2015, area_box = c(xmin = 10, ymin = -30, xmax = 55, ymax = 20))
arc2016_rast<- get_arc2_raster(2016, area_box = c(xmin = 10, ymin = -30, xmax = 55, ymax = 20))
arc2017_rast<- get_arc2_raster(2017, area_box = c(xmin = 10, ymin = -30, xmax = 55, ymax = 20))
arc2018_rast<- get_arc2_raster(2018, area_box = c(xmin = 10, ymin = -30, xmax = 55, ymax = 20))
arc2019_rast<- get_arc2_raster(2019, area_box = c(xmin = 10, ymin = -30, xmax = 55, ymax = 20))

### -------------- get cumulative rainfall ---------------------------###

cumulative_2015_sep_dec<- raster::calc(arc2015_rast[[244:365]], sum)
cumulative_2016_jan_may<- raster::calc(arc2016_rast[[1:115]], sum)
cumulative_2016_sep_dec<- raster::calc(arc2016_rast[[244:365]], sum)
cumulative_2017_jan_may<- raster::calc(arc2017_rast[[1:115]], sum)
cumulative_2017_sep_dec<- raster::calc(arc2017_rast[[244:365]], sum)
cumulative_2018_jan_may<- raster::calc(arc2018_rast[[1:115]], sum)
cumulative_2018_sep_dec<- raster::calc(arc2018_rast[[244:365]], sum)
cumulative_2019_jan_may<- raster::calc(arc2019_rast[[1:115]], sum)
cumulative_2019_sep_dec<- raster::calc(arc2019_rast[[244:365]], sum)




# monthly mean rainfall
mon.mean.2015.2019<- data.frame(arc2015.2019[,1:2] , mon.mean=rowMeans(arc2015.2019[,3:33], na.rm = T))
mon.mean.2015.2019$date<- rep(seq(as.Date("2015-01-01"), as.Date("2019-12-31"), "months"), times= 1, each= 429)
mon.mean.2015.2019.dt<- data.table(mon.mean.2015.2019)

# daily mean rainfall of the full area
arc2015.2019_dt<- data.table(arc2015.2019)
area.mean.2015.2019<- arc2015.2019_dt[,apply(.SD, 2, mean), .SDcols= patterns("d"), by= .(year, month) ]
colnames(area.mean.2015.2019)[3]<- "area.mean"
# remove NA caused by Feb 29, 30, 31 etc.
area.mean.2015.2019<- area.mean.2015.2019[complete.cases(area.mean.2015.2019),]
area.mean.2015.2019$date<- seq(as.Date("2015-01-01"), as.Date("2019-12-31"), "days")[c(1:424, 426:1826)]


# rainfall anomaly index
daily.obj2015<- as.daily(arc2015[,c(35, 34, 3:33)], na.value = NA)
rai.2015<-rai(daily.obj2015)


daily.obj<- as.daily(arc2015.2019, na.value = NA)
rai.2015.2019<-rai(daily.obj)
rai.2015.2019.dt<- data.table(rai.2015.2019)
rai.2015.2019.dt[,mean(rai), by= .(year, month)]
rai.2015.2019.dt$date<- rep(seq(as.Date("2015-01-01"), as.Date("2019-12-31"), "months"), times= 1, each= 429)

ggplot(data = rai.2015.2019.dt[,mean(rai), by= .(year, month)])+geom_line(aes(x= month, y= V1, color= year))

# can't take mean rai to match with cholera data because rai is monthly 
#mean.rai<- rai.2015.2019.dt[,rai.m:=mean(rai, na.rm= T), by= date]
#colnames(mean.rai)[2]<- "rai.m"

chl_tz<- chl_dt[Country=="Tanzania",]
merged.dt<- merge.data.table(area.mean.2015.2019, chl_tz, by.x = "date", by.y = "End_Date", all= TRUE )
# note: more rows then area.mean.2015.2019 becaue in the chl_tz multiple locations were included for the same date e.g.  2018-12-31 had five location so 5 rows for the same date


ggplot()+
  geom_line(data = rai.2015.2019.dt[,mean(rai), by= date], aes(x= date, y= V1, color= year))+ 
  geom_point(data= chl_dt[Country=="Tanzania",], aes(x= End_Date, y=log(Confirmed_Cases)))+
  geom_line(data = mon.mean.2015.2019.dt[, mean(mon.mean), by= date], aes(x=date, y= V1, color= year),size= 2 )+ 
  scale_x_date(date_minor_breaks = "1 month", date_breaks = "6 month", date_labels = "%Y-%b")+ 
  theme(axis.text= element_text(angle = 90), 
        legend.key.size = unit(3, "pt") )

# plot with rainfall moving average and lag 
rain_confirmed<- ggplot()+ 
  geom_point(data= merged.dt, aes(x= date, y= log(Confirmed_Cases)), size= 1.2, color= "#c09369") +
  geom_line(data= merged.dt, aes(x= date, y= lag(rollmean(area.mean, 7, na.pad = T), k= 5)), color= "turquoise")+ 
  scale_x_date(date_minor_breaks = "1 month", date_breaks = "6 month", date_labels = "%Y-%b")+ 
  scale_y_continuous(name= "log of confirmed cases", sec.axis = sec_axis(~ . * 1, name = "Rainfall (mm)"), minor_breaks = NULL)+ 
  theme(axis.text= element_text(angle = 90))
ggsave("X:/Spatial Stat/climate_cholera/rainfallVsconfirmed.jpeg", plot= rain_confirmed, width = 6, height = 2.5, units = "in", dpi= 500)



# plot with rainfall anomaly 
  ggplot()+ 
    geom_point(data= merged.dt, aes(x= date, y= log(Confirmed_Cases),size= 1.1, color= "#c09369")) +
    #geom_line(data= merged.dt, aes(x= date, y= lag(rollmean(area.mean, 7, na.pad = T), k= 5), color= year))+ 
    geom_line(data = rai.2015.2019.dt[,mean(rai), by= date], aes(x= date, y= V1, color= year), size= 1.2, color= "turquoise")+ 
    scale_x_date(date_minor_breaks = "1 month", date_breaks = "6 month", date_labels = "%Y-%b")+ 
    scale_y_continuous(name= "log of confirmed cases", sec.axis = sec_axis(~ . * 1, name = "Rainfall anomalies (mm)")) +
  theme(axis.text= element_text(angle = 90))
  


ggplot(tz.rf.16.1.df)+geom_point(aes(x= lon, y= lat, color= rowMeans(tz.rf.16.1.df[,names(tz.rf.16.1.df)[grep("X", names(tz.rf.16.1.df))]])))


tz.raster<- rasterFromXYZ(tz.rf$`2017-06-14`[, c("lon", "lat", "precip")])

